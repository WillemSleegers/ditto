# Shared tokenization, n-gram, and alignment helpers used by bleu(), chrf(),
# rouge(), ter(), and meteor(). Not exported.

# The metrics score one pair of strings at a time. Erroring on longer vectors
# stops a vectorized call from silently scoring only the first pair.
check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L) {
    stop(
      sprintf(
        "`%s` must be a single string, not %s of length %d.",
        arg, class(x)[1], length(x)
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Validates a candidate/reference pair and reports whether either is missing,
# in which case the metric returns NA rather than scoring the string "NA".
check_pair <- function(candidate, reference) {
  check_string(candidate, "candidate")
  check_string(reference, "reference")
  is.na(candidate) || is.na(reference)
}

# Word tokens: splits on whitespace, and further splits punctuation from
# adjacent word characters into its own token, matching TER's convention
# (Snover et al., 2006) that "punctuation tokens are treated as normal words"
# and sacrebleu's `13a` tokenizer. Runs of whitespace and empty strings yield
# no tokens, so surrounding whitespace cannot change a score.
tokenize_words <- function(x) {
  stringr::str_extract_all(x, "[\\p{L}\\p{N}]+|[^\\p{L}\\p{N}\\s]")[[1]]
}

# Character tokens for chrF: whitespace is removed before the string is split
# into characters, as in Popovic (2015) and sacrebleu's default settings.
tokenize_chars <- function(x) {
  x <- stringr::str_remove_all(x, "\\s")
  if (!nzchar(x)) {
    return(character(0))
  }
  stringr::str_split(x, "")[[1]]
}

# All contiguous n-token sequences in a token vector, joined with `sep`
# (" " for word tokens, "" for character tokens).
get_ngrams <- function(tokens, n, sep = " ") {
  if (length(tokens) < n) {
    return(character(0))
  }
  vapply(seq_len(length(tokens) - n + 1), function(i) {
    paste(tokens[i:(i + n - 1)], collapse = sep)
  }, character(1))
}

# Count of n-grams shared between two n-gram vectors, with each reference
# n-gram credited at most as many times as it occurs (clipping).
ngram_match_count <- function(cand_ngrams, ref_ngrams) {
  if (length(cand_ngrams) == 0) {
    return(0)
  }
  sum(vapply(unique(cand_ngrams), function(ng) {
    min(sum(cand_ngrams == ng), sum(ref_ngrams == ng))
  }, numeric(1)))
}

# Map two token vectors onto a shared vocabulary of single Unicode code points,
# so that word-level sequences can be compared with stringdist's
# character-based edit distance functions. The encoded strings contain only
# these symbols -- never the original text -- so the symbols only have to be
# distinct from each other, which `match()` on a shared vocabulary guarantees.
# Numbering starts in the Private Use Area to keep the encoded strings from
# looking like meaningful text when printed.
encode_tokens <- function(cand_tokens, ref_tokens) {
  vocab <- unique(c(cand_tokens, ref_tokens))
  symbols <- intToUtf8(0xE000 + seq_along(vocab) - 1, multiple = TRUE)
  list(
    candidate = paste0(symbols[match(cand_tokens, vocab)], collapse = ""),
    reference = paste0(symbols[match(ref_tokens, vocab)], collapse = "")
  )
}

# Align two token vectors by walking `a` from right to left and matching each
# token to the last not-yet-matched token of `b` with the same value. Returns a
# two-column (i, j) matrix of matched positions, ordered by i.
#
# Every token type contributes min(count in a, count in b) matches, which is
# the largest number any alignment can achieve. Unlike a longest common
# subsequence, matches may cross, so tokens that appear in both vectors but in
# a different order are still matched -- METEOR charges for the reordering
# through its fragmentation penalty rather than by dropping them.
#
# When a token repeats, which occurrence gets matched changes how the matched
# tokens chunk together. Scanning right to left and taking the latest available
# partner reproduces `nltk.translate.meteor_score`, so ditto's scores can be
# checked against it. Both directions are heuristics: neither is guaranteed to
# find the chunk-minimal alignment that METEOR's tie-break calls for.
greedy_alignment <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) {
    return(empty_alignment())
  }

  matched <- logical(length(b))
  pairs <- list()
  for (i in rev(seq_along(a))) {
    j <- which(!matched & b == a[i])
    if (length(j) == 0) {
      next
    }
    last <- j[length(j)]
    matched[last] <- TRUE
    pairs[[length(pairs) + 1]] <- c(i = i, j = last)
  }

  if (length(pairs) == 0) {
    return(empty_alignment())
  }
  pairs <- do.call(rbind, pairs)
  pairs[order(pairs[, "i"]), , drop = FALSE]
}

empty_alignment <- function() {
  matrix(integer(0), ncol = 2, dimnames = list(NULL, c("i", "j")))
}

# METEOR's staged match-then-merge search: words that match exactly are aligned
# first, and only the words neither side has claimed are then matched by stem.
# Stemming everything up front instead would let a stem match steal a word that
# an exact match wants, changing how the matched words chunk together.
staged_alignment <- function(cand, ref, cand_stems, ref_stems) {
  pairs <- greedy_alignment(cand, ref)
  cand_free <- setdiff(seq_along(cand), pairs[, "i"])
  ref_free <- setdiff(seq_along(ref), pairs[, "j"])

  if (length(cand_free) > 0 && length(ref_free) > 0) {
    stem_pairs <- greedy_alignment(cand_stems[cand_free], ref_stems[ref_free])
    if (nrow(stem_pairs) > 0) {
      pairs <- rbind(pairs, cbind(
        i = cand_free[stem_pairs[, "i"]],
        j = ref_free[stem_pairs[, "j"]]
      ))
    }
  }

  pairs[order(pairs[, "i"]), , drop = FALSE]
}

# Number of maximal runs in an (i, j) alignment matrix where both indices
# increase by exactly 1 from one matched pair to the next.
count_chunks <- function(pairs) {
  if (nrow(pairs) == 0) {
    return(0)
  }
  if (nrow(pairs) == 1) {
    return(1)
  }
  contiguous <- vapply(2:nrow(pairs), function(k) {
    pairs[k, "i"] == pairs[k - 1, "i"] + 1 && pairs[k, "j"] == pairs[k - 1, "j"] + 1
  }, logical(1))
  sum(!contiguous) + 1
}
