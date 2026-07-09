# Shared n-gram and alignment helpers used by bleu(), chrf(), rouge(), ter(),
# and meteor(). Not exported.

# Splits on whitespace, and further splits punctuation from adjacent word
# characters into its own token, matching TER's tokenization convention
# (Snover et al., 2006): "punctuation tokens are treated as normal words."
tokenize_words <- function(x) {
  stringr::str_extract_all(x, "[\\p{L}\\p{N}]+|[^\\p{L}\\p{N}\\s]")[[1]]
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

# Map two token vectors onto a shared vocabulary of single Unicode code
# points (from the Private Use Area, so they cannot collide with real
# text), so that word-level sequences can be compared with stringdist's
# character-based edit distance functions.
encode_tokens <- function(cand_tokens, ref_tokens) {
  vocab <- unique(c(cand_tokens, ref_tokens))
  symbols <- intToUtf8(0xE000 + seq_along(vocab) - 1, multiple = TRUE)
  list(
    candidate = paste0(symbols[match(cand_tokens, vocab)], collapse = ""),
    reference = paste0(symbols[match(ref_tokens, vocab)], collapse = "")
  )
}

# Longest common subsequence alignment between two token vectors, returned
# as a two-column (i, j) matrix of matched positions in increasing order.
# Ties in the backtrace are broken arbitrarily; any tie-breaking yields a
# valid maximum-length alignment, though not necessarily a unique one.
lcs_alignment <- function(a, b) {
  n <- length(a)
  m <- length(b)
  empty <- matrix(integer(0), ncol = 2, dimnames = list(NULL, c("i", "j")))
  if (n == 0 || m == 0) {
    return(empty)
  }

  dp <- matrix(0L, nrow = n + 1, ncol = m + 1)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      dp[i + 1, j + 1] <- if (a[i] == b[j]) {
        dp[i, j] + 1L
      } else {
        max(dp[i, j + 1], dp[i + 1, j])
      }
    }
  }

  i <- n
  j <- m
  pairs <- list()
  while (i > 0 && j > 0) {
    if (a[i] == b[j]) {
      pairs[[length(pairs) + 1]] <- c(i = i, j = j)
      i <- i - 1
      j <- j - 1
    } else if (dp[i, j + 1] >= dp[i + 1, j]) {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }

  if (length(pairs) == 0) {
    return(empty)
  }
  do.call(rbind, rev(pairs))
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
