#' Compute a BLEU score
#'
#' Computes a sentence-level BLEU score for a candidate string against a
#' reference. BLEU combines clipped n-gram precision with a brevity penalty
#' that discourages candidates shorter than the reference. `max_n` is capped
#' at the length of the shorter string, so a short input is evaluated on the
#' n-gram orders that are actually possible rather than forced to score zero.
#'
#' @details
#' The score is the brevity penalty multiplied by the geometric mean of the
#' clipped n-gram precisions. It is 1 when the candidate and reference are
#' identical, and 0 when any n-gram order has no matching n-grams, which makes
#' BLEU strict on word order. Scores are most meaningful for full sentences;
#' on very short strings there are few n-grams to aggregate, so scores are
#' noisier.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param max_n Maximum n-gram order to consider (default 4).
#' @return A BLEU score between 0 and 1.
#' @seealso [compare_strings()] to compute BLEU alongside other metrics, and
#'   [bertscore()] for a meaning-based comparison.
#' @references
#' Papineni, K., Roukos, S., Ward, T., & Zhu, W.-J. (2002). BLEU: a method for
#' automatic evaluation of machine translation. *Proceedings of the 40th
#' Annual Meeting of the Association for Computational Linguistics*, 311-318.
#' <https://doi.org/10.3115/1073083.1073135>
#' @export
#' @examples
#' bleu(
#'   "how much do you agree with the statement",
#'   "to what extent do you agree with the statement"
#' )
bleu <- function(candidate, reference, max_n = 4) {
  cand_tokens <- stringr::str_split(candidate, "\\s+")[[1]]
  ref_tokens <- stringr::str_split(reference, "\\s+")[[1]]
  max_n <- min(max_n, length(cand_tokens), length(ref_tokens))
  precisions <- purrr::map_dbl(seq_len(max_n), function(n) {
    clipped_precision(cand_tokens, ref_tokens, n)
  })
  if (any(precisions == 0)) {
    return(0)
  }
  bp <- brevity_penalty(cand_tokens, ref_tokens)
  bp * exp(mean(log(precisions)))
}

# Penalize candidates shorter than the reference. Equal or longer candidates
# are not penalized.
brevity_penalty <- function(cand_tokens, ref_tokens) {
  if (length(cand_tokens) >= length(ref_tokens)) {
    1
  } else {
    exp(1 - length(ref_tokens) / length(cand_tokens))
  }
}

# All contiguous n-token sequences in a token vector.
get_ngrams <- function(tokens, n) {
  if (length(tokens) < n) {
    return(character(0))
  }
  purrr::map_chr(seq_len(length(tokens) - n + 1), function(i) {
    paste(tokens[i:(i + n - 1)], collapse = " ")
  })
}

# Fraction of candidate n-grams that appear in the reference, with each
# reference n-gram credited at most as many times as it occurs.
clipped_precision <- function(cand_tokens, ref_tokens, n) {
  cand_ngrams <- get_ngrams(cand_tokens, n)
  ref_ngrams <- get_ngrams(ref_tokens, n)
  if (length(cand_ngrams) == 0) {
    return(0)
  }
  sum(purrr::map_dbl(unique(cand_ngrams), function(ng) {
    min(sum(cand_ngrams == ng), sum(ref_ngrams == ng))
  })) /
    length(cand_ngrams)
}
