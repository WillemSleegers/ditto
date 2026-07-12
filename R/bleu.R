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
#' Words are tokenized as in [ter()]: whitespace separates tokens, and
#' punctuation becomes a token of its own. A string with no words scores 0.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param max_n Maximum n-gram order to consider (default 4).
#' @return A BLEU score between 0 and 1, or `NA` if either input is `NA`.
#' @seealso [compare_strings()] to compute BLEU alongside other metrics, and
#'   [bertscore()] for a meaning-based comparison.
#' @references
#' Papineni, K., Roukos, S., Ward, T., & Zhu, W.-J. (2002). BLEU: a method for
#' automatic evaluation of machine translation. *Proceedings of the 40th
#' Annual Meeting of the Association for Computational Linguistics*, 311-318.
#' <https://doi.org/10.3115/1073083.1073135>
#' @examples
#' bleu(
#'   "how much do you agree with the statement",
#'   "to what extent do you agree with the statement"
#' )
#' @export
bleu <- function(candidate, reference, max_n = 4) {
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)
  max_n <- min(max_n, length(cand_tokens), length(ref_tokens))
  if (max_n < 1) {
    return(0)
  }
  precisions <- vapply(seq_len(max_n), function(n) {
    clipped_precision(cand_tokens, ref_tokens, n)
  }, numeric(1))
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

# Fraction of candidate n-grams that appear in the reference, with each
# reference n-gram credited at most as many times as it occurs.
clipped_precision <- function(cand_tokens, ref_tokens, n) {
  cand_ngrams <- get_ngrams(cand_tokens, n)
  ref_ngrams <- get_ngrams(ref_tokens, n)
  if (length(cand_ngrams) == 0) {
    return(0)
  }
  ngram_match_count(cand_ngrams, ref_ngrams) / length(cand_ngrams)
}
