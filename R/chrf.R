#' Compute a CHRF score
#'
#' Computes a character n-gram F-score for a candidate string against a
#' reference. Where BLEU counts word n-grams and only precision, CHRF counts
#' character n-grams and combines precision with recall, which makes it more
#' sensitive to morphological variation: an inflected form that shares no
#' words with the reference can still share most of its character n-grams.
#'
#' @details
#' Precision and recall are computed separately for every n-gram order from 1
#' to `n` and averaged, then combined into an F-score that weights recall
#' `beta` times as much as precision. `n` is capped at the length of the
#' shorter string, the same treatment [bleu()] uses for short strings.
#' Character n-grams are extracted from the raw string, so spaces count as
#' ordinary characters and n-grams do not cross word boundaries on their own.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param n Maximum character n-gram order to consider (default 6).
#' @param beta Weight of recall relative to precision (default 2, the
#'   standard CHRF setting).
#' @return A CHRF score between 0 and 1.
#' @seealso [bleu()] for the word n-gram, precision-only counterpart, and
#'   [rouge()] for a recall-oriented word n-gram score.
#' @references
#' Popović, M. (2015). chrF: character n-gram F-score for automatic MT
#' evaluation. *Proceedings of the Tenth Workshop on Statistical Machine
#' Translation*, 392-395. <https://doi.org/10.18653/v1/W15-3049>
#' @export
#' @examples
#' chrf("agreeing", "agreed")
chrf <- function(candidate, reference, n = 6, beta = 2) {
  cand_chars <- stringr::str_split(candidate, "")[[1]]
  ref_chars <- stringr::str_split(reference, "")[[1]]
  n <- min(n, length(cand_chars), length(ref_chars))
  if (n < 1) {
    return(0)
  }

  scores <- vapply(seq_len(n), function(k) {
    cand_ngrams <- get_ngrams(cand_chars, k, sep = "")
    ref_ngrams <- get_ngrams(ref_chars, k, sep = "")
    matches <- ngram_match_count(cand_ngrams, ref_ngrams)
    c(
      precision = if (length(cand_ngrams) == 0) 0 else matches / length(cand_ngrams),
      recall = if (length(ref_ngrams) == 0) 0 else matches / length(ref_ngrams)
    )
  }, numeric(2))

  p <- mean(scores["precision", ])
  r <- mean(scores["recall", ])
  if (p == 0 && r == 0) {
    return(0)
  }
  (1 + beta^2) * p * r / (beta^2 * p + r)
}
