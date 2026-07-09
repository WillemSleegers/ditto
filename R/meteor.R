#' Compute a METEOR score
#'
#' Computes a METEOR score for a candidate string against a reference.
#' METEOR improves on BLEU in two ways: it combines precision and recall
#' instead of measuring precision alone, and it matches words by stem as
#' well as by exact form, so "agreeing" and "agreed" count as a match.
#'
#' @details
#' This implementation covers exact and stem matching only; it does not
#' include METEOR's synonym-matching stage, which requires a WordNet
#' installation available for only a handful of languages. Matching also
#' departs from the original algorithm's match-then-merge search: instead of
#' preferring exact matches and filling gaps with stem matches, words are
#' stemmed first and then aligned in one pass by longest common subsequence,
#' which is simpler but can occasionally choose a different alignment than
#' the original search would.
#'
#' Precision and recall over the aligned words are combined into an F-score
#' that weights recall `recall_weight` times as much as precision (default
#' 9, as in the original metric, giving `f_mean = 10PR / (R + 9P)`). A
#' fragmentation penalty then discounts that score based on how many
#' contiguous chunks the matched words fall into: `penalty = gamma *
#' (chunks / matches) ^ frag_power`, so a candidate that matches the same
#' words but scattered across the sentence scores lower than one that
#' matches them in one contiguous run.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param recall_weight How much more heavily recall counts than precision
#'   in the F-score (default 9).
#' @param gamma Fragmentation penalty coefficient (default 0.5).
#' @param frag_power Fragmentation penalty exponent (default 3).
#' @return A METEOR score between 0 and 1.
#' @seealso [bleu()] for the exact-match, precision-only counterpart.
#' @references
#' Banerjee, S., & Lavie, A. (2005). METEOR: An automatic metric for MT
#' evaluation with improved correlation with human judgments. *Proceedings of
#' the ACL Workshop on Intrinsic and Extrinsic Evaluation Measures for
#' Machine Translation and/or Summarization*, 65-72.
#' <https://aclanthology.org/W05-0909/>
#' @export
#' @examples
#' meteor("the cats are agreeing", "the cat agreed")
meteor <- function(candidate, reference, recall_weight = 9, gamma = 0.5,
                   frag_power = 3) {
  cand_tokens <- stringr::str_split(stringr::str_to_lower(candidate), "\\s+")[[1]]
  ref_tokens <- stringr::str_split(stringr::str_to_lower(reference), "\\s+")[[1]]

  cand_stems <- SnowballC::wordStem(cand_tokens, language = "en")
  ref_stems <- SnowballC::wordStem(ref_tokens, language = "en")

  pairs <- lcs_alignment(cand_stems, ref_stems)
  matches <- nrow(pairs)
  if (matches == 0) {
    return(0)
  }

  p <- matches / length(cand_tokens)
  r <- matches / length(ref_tokens)
  f_mean <- (1 + recall_weight) * p * r / (r + recall_weight * p)

  chunks <- count_chunks(pairs)
  penalty <- gamma * (chunks / matches)^frag_power

  f_mean * (1 - penalty)
}
