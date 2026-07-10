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
#' installation available for only a handful of languages. Scores therefore
#' agree with the reference implementation on text whose matching words are
#' not synonyms, and are lower than it where synonyms would have matched.
#'
#' Alignment follows the original's staged search. Words that match exactly
#' are aligned first; the words neither side has claimed are then matched the
#' same way by stem. Matches may cross, so words that appear in both strings
#' in a different order are still matched, and the reordering is charged for
#' by the fragmentation penalty below rather than by dropping the words from
#' the alignment.
#'
#' When a word repeats, which of its occurrences is matched changes how the
#' matched words chunk together. Each stage scans the candidate from right to
#' left and takes the latest unclaimed reference word, which reproduces
#' `nltk.translate.meteor_score`; ditto's scores agree with it to within
#' floating-point error on text with no synonym matches. This is a heuristic,
#' not the chunk-minimal alignment the original tie-break calls for, and a
#' left-to-right scan would sometimes find fewer chunks.
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
#' Stemming is language-specific. `language` is passed to
#' [SnowballC::wordStem()]; call [SnowballC::getStemLanguages()] for the
#' languages it supports.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param language Language used to stem words before matching (default
#'   `"en"`), passed to [SnowballC::wordStem()].
#' @param recall_weight How much more heavily recall counts than precision
#'   in the F-score (default 9).
#' @param gamma Fragmentation penalty coefficient (default 0.5).
#' @param frag_power Fragmentation penalty exponent (default 3).
#' @return A METEOR score between 0 and 1, or `NA` if either input is `NA`.
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
#'
#' # Reordered words are matched, then charged a fragmentation penalty.
#' meteor("the cat sat", "sat the cat")
#'
#' # Stemming follows the language of the text.
#' meteor("de katten liepen", "de kat liep", language = "dutch")
meteor <- function(candidate, reference, language = "en", recall_weight = 9,
                   gamma = 0.5, frag_power = 3) {
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(stringr::str_to_lower(candidate))
  ref_tokens <- tokenize_words(stringr::str_to_lower(reference))
  if (length(cand_tokens) == 0 || length(ref_tokens) == 0) {
    return(0)
  }

  cand_stems <- SnowballC::wordStem(cand_tokens, language = language)
  ref_stems <- SnowballC::wordStem(ref_tokens, language = language)

  pairs <- staged_alignment(cand_tokens, ref_tokens, cand_stems, ref_stems)
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
