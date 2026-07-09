#' Compute a TER score
#'
#' Computes Translation Edit Rate: the word-level edit distance between a
#' candidate and a reference, divided by the number of words in the
#' reference. Unlike the similarity scores elsewhere in this package, TER is
#' not bounded between 0 and 1; a score above 1 is possible when the
#' candidate requires more edits than the reference has words.
#'
#' @details
#' The original TER also treats moving a contiguous block of words to a
#' different position as a single edit (a "shift"), found by a heuristic
#' search over candidate shifts, because finding the optimal set of shifts
#' exactly is computationally intractable. This implementation omits that
#' search and counts only insertions, deletions, and substitutions, which
#' makes it equivalent to word error rate (WER) rather than true TER: a
#' reordering of otherwise-matching words costs more here than in the
#' original metric.
#'
#' The word-level edit distance is obtained by encoding each word as a single
#' character and reusing [stringdist::stringdist()]'s Levenshtein
#' implementation on the resulting strings.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @return A TER score, 0 or greater.
#' @seealso [bleu()] and [rouge()] for bounded, precision/recall-based
#'   translation comparisons.
#' @references
#' Snover, M., Dorr, B., Schwartz, R., Micciulla, L., & Makhoul, J. (2006). A
#' study of translation edit rate with targeted human annotation.
#' *Proceedings of the 7th Conference of the Association for Machine
#' Translation in the Americas*, 223-231.
#' @export
#' @examples
#' ter("the cat sat on the mat", "a cat was sitting on the mat")
ter <- function(candidate, reference) {
  cand_tokens <- stringr::str_split(candidate, "\\s+")[[1]]
  ref_tokens <- stringr::str_split(reference, "\\s+")[[1]]
  encoded <- encode_tokens(cand_tokens, ref_tokens)
  dist <- stringdist::stringdist(encoded$candidate, encoded$reference, method = "lv")
  dist / length(ref_tokens)
}
