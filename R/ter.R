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
#' exactly is provably NP-complete (Shapira & Storer, 2002). This
#' implementation omits that search and counts only insertions, deletions,
#' and substitutions, which makes it equivalent to word error rate (WER)
#' rather than true TER: a reordering of otherwise-matching words costs more
#' here than in the original metric.
#'
#' Candidate and reference are tokenized by splitting on whitespace and then
#' splitting punctuation from adjacent word characters into its own token
#' (so `"agree."` becomes `"agree"` and `"."`), matching the original TER's
#' convention of treating punctuation as ordinary tokens rather than leaving
#' it attached to words.
#'
#' The word-level edit distance is obtained by encoding each token as a
#' single character and reusing [stringdist::stringdist()]'s Levenshtein
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
#'
#' Shapira, D., & Storer, J. A. (2002). Edit distance with move operations.
#' *Proceedings of the 13th Annual Symposium on Combinatorial Pattern
#' Matching*, 85-98.
#' @export
#' @examples
#' ter("the cat sat on the mat", "a cat was sitting on the mat")
ter <- function(candidate, reference) {
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)
  encoded <- encode_tokens(cand_tokens, ref_tokens)
  dist <- stringdist::stringdist(encoded$candidate, encoded$reference, method = "lv")
  dist / length(ref_tokens)
}
