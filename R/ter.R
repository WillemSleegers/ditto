#' Compute a TER score
#'
#' Computes Translation Edit Rate: the number of edits needed to turn the
#' candidate into the reference, divided by the number of words in the
#' reference. Unlike the similarity scores elsewhere in this package, TER is an
#' error rate: 0 is a perfect match, lower is better, and a score above 1 is
#' possible when the candidate needs more edits than the reference has words.
#'
#' @details
#' The edits counted are insertions, deletions, substitutions, and *shifts*: a
#' contiguous block of words moved to a different position costs a single edit,
#' no matter how far it moves or how many words it contains. Shifts are what
#' separate TER from word error rate, which charges a reordering as a
#' substitution per word; see [wer()] for that metric.
#'
#' Finding the cheapest set of shifts exactly is NP-complete (Shapira & Storer,
#' 2002), so TER's original implementation, TERCOM, uses a greedy search:
#' repeatedly apply the single shift that most reduces the edit distance, until
#' none reduces it further. `ter()` reproduces that search, following
#' `sacrebleu`'s reimplementation of TERCOM closely enough to return the same
#' score; see `vignette("metrics")`.
#'
#' Candidate and reference are tokenized by splitting on whitespace and then
#' splitting punctuation from adjacent word characters into its own token (so
#' `"agree."` becomes `"agree"` and `"."`), matching TER's convention of
#' treating punctuation as ordinary tokens. Unlike `sacrebleu`, which
#' lowercases by default, `ter()` compares the text as given; use [clean()]
#' first to ignore case.
#'
#' Because TER divides by the reference length, a reference with no words is a
#' special case: it scores 0 when the candidate is also empty, and `Inf`
#' otherwise.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @return A TER score, 0 or greater, or `NA` if either input is `NA`.
#' @seealso [wer()] for the same edit rate without the shift operation, and
#'   [bleu()] and [rouge()] for bounded, precision/recall-based comparisons.
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
#'
#' # A reordering is one shift, not two substitutions.
#' ter("b a c d", "a b c d")
#' wer("b a c d", "a b c d")
ter <- function(candidate, reference) {
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)
  if (length(ref_tokens) == 0) {
    return(if (length(cand_tokens) == 0) 0 else Inf)
  }

  result <- translation_edit_rate(cand_tokens, ref_tokens)
  result$edits / result$length
}
