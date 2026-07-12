#' Compute a word error rate
#'
#' Computes the word-level edit distance between a candidate and a reference,
#' divided by the number of words in the reference. Like [ter()], this is an
#' error rate rather than a similarity: 0 is a perfect match, and a score above
#' 1 is possible when the candidate needs more edits than the reference has
#' words.
#'
#' @details
#' Only insertions, deletions, and substitutions are counted. `wer()` is
#' therefore [ter()] without the shift operation: a block of words moved to a
#' different position costs one edit per word here, rather than a single edit.
#' Where TER was designed to forgive reordering, word error rate does not, which
#' is what makes it the standard metric for speech recognition, where the words
#' arrive in order.
#'
#' Candidate and reference are tokenized as in [ter()]: whitespace separates
#' tokens, and punctuation becomes a token of its own. The edit distance is
#' obtained by encoding each token as a single character and reusing
#' [stringdist::stringdist()]'s Levenshtein implementation.
#'
#' A reference with no words scores 0 when the candidate is also empty, and
#' `Inf` otherwise.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @return A word error rate, 0 or greater, or `NA` if either input is `NA`.
#' @seealso [ter()], which additionally counts a moved block of words as one
#'   edit.
#' @examples
#' wer("the cat sat on the mat", "a cat was sitting on the mat")
#'
#' # Reordering costs a substitution per word, where ter() charges one shift.
#' wer("b a c d", "a b c d")
#' ter("b a c d", "a b c d")
#' @export
wer <- function(candidate, reference) {
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)
  if (length(ref_tokens) == 0) {
    return(if (length(cand_tokens) == 0) 0 else Inf)
  }

  encoded <- encode_tokens(cand_tokens, ref_tokens)
  dist <- stringdist::stringdist(encoded$candidate, encoded$reference, method = "lv")
  dist / length(ref_tokens)
}
