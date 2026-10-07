#' Compute a word-overlap Jaccard similarity
#'
#' Computes the Jaccard similarity between the sets of words (or word n-grams)
#' in a candidate string and a reference: the number of distinct n-grams the
#' two share divided by the number of distinct n-grams in either. Unlike
#' [rouge()], it ignores how often a word occurs, and it treats both strings
#' symmetrically, so swapping `candidate` and `reference` gives the same score.
#'
#' @details
#' The score is \eqn{|A \cap B| / |A \cup B|}, where \eqn{A} and \eqn{B} are
#' the sets of distinct word n-grams in the candidate and reference. With
#' `n = 1` (the default) word order does not matter; larger `n` makes the score
#' sensitive to order, since reordering words breaks the n-grams they span.
#'
#' This is word-level overlap. The `jaccard_char` column of
#' [compare_strings()] is the character-level counterpart, the Jaccard
#' similarity of the sets of characters, from [stringdist::stringsim()].
#'
#' Words are tokenized as in [ter()]: whitespace separates tokens, and
#' punctuation becomes a token of its own. When neither string has any
#' n-grams -- both are empty, or both are shorter than `n` words -- the score
#' is 1 if their words are identical and 0 otherwise, so two empty strings
#' score 1 but `"cat"` and `"dog"` do not with `n = 2`.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param n Length of the word n-grams to compare (default 1). A single
#'   positive whole number.
#' @return A Jaccard similarity between 0 and 1, or `NA` if either input is
#'   `NA`.
#' @seealso [rouge()] for a count-based unigram and bigram overlap that
#'   distinguishes recall from precision.
#' @references
#' Jaccard, P. (1912). The distribution of the flora in the alpine zone.
#' *New Phytologist*, 11(2), 37-50.
#' <https://doi.org/10.1111/j.1469-8137.1912.tb05611.x>
#' @examples
#' jaccard("the cat sat on the mat", "a cat was sitting on the mat")
#' jaccard("the cat sat on the mat", "a cat was sitting on the mat", n = 2)
#' # Word order is ignored with n = 1, but not with n = 2.
#' jaccard("the cat", "cat the")
#' jaccard("the cat", "cat the", n = 2)
#' @export
jaccard <- function(candidate, reference, n = 1) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n %% 1 != 0) {
    stop("`n` must be a single positive whole number.", call. = FALSE)
  }
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)
  cand_ngrams <- unique(get_ngrams(cand_tokens, n))
  ref_ngrams <- unique(get_ngrams(ref_tokens, n))

  union_size <- length(union(cand_ngrams, ref_ngrams))
  if (union_size == 0) {
    return(as.numeric(identical(cand_tokens, ref_tokens)))
  }
  length(intersect(cand_ngrams, ref_ngrams)) / union_size
}
