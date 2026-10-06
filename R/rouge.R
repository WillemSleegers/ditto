#' Compute a ROUGE score
#'
#' Computes a ROUGE score for a candidate string against a reference. ROUGE
#' is the recall-oriented counterpart to BLEU: `variant = "1"` and
#' `variant = "2"` count shared unigrams and bigrams the way BLEU does, but
#' report precision and recall instead of a precision-only, brevity-penalized
#' score. `variant = "l"` instead matches on the longest common subsequence of
#' words, so a candidate and reference that share a long run of words in
#' order score highly even when other words are interspersed.
#'
#' @details
#' Recall is the number of matched n-grams (or, for `variant = "l"`, the
#' longest common subsequence length) divided by the number in the reference;
#' this is ROUGE-N as Lin (2004) defines it. Precision divides by the number in
#' the candidate instead. By default all variants combine the two into an
#' F-score weighting recall `beta` times as much as precision (`beta = 1`, the
#' default, is the harmonic mean); `beta = Inf` gives recall and `beta = 0`
#' gives precision. The longest common subsequence length for `variant = "l"`
#' is obtained by encoding each word as a single character and reusing
#' [stringdist::stringdist()]'s `"lcs"` method, which returns an edit
#' distance under insertions and deletions only; the LCS length is
#' `(length(candidate) + length(reference) - distance) / 2`.
#'
#' Words are tokenized as in [ter()]: whitespace separates tokens, and
#' punctuation becomes a token of its own. A string with no words scores 0,
#' since there are no n-grams to match, and so does any measure whose
#' denominator is 0.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param variant Which ROUGE variant to compute: `"1"` (unigram), `"2"`
#'   (bigram), or `"l"` (longest common subsequence).
#' @param beta Weight of recall relative to precision in the F-score (default
#'   1). A single non-negative number; `Inf` gives recall and `0` gives
#'   precision. Ignored unless `measure = "f"`.
#' @param measure What to return: `"f"` (the F-score, the default),
#'   `"recall"`, or `"precision"`.
#' @return A ROUGE score between 0 and 1, or `NA` if either input is `NA`.
#' @seealso [bleu()] for the precision-only, brevity-penalized counterpart,
#'   and [chrf()] for a character n-gram F-score.
#' @references
#' Lin, C.-Y. (2004). ROUGE: A package for automatic evaluation of summaries.
#' *Text Summarization Branches Out*, 74-81.
#' <https://aclanthology.org/W04-1013/>
#' @examples
#' rouge("the cat sat on the mat", "a cat was sitting on the mat")
#' rouge("the cat sat on the mat", "a cat was sitting on the mat", variant = "l")
#' rouge("the cat sat on the mat", "a cat was sitting on the mat", measure = "recall")
#' @export
rouge <- function(candidate, reference, variant = c("1", "2", "l"), beta = 1,
                  measure = c("f", "recall", "precision")) {
  variant <- match.arg(variant)
  measure <- match.arg(measure)
  if (!is.numeric(beta) || length(beta) != 1L || is.na(beta) || beta < 0) {
    stop("`beta` must be a single non-negative number.", call. = FALSE)
  }
  if (check_pair(candidate, reference)) {
    return(NA_real_)
  }
  cand_tokens <- tokenize_words(candidate)
  ref_tokens <- tokenize_words(reference)

  if (variant == "l") {
    matches <- lcs_length(cand_tokens, ref_tokens)
    cand_count <- length(cand_tokens)
    ref_count <- length(ref_tokens)
  } else {
    n <- as.integer(variant)
    cand_ngrams <- get_ngrams(cand_tokens, n)
    ref_ngrams <- get_ngrams(ref_tokens, n)
    matches <- ngram_match_count(cand_ngrams, ref_ngrams)
    cand_count <- length(cand_ngrams)
    ref_count <- length(ref_ngrams)
  }

  p <- if (cand_count == 0) 0 else matches / cand_count
  r <- if (ref_count == 0) 0 else matches / ref_count
  if (measure == "precision") {
    return(p)
  }
  # beta = Inf would otherwise compute Inf / Inf.
  if (measure == "recall" || is.infinite(beta)) {
    return(r)
  }
  if (p == 0 && r == 0) {
    return(0)
  }
  (1 + beta^2) * p * r / (beta^2 * p + r)
}

# Longest common subsequence length between two token vectors, via
# stringdist's "lcs" edit distance (insertions and deletions only):
# distance = length(a) + length(b) - 2 * lcs_length.
lcs_length <- function(cand_tokens, ref_tokens) {
  if (length(cand_tokens) == 0 || length(ref_tokens) == 0) {
    return(0)
  }
  encoded <- encode_tokens(cand_tokens, ref_tokens)
  dist <- stringdist::stringdist(encoded$candidate, encoded$reference, method = "lcs")
  (length(cand_tokens) + length(ref_tokens) - dist) / 2
}
