#' Compare strings across multiple similarity metrics
#'
#' Computes several similarity metrics for aligned candidate and reference
#' strings and returns them in a single table. Edit-distance, n-gram, and
#' character-frequency metrics come from [stringdist::stringsim()]; the
#' translation metrics are computed by [bleu()], [chrf()], [rouge()], [ter()],
#' [wer()], and [meteor()]. The embedding-based metrics (BERTScore F1 and
#' whole-string cosine) are optional because they require a running
#' `llama.cpp` server.
#'
#' Inputs are compared as given; clean them first with [clean()] if surface
#' differences such as case or punctuation should be ignored.
#'
#' Every column is a similarity between 0 and 1 except `ter` and `wer`, which
#' are error rates: they are 0 for a perfect match, higher is worse, and they
#' are not bounded above. They differ only in that `ter` counts a moved block
#' of words as a single edit.
#'
#' @param candidate,reference Character vectors of equal length.
#' @param bert Whether to include the embedding-based columns (`bertscore_f1`
#'   and `cosine_emb`). Default `FALSE`.
#' @param language Language used to stem words for the `meteor` column
#'   (default `"en"`); see [meteor()].
#' @param host Base URL of the llama.cpp server, used when `bert = TRUE`.
#' @param prefix Optional task prefix passed to [bertscore()] and
#'   [cosine_similarity()] for models that require one (e.g. `"query: "`), used
#'   when `bert = TRUE`. Default `""`.
#' @param baseline Optional BERTScore baseline passed to [bertscore()] for
#'   rescaling, used when `bert = TRUE`; see [bertscore_baseline()]. Default
#'   `NULL`.
#' @param pooling Token pooling for the `cosine_emb` column, passed to
#'   [cosine_similarity()]; `"mean"` (default) or `"cls"`. Used when
#'   `bert = TRUE`.
#' @return A [tibble][tibble::tibble] with one row per input pair, containing
#'   the candidate and reference text and a column for each metric:
#'   `levenshtein`, `jaccard`, `cosine` (character-frequency), `bleu`, `chrf`,
#'   `rouge_1`, `rouge_l`, `ter`, `wer`, `meteor`, and, when `bert = TRUE`,
#'   `bertscore_f1` and `cosine_emb` (whole-string embedding cosine).
#' @seealso [bleu()], [chrf()], [rouge()], [ter()], [wer()], [meteor()],
#'   [bertscore()], and [cosine_similarity()] for the individual metrics, and
#'   [clean()] to normalise text before comparison.
#' @examples
#' compare_strings(
#'   c("how much do you agree with the statement", "what is your age"),
#'   c("to what extent do you agree with the statement",
#'     "to what extent do you agree with the statement")
#' )
#' @export
compare_strings <- function(candidate, reference, bert = FALSE, language = "en",
                            host = "http://localhost:8080", prefix = "",
                            baseline = NULL, pooling = c("mean", "cls")) {
  pooling <- match.arg(pooling)
  pairwise <- function(f, ...) {
    mapply(f, candidate, reference, MoreArgs = list(...), USE.NAMES = FALSE)
  }

  out <- tibble::tibble(
    candidate = candidate,
    reference = reference,
    levenshtein = stringdist::stringsim(candidate, reference, method = "lv"),
    jaccard = stringdist::stringsim(candidate, reference, method = "jaccard"),
    cosine = stringdist::stringsim(candidate, reference, method = "cosine"),
    bleu = pairwise(bleu),
    chrf = pairwise(chrf),
    rouge_1 = pairwise(rouge, variant = "1"),
    rouge_l = pairwise(rouge, variant = "l"),
    ter = pairwise(ter),
    wer = pairwise(wer),
    meteor = pairwise(meteor, language = language)
  )

  if (bert) {
    out$bertscore_f1 <- pairwise(function(a, b) {
      bertscore(a, b, host = host, prefix = prefix, baseline = baseline)[["f1"]]
    })
    out$cosine_emb <- pairwise(function(a, b) {
      cosine_similarity(a, b, host = host, prefix = prefix, pooling = pooling)
    })
  }

  out
}
