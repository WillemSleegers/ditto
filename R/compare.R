#' Compare strings across multiple similarity metrics
#'
#' Computes several similarity metrics for aligned candidate and reference
#' strings and returns them in a single table. Edit-distance, n-gram, and
#' character-frequency metrics come from [stringdist::stringsim()]; BLEU is
#' computed by [bleu()]. The embedding-based metrics (BERTScore F1 and
#' whole-string cosine) are optional because they require a running
#' `llama.cpp` server.
#'
#' Inputs are compared as given; clean them first with [clean()] if surface
#' differences such as case or punctuation should be ignored.
#'
#' @param candidate,reference Character vectors of equal length.
#' @param bert Whether to include the embedding-based columns (`bertscore_f1`
#'   and `cosine_emb`). Default `FALSE`.
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
#'   `levenshtein`, `jaccard`, `cosine` (character-frequency), `bleu`, and, when
#'   `bert = TRUE`, `bertscore_f1` and `cosine_emb` (whole-string embedding
#'   cosine).
#' @seealso [bleu()], [bertscore()], and [cosine_similarity()] for the
#'   individual metrics, and [clean()] to normalise text before comparison.
#' @export
#' @examples
#' compare_strings(
#'   c("how much do you agree with the statement", "what is your age"),
#'   c("to what extent do you agree with the statement",
#'     "to what extent do you agree with the statement")
#' )
compare_strings <- function(candidate, reference, bert = FALSE,
                            host = "http://localhost:8080", prefix = "",
                            baseline = NULL, pooling = c("mean", "cls")) {
  pooling <- match.arg(pooling)
  out <- tibble::tibble(
    candidate = candidate,
    reference = reference,
    levenshtein = stringdist::stringsim(candidate, reference, method = "lv"),
    jaccard = stringdist::stringsim(candidate, reference, method = "jaccard"),
    cosine = stringdist::stringsim(candidate, reference, method = "cosine"),
    bleu = mapply(bleu, candidate, reference, USE.NAMES = FALSE)
  )

  if (bert) {
    out$bertscore_f1 <- mapply(function(a, b) {
      bertscore(a, b, host = host, prefix = prefix, baseline = baseline)[["f1"]]
    }, candidate, reference, USE.NAMES = FALSE)
    out$cosine_emb <- mapply(function(a, b) {
      cosine_similarity(a, b, host = host, prefix = prefix, pooling = pooling)
    }, candidate, reference, USE.NAMES = FALSE)
  }

  out
}
