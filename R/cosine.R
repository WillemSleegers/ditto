#' Whole-string cosine similarity from embeddings
#'
#' Embeds each string into a single vector and returns the cosine similarity
#' between them. This is the pooled, whole-string counterpart to [bertscore()]:
#' where BERTScore matches individual tokens, this collapses each string to one
#' vector first, which is faster and is the classic sentence-embedding
#' similarity. Like [bertscore()] it requires a running `llama.cpp` server; see
#' [token_embeddings()].
#'
#' @details
#' The server runs with `--pooling none` and returns per-token vectors, so the
#' pooling into a single vector happens here. `pooling = "mean"` averages the
#' token vectors (the pooling used by sentence-transformers models such as
#' all-MiniLM and the E5 family); `pooling = "cls"` takes the leading special
#' token's vector (the pooling used by BGE models such as bge-m3). Choose the
#' one your model was trained with.
#'
#' Not to be confused with the character-frequency `cosine` column from
#' [stringdist::stringsim()] reported by [compare_strings()]: that compares
#' character q-gram counts, while this compares meaning.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param host Base URL of the llama.cpp server.
#' @param prefix Optional task prefix required by some models (e.g. `"query: "`
#'   for the E5 family); see [token_embeddings()]. Default `""`.
#' @param pooling How to pool token vectors into one: `"mean"` (default) or
#'   `"cls"`.
#' @return A single cosine similarity, between -1 and 1.
#' @seealso [bertscore()] for the token-level counterpart and
#'   [compare_strings()] to report this next to the other metrics.
#' @examples
#' \dontrun{
#' cosine_similarity("how much do you agree", "to what extent do you agree")
#' # bge-m3 uses CLS pooling:
#' cosine_similarity("how much do you agree", "to what extent do you agree",
#'                   pooling = "cls")
#' }
#' @export
cosine_similarity <- function(candidate, reference,
                              host = "http://localhost:8080",
                              prefix = "",
                              pooling = c("mean", "cls")) {
  pooling <- match.arg(pooling)
  v1 <- pooled_vector(candidate, host = host, prefix = prefix, pooling = pooling)
  v2 <- pooled_vector(reference, host = host, prefix = prefix, pooling = pooling)
  sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
}

# Collapse a string to a single embedding vector using the chosen pooling.
pooled_vector <- function(text, host, prefix, pooling) {
  if (pooling == "cls") {
    # The CLS vector is the leading special token of the full sequence; it
    # already summarises the whole string, so no trimming is needed.
    embed_raw(paste0(prefix, text), host = host)[1, ]
  } else {
    colMeans(token_embeddings(text, host = host, prefix = prefix))
  }
}
