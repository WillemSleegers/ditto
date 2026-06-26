#' Token-level embeddings from a local llama.cpp server
#'
#' Requests un-pooled, per-token embeddings from a running `llama.cpp` server
#' (`llama-server`) started with `--pooling none`. Under that setting the
#' native `/embeddings` endpoint returns one embedding per input token rather
#' than a single pooled vector, which is what [bertscore()] needs.
#'
#' The model's special tokens (added at the start and end of the sequence)
#' are dropped by default, matching the original BERTScore definition.
#'
#' @details
#' Ollama and LM Studio expose only pooled, OpenAI-style embedding endpoints
#' that return a single vector per string, so they cannot be used here. Start
#' `llama-server` with both `--embedding` and `--pooling none`.
#'
#' Some embedding models require a task prefix, such as `"query: "` for the
#' E5 family or `"search_document: "` for nomic. Pass it as `prefix`: the
#' prefix is prepended before the text is embedded, and its tokens are then
#' counted via the server's `/tokenize` endpoint and dropped, so they do not
#' enter the [bertscore()] matching. With `prefix = ""` (the default) no prefix
#' is added, which suits models such as BGE that do not need one.
#'
#' @param text A single string.
#' @param host Base URL of the llama.cpp server.
#' @param prefix Optional task prefix required by some models (e.g. `"query: "`).
#'   Prepended before embedding and stripped from the returned rows. Default
#'   `""` (no prefix).
#' @param drop_special Whether to drop the first and last token rows, which
#'   correspond to the model's special tokens. Default `TRUE`.
#' @return A numeric matrix with one row per token and one column per
#'   embedding dimension.
#' @seealso [bertscore()], which builds on these embeddings.
#' @export
token_embeddings <- function(text,
                             host = "http://localhost:8080",
                             prefix = "",
                             drop_special = TRUE) {
  resp <- httr2::request(host) |>
    httr2::req_url_path("/embeddings") |>
    httr2::req_body_json(list(content = paste0(prefix, text))) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  # NOTE: the exact response shape depends on the llama.cpp version. With
  # `--pooling none` the native /embeddings endpoint returns the per-token
  # vectors under `embedding`. This extracts a list of numeric vectors and
  # stacks them into a matrix; adjust here if your server's shape differs.
  rows <- parsed[[1]]$embedding
  mat <- do.call(rbind, lapply(rows, function(v) as.numeric(unlist(v))))

  if (drop_special && nrow(mat) > 2) {
    mat <- mat[-c(1, nrow(mat)), , drop = FALSE]
  }

  # Drop the prefix's own tokens, which now sit at the start of the matrix.
  if (nzchar(prefix)) {
    n_prefix <- count_tokens(prefix, host = host)
    if (n_prefix > 0 && nrow(mat) > n_prefix) {
      mat <- mat[-seq_len(n_prefix), , drop = FALSE]
    }
  }
  mat
}

# Number of tokens a string occupies, excluding special tokens, via the
# server's /tokenize endpoint.
count_tokens <- function(text, host = "http://localhost:8080") {
  resp <- httr2::request(host) |>
    httr2::req_url_path("/tokenize") |>
    httr2::req_body_json(list(content = text, add_special = FALSE)) |>
    httr2::req_perform()
  length(httr2::resp_body_json(resp, simplifyVector = FALSE)$tokens)
}

#' Compute a BERTScore
#'
#' Computes a token-level similarity score between a candidate and a
#' reference using contextual embeddings. Each candidate token is greedily
#' matched to its most similar reference token (precision) and each reference
#' token to its most similar candidate token (recall); the F1 is their
#' harmonic mean. This is the vanilla formulation, without inverse
#' document-frequency weighting or baseline rescaling.
#'
#' Requires a running `llama.cpp` server; see [token_embeddings()].
#'
#' @details
#' Precision is the mean, over candidate tokens, of each token's highest
#' cosine similarity to any reference token; recall is the same with the roles
#' of candidate and reference reversed; F1 is their harmonic mean. Because the
#' comparison is between contextual token embeddings, a paraphrase that
#' preserves meaning scores higher than its word overlap alone would suggest.
#'
#' @param candidate A single candidate string.
#' @param reference A single reference string.
#' @param host Base URL of the llama.cpp server.
#' @param prefix Optional task prefix required by some models (e.g. `"query: "`
#'   for the E5 family). Applied to both strings and stripped before matching;
#'   see [token_embeddings()]. Default `""`.
#' @return A named numeric vector with `precision`, `recall`, and `f1`.
#' @seealso [token_embeddings()] for the embeddings this uses, and
#'   [compare_strings()] to report it next to the surface metrics.
#' @references
#' Zhang, T., Kishore, V., Wu, F., Weinberger, K. Q., & Artzi, Y. (2020).
#' BERTScore: Evaluating text generation with BERT. *International Conference
#' on Learning Representations (ICLR)*.
#' <https://doi.org/10.48550/arXiv.1904.09675>
#' @export
#' @examples
#' \dontrun{
#' bertscore("how much do you agree", "to what extent do you agree")
#' }
bertscore <- function(candidate, reference, host = "http://localhost:8080",
                      prefix = "") {
  cand <- token_embeddings(candidate, host = host, prefix = prefix)
  ref <- token_embeddings(reference, host = host, prefix = prefix)

  cand_n <- cand / sqrt(rowSums(cand^2))
  ref_n <- ref / sqrt(rowSums(ref^2))

  sim <- cand_n %*% t(ref_n)
  precision <- mean(apply(sim, 1, max))
  recall <- mean(apply(sim, 2, max))
  f1 <- 2 * precision * recall / (precision + recall)

  c(precision = precision, recall = recall, f1 = f1)
}
