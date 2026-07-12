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
  mat <- embed_raw(paste0(prefix, text), host = host)

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

# Raw per-token embedding matrix for `text`, with no trimming. The exact
# response shape depends on the llama.cpp version. With `--pooling none` the
# native /embeddings endpoint returns the per-token vectors under `embedding`;
# this stacks them into a matrix. Adjust here if your server's shape differs.
embed_raw <- function(text, host = "http://localhost:8080") {
  resp <- httr2::request(host) |>
    httr2::req_url_path("/embeddings") |>
    httr2::req_body_json(list(content = text)) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  rows <- parsed[[1]]$embedding
  do.call(rbind, lapply(rows, function(v) as.numeric(unlist(v))))
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
#' @param baseline Optional baseline for rescaling, as returned by
#'   [bertscore_baseline()] (a named `precision`/`recall`/`f1` vector) or a
#'   single number applied to all three. Each score `x` is rescaled to
#'   `(x - b) / (1 - b)`, which maps the unrelated-text floor to 0 and widens
#'   the usable range. Default `NULL` (raw scores).
#' @return A named numeric vector with `precision`, `recall`, and `f1`.
#' @seealso [token_embeddings()] for the embeddings this uses,
#'   [bertscore_baseline()] to estimate a `baseline`, and [compare_strings()]
#'   to report it next to the surface metrics.
#' @references
#' Zhang, T., Kishore, V., Wu, F., Weinberger, K. Q., & Artzi, Y. (2020).
#' BERTScore: Evaluating text generation with BERT. *International Conference
#' on Learning Representations (ICLR)*.
#' <https://doi.org/10.48550/arXiv.1904.09675>
#' @examples
#' \dontrun{
#' bertscore("how much do you agree", "to what extent do you agree")
#' }
#' @export
bertscore <- function(candidate, reference, host = "http://localhost:8080",
                      prefix = "", baseline = NULL) {
  cand <- normalize_rows(token_embeddings(candidate, host = host, prefix = prefix))
  ref <- normalize_rows(token_embeddings(reference, host = host, prefix = prefix))

  score <- score_normed(cand, ref)
  if (!is.null(baseline)) {
    score <- rescale_score(score, baseline)
  }
  score
}

#' Estimate a BERTScore baseline from unrelated text
#'
#' Estimates the BERTScore an embedding model assigns to unrelated text, so
#' scores can be rescaled to span a useful range (see the `baseline` argument
#' of [bertscore()]). Some models, such as bge-m3, place even unrelated
#' sentences at a high cosine similarity; subtracting this floor restores the
#' separation between matched and unmatched pairs.
#'
#' @details
#' The baseline is the mean score over `n` random, and therefore unrelated,
#' pairs drawn from `texts`. It is specific to the model *and* the language, so
#' estimate it from text representative of your use. Each sentence is embedded
#' once and reused across pairs, so a few hundred pairs are cheap to score.
#'
#' @param texts Character vector of distinct, mutually unrelated sentences.
#'   Defaults to the English rows of [example_sentences]. For a multilingual
#'   model, pass text in the language you will score.
#' @param host Base URL of the llama.cpp server.
#' @param prefix Optional task prefix; see [token_embeddings()]. Must match the
#'   prefix used when scoring. Default `""`.
#' @param n Number of random pairs to average over. Default `400`.
#' @param seed Optional integer seed for reproducible pairing. The global random
#'   state is restored afterwards.
#' @return A named numeric vector with the baseline `precision`, `recall`, and
#'   `f1`, suitable to pass as `baseline` to [bertscore()].
#' @seealso [bertscore()], which consumes the result.
#' @examples
#' \dontrun{
#' b <- bertscore_baseline()
#' bertscore("how much do you agree", "to what extent do you agree",
#'           baseline = b)
#' }
#' @export
bertscore_baseline <- function(texts = NULL,
                               host = "http://localhost:8080",
                               prefix = "",
                               n = 400L,
                               seed = NULL) {
  if (is.null(texts)) {
    texts <- example_sentences$text[example_sentences$language == "en"]
  }
  texts <- unique(texts)
  if (length(texts) < 2) {
    stop("`texts` must contain at least two distinct strings.", call. = FALSE)
  }

  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }

  embs <- lapply(texts, function(t) {
    normalize_rows(token_embeddings(t, host = host, prefix = prefix))
  })

  m <- length(texts)
  scores <- vapply(seq_len(n), function(k) {
    ij <- sample.int(m, 2)
    score_normed(embs[[ij[1]]], embs[[ij[2]]])
  }, numeric(3))

  rowMeans(scores)
}

#' Estimate a BERTScore baseline per language
#'
#' Convenience wrapper that runs [bertscore_baseline()] once for each language
#' in a labelled corpus, since the baseline is language-specific. Useful for
#' multilingual models such as bge-m3, where you score text in several languages
#' and want each rescaled against its own floor.
#'
#' @param data A data frame with a language column and a text column. Defaults
#'   to [example_sentences].
#' @param host Base URL of the llama.cpp server.
#' @param prefix Optional task prefix; see [token_embeddings()]. Default `""`.
#' @param n Number of random pairs to average over per language. Default `400`.
#' @param seed Optional integer seed for reproducible pairing, applied to each
#'   language. The global random state is restored afterwards.
#' @param language_col,text_col Column names holding the language label and the
#'   text. Default `"language"` and `"text"`.
#' @return A named list with one baseline per language, each a named numeric
#'   vector as returned by [bertscore_baseline()]. Index it by language code to
#'   get a `baseline` for [bertscore()], e.g. `baselines[["nl"]]`.
#' @seealso [bertscore_baseline()] for a single baseline and [bertscore()],
#'   which consumes one.
#' @examples
#' \dontrun{
#' baselines <- bertscore_baselines()
#' bertscore("in hoeverre bent u het eens", "hoeveel bent u het eens",
#'           baseline = baselines[["nl"]])
#' }
#' @export
bertscore_baselines <- function(data = example_sentences,
                                host = "http://localhost:8080",
                                prefix = "",
                                n = 400L,
                                seed = NULL,
                                language_col = "language",
                                text_col = "text") {
  missing_cols <- setdiff(c(language_col, text_col), names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing column(s): ", paste(missing_cols, collapse = ", "),
         ".", call. = FALSE)
  }

  languages <- unique(data[[language_col]])
  baselines <- lapply(languages, function(lg) {
    texts <- data[[text_col]][data[[language_col]] == lg]
    bertscore_baseline(texts, host = host, prefix = prefix, n = n, seed = seed)
  })
  names(baselines) <- languages
  baselines
}

# Row-normalize an embedding matrix so rows are unit vectors (cosine ready).
normalize_rows <- function(m) {
  m / sqrt(rowSums(m^2))
}

# Greedy-matched precision/recall/f1 from two row-normalized matrices.
score_normed <- function(cand, ref) {
  sim <- cand %*% t(ref)
  precision <- mean(apply(sim, 1, max))
  recall <- mean(apply(sim, 2, max))
  f1 <- 2 * precision * recall / (precision + recall)
  c(precision = precision, recall = recall, f1 = f1)
}

# Apply (x - b) / (1 - b) per component.
rescale_score <- function(score, baseline) {
  b <- resolve_baseline(baseline, names(score))
  (score - b) / (1 - b)
}

# Coerce `baseline` to a vector aligned with `nms` (precision/recall/f1).
resolve_baseline <- function(baseline, nms) {
  if (length(baseline) == 1 && is.null(names(baseline))) {
    out <- rep(baseline, length(nms))
    names(out) <- nms
    return(out)
  }
  if (is.null(names(baseline))) {
    stop("`baseline` must be a single number or a named vector with ",
         "precision, recall, and f1.", call. = FALSE)
  }
  missing <- setdiff(nms, names(baseline))
  if (length(missing) > 0) {
    stop("`baseline` is missing components: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  baseline[nms]
}
