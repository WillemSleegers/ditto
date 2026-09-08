# Portable tests for the scoring helpers and rescaling that don't need a
# server. The live numeric behaviour is covered in test-server.R.

test_that("example_sentences has the documented shape", {
  expect_s3_class(example_sentences, "tbl_df")
  expect_named(example_sentences, c("language", "text"))
  expect_true(all(c("en", "nl", "fr") %in% example_sentences$language))
  expect_true(any(example_sentences$language == "en"))
  expect_false(anyDuplicated(example_sentences$text) > 0)
})

test_that("rescale_score maps the baseline to zero and a perfect score to one", {
  score <- c(precision = 0.9, recall = 0.8, f1 = 0.85)

  # A scalar baseline applies to every component.
  resc <- rescale_score(score, 0.5)
  expect_equal(resc, c(precision = 0.8, recall = 0.6, f1 = 0.7))

  # The floor maps to 0 and 1 stays at 1.
  expect_equal(unname(rescale_score(c(f1 = 0.632), 0.632)), 0)
  expect_equal(unname(rescale_score(c(f1 = 1), 0.632)), 1)
})

test_that("rescale_score accepts a per-component named baseline", {
  score <- c(precision = 0.9, recall = 0.8, f1 = 0.85)
  b <- c(precision = 0.6, recall = 0.4, f1 = 0.5)
  resc <- rescale_score(score, b)
  expect_equal(
    resc,
    c(precision = (0.9 - 0.6) / 0.4,
      recall = (0.8 - 0.4) / 0.6,
      f1 = (0.85 - 0.5) / 0.5)
  )
})

test_that("resolve_baseline rejects an unnamed multi-value baseline", {
  expect_error(
    rescale_score(c(precision = 0.9, recall = 0.8, f1 = 0.85), c(0.5, 0.6)),
    "single number or a named vector"
  )
})

test_that("resolve_baseline reports missing components", {
  expect_error(
    rescale_score(c(precision = 0.9, recall = 0.8, f1 = 0.85),
                  c(precision = 0.5, recall = 0.4)),
    "missing components: f1"
  )
})

test_that("score_normed computes greedy-matched precision, recall, and f1", {
  # Identical unit rows -> perfect score.
  a <- normalize_rows(matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE))
  expect_equal(score_normed(a, a), c(precision = 1, recall = 1, f1 = 1))
})

test_that("bertscore_baseline requires at least two distinct texts", {
  expect_error(
    bertscore_baseline(texts = c("only one", "only one")),
    "at least two distinct"
  )
})

test_that("bertscore_baselines validates the required columns", {
  expect_error(
    bertscore_baselines(data.frame(lang = "en", text = "hello")),
    "missing column"
  )
})

test_that("special_span finds a leading and a trailing special token", {
  # bge-m3 wraps the content in <s> and </s>.
  expect_equal(
    special_span(c(0L, 5L, 6L, 7L, 2L), c(5L, 6L, 7L)),
    c(lead = 1L, trail = 1L)
  )
})

test_that("special_span finds a leading special with nothing after it", {
  # granite-embedding-r2 prepends <bos> and appends nothing.
  expect_equal(
    special_span(c(2L, 5L, 6L, 7L), c(5L, 6L, 7L)),
    c(lead = 1L, trail = 0L)
  )
})

test_that("special_span handles a trailing special and no specials at all", {
  expect_equal(
    special_span(c(5L, 6L, 7L, 2L), c(5L, 6L, 7L)),
    c(lead = 0L, trail = 1L)
  )
  expect_equal(
    special_span(c(5L, 6L, 7L), c(5L, 6L, 7L)),
    c(lead = 0L, trail = 0L)
  )
})

test_that("special_span prefers the leading match when the ids are ambiguous", {
  # The content itself starts with the id used as the special token, so both
  # lead = 1 and trail = 1 align. The leading one is the right reading.
  expect_equal(
    special_span(c(2L, 2L, 5L), c(2L, 5L)),
    c(lead = 1L, trail = 0L)
  )
})

test_that("special_span falls back to one at each end when nothing aligns", {
  expect_equal(
    special_span(c(9L, 8L, 7L), c(1L, 2L)),
    c(lead = 1L, trail = 0L)
  )
})

test_that("token_embeddings keeps the last token when no trailing special exists", {
  forget_special_tokens()

  # Four rows: a leading special and three content tokens. Dropping a fixed row
  # at each end would discard the final content token.
  mat <- matrix(as.numeric(1:8), nrow = 4)

  local_mocked_bindings(
    embed_raw = function(text, host = "http://localhost:8080") mat,
    tokenize_ids = function(text, host = "http://localhost:8080",
                            add_special = FALSE) {
      if (add_special) c(2L, 5L, 6L, 7L) else c(5L, 6L, 7L)
    }
  )

  expect_equal(token_embeddings("a b c"), mat[2:4, , drop = FALSE])
  expect_equal(token_embeddings("a b c", drop_special = FALSE), mat)
})

test_that("token_embeddings drops both specials on a model that adds both", {
  forget_special_tokens()

  mat <- matrix(as.numeric(1:10), nrow = 5)

  local_mocked_bindings(
    embed_raw = function(text, host = "http://localhost:8080") mat,
    tokenize_ids = function(text, host = "http://localhost:8080",
                            add_special = FALSE) {
      if (add_special) c(0L, 5L, 6L, 7L, 2L) else c(5L, 6L, 7L)
    }
  )

  expect_equal(token_embeddings("a b c"), mat[2:4, , drop = FALSE])
})

test_that("the special-token layout is derived once per host and then cached", {
  forget_special_tokens()
  mat <- matrix(as.numeric(1:8), nrow = 4)
  calls <- 0L

  local_mocked_bindings(
    embed_raw = function(text, host = "http://localhost:8080") mat,
    tokenize_ids = function(text, host = "http://localhost:8080",
                            add_special = FALSE) {
      calls <<- calls + 1L
      if (add_special) c(2L, 5L, 6L, 7L) else c(5L, 6L, 7L)
    }
  )

  # One /tokenize with special tokens and one without, to derive the layout.
  token_embeddings("a b c")
  expect_equal(calls, 2L)

  # A second string reuses it, so the embeddings call is the only round trip.
  token_embeddings("d e f")
  expect_equal(calls, 2L)

  forget_special_tokens()
  token_embeddings("a b c")
  expect_equal(calls, 4L)
})

test_that("a changed embedding dimension re-derives the cached layout", {
  forget_special_tokens()
  ncols <- 2L
  calls <- 0L

  local_mocked_bindings(
    embed_raw = function(text, host = "http://localhost:8080") {
      matrix(as.numeric(seq_len(4 * ncols)), nrow = 4)
    },
    tokenize_ids = function(text, host = "http://localhost:8080",
                            add_special = FALSE) {
      calls <<- calls + 1L
      if (add_special) c(2L, 5L, 6L, 7L) else c(5L, 6L, 7L)
    }
  )

  token_embeddings("a b c")
  expect_equal(calls, 2L)

  # A different model is now answering at this host. Its embeddings are a
  # different width, which is visible for free in the response.
  ncols <- 3L
  token_embeddings("a b c")
  expect_equal(calls, 4L)

  forget_special_tokens()
})
