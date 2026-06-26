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
