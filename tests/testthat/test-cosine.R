# Portable test; the live numeric behaviour is covered in test-server.R.

test_that("cosine_similarity rejects an unknown pooling", {
  # match.arg fails before any server request is attempted.
  expect_error(
    cosine_similarity("a", "b", pooling = "bad"),
    "should be one of"
  )
})
