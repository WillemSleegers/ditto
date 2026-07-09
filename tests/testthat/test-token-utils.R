test_that("tokenize_words splits punctuation from adjacent words", {
  expect_equal(tokenize_words("do you agree?"), c("do", "you", "agree", "?"))
  expect_equal(tokenize_words("a, b"), c("a", ",", "b"))
})

test_that("tokenize_words leaves plain words untouched", {
  expect_equal(tokenize_words("a b c d"), c("a", "b", "c", "d"))
})

test_that("get_ngrams returns overlapping windows joined by sep", {
  expect_equal(get_ngrams(c("a", "b", "c"), 2), c("a b", "b c"))
  expect_equal(get_ngrams(c("a", "b", "c"), 2, sep = ""), c("ab", "bc"))
})

test_that("get_ngrams returns an empty vector when n exceeds the token count", {
  expect_equal(get_ngrams(c("a", "b"), 3), character(0))
})

test_that("ngram_match_count clips repeated matches to the reference count", {
  expect_equal(ngram_match_count(c("a", "a", "a"), c("a", "a")), 2)
})

test_that("encode_tokens maps equal tokens to equal symbols", {
  encoded <- encode_tokens(c("a", "b", "a"), c("b", "a"))
  expect_equal(nchar(encoded$candidate), 3)
  expect_equal(nchar(encoded$reference), 2)
  chars <- strsplit(paste0(encoded$candidate, encoded$reference), "")[[1]]
  expect_equal(length(unique(chars)), 2)
})

test_that("lcs_alignment finds the longest order-preserving match", {
  pairs <- lcs_alignment(c("a", "x", "b", "c"), c("a", "b", "y", "c"))
  expect_equal(nrow(pairs), 3)
  expect_true(all(diff(pairs[, "i"]) > 0))
  expect_true(all(diff(pairs[, "j"]) > 0))
})

test_that("lcs_alignment returns an empty matrix for disjoint sequences", {
  pairs <- lcs_alignment(c("a", "b"), c("x", "y"))
  expect_equal(nrow(pairs), 0)
})

test_that("count_chunks counts contiguous runs, not total matches", {
  contiguous <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 2, dimnames = list(NULL, c("i", "j")))
  expect_equal(count_chunks(contiguous), 1)

  scattered <- matrix(c(1, 3, 5, 1, 3, 5), ncol = 2, dimnames = list(NULL, c("i", "j")))
  expect_equal(count_chunks(scattered), 3)
})
