test_that("identical strings score 1 and disjoint strings score 0", {
  expect_equal(jaccard("a b c d", "a b c d"), 1)
  expect_equal(jaccard("a b c d", "w x y z"), 0)
  expect_equal(jaccard("a b c d", "a b c d", n = 2), 1)
  expect_equal(jaccard("a b c d", "w x y z", n = 2), 0)
})

test_that("the score is shared words over all distinct words", {
  # Shared: cat, on, the, mat (4). Union adds sat, a, was, sitting (8).
  cand <- "the cat sat on the mat"
  ref <- "a cat was sitting on the mat"
  expect_equal(jaccard(cand, ref), 4 / 8)
  # Shared bigrams: "on the", "the mat" (2) out of 5 + 6 - 2 = 9 distinct.
  expect_equal(jaccard(cand, ref, n = 2), 2 / 9)
})

test_that("repeated words count once", {
  # As sets, both strings are {a, b}.
  expect_equal(jaccard("a a a b", "a b"), 1)
  expect_equal(jaccard("a a", "a b"), 1 / 2)
})

test_that("the score is symmetric", {
  cand <- "how much do you agree with the statement"
  ref <- "to what extent do you agree with the statement"
  expect_equal(jaccard(cand, ref), jaccard(ref, cand))
  expect_equal(jaccard(cand, ref, n = 3), jaccard(ref, cand, n = 3))
})

test_that("word order matters only when n is above 1", {
  expect_equal(jaccard("cat the", "the cat"), 1)
  expect_equal(jaccard("cat the", "the cat", n = 2), 0)
})

test_that("it compares words, unlike the character-set Jaccard", {
  # The two strings use exactly the same characters but no common word.
  expect_equal(stringdist::stringsim("tab", "bat", method = "jaccard"), 1)
  expect_equal(jaccard("tab", "bat"), 0)
})

test_that("punctuation is tokenized separately from the word it follows", {
  # {agree, .} and {agree, ?} share one of three distinct tokens.
  expect_equal(jaccard("agree.", "agree?"), 1 / 3)
})

test_that("surrounding whitespace does not change the score", {
  expect_equal(jaccard(" cat ", "cat"), 1)
  expect_equal(jaccard("a  b", "a b"), 1)
})

test_that("two strings without n-grams score 1 only if their words match", {
  expect_equal(jaccard("", ""), 1)
  expect_equal(jaccard("  ", ""), 1)
  expect_equal(jaccard("", "a b"), 0)
  expect_equal(jaccard("a b", ""), 0)
  # Neither one-word string has a bigram, so their words decide.
  expect_equal(jaccard("cat", "cat", n = 2), 1)
  expect_equal(jaccard("cat", "dog", n = 2), 0)
  # Only one side has a bigram, so nothing is shared.
  expect_equal(jaccard("cat", "the cat", n = 2), 0)
})

test_that("n must be a single positive whole number", {
  expect_error(jaccard("a b", "a b", n = 0), "positive whole number")
  expect_error(jaccard("a b", "a b", n = 1.5), "positive whole number")
  expect_error(jaccard("a b", "a b", n = c(1, 2)), "positive whole number")
  expect_error(jaccard("a b", "a b", n = NA_real_), "positive whole number")
  expect_error(jaccard("a b", "a b", n = "1"), "positive whole number")
})

test_that("a missing input gives a missing score", {
  expect_identical(jaccard(NA_character_, "a b"), NA_real_)
  expect_identical(jaccard("a b", NA_character_, n = 2), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(jaccard(c("a b", "c d"), c("a b", "c d")), "single string")
})
