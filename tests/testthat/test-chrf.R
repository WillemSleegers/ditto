test_that("identical strings score 1", {
  expect_equal(chrf("a b c d", "a b c d"), 1)
})

test_that("completely different strings score 0", {
  expect_equal(chrf("abcd", "wxyz"), 0)
})

test_that("a partial match lands strictly between 0 and 1", {
  s <- chrf("agreeing", "agreed")
  expect_gt(s, 0)
  expect_lt(s, 1)
})

test_that("an inflected form scores higher on chrf than on bleu", {
  # chrf shares character n-grams across "agreeing"/"agreed"; bleu requires
  # exact word matches and shares none.
  expect_gt(chrf("agreeing", "agreed"), bleu("agreeing", "agreed"))
})

test_that("n is capped at the shorter string's length", {
  expect_gt(chrf("hi", "hi"), 0)
})

test_that("scores match the sacrebleu reference implementation", {
  # sacrebleu CHRF(char_order = 6, word_order = 0, beta = 2,
  # whitespace = FALSE, eps_smoothing = FALSE), rescaled from 0-100 to 0-1.
  # See dev/validation/text-metrics-reference.py.
  expect_equal(chrf("agreeing", "agreed"), 0.5366165, tolerance = 1e-6)
  expect_equal(chrf("abc", "abcxyz"), 0.4372624, tolerance = 1e-6)
  expect_equal(chrf("cats", "cat"), 0.8984375, tolerance = 1e-6)
  expect_equal(
    chrf("the cat sat on the mat", "a cat was sitting on the mat"),
    0.3722543,
    tolerance = 1e-6
  )
})

test_that("capping n agrees with sacrebleu's effective order on short strings", {
  # Orders too high to produce an n-gram are dropped from the average rather
  # than scored 0, so these do not collapse toward 0.
  expect_equal(chrf("abcdefgh", "ab"), 0.55, tolerance = 1e-6)
  expect_equal(chrf("ab", "abcdefgh"), 0.2340426, tolerance = 1e-6)
  expect_equal(chrf("a", "abcdef"), 0.2, tolerance = 1e-6)
  expect_equal(chrf("abcdef", "a"), 0.5, tolerance = 1e-6)
  expect_equal(chrf("q", "q"), 1)
  expect_equal(chrf("xy", "ab"), 0)
})

test_that("whitespace is removed before character n-grams are extracted", {
  # As in Popovic (2015) and sacrebleu, spaces are not characters, so an
  # n-gram may span a word boundary and spacing cannot change the score.
  expect_equal(chrf("ab cd", "abcd"), 1)
  expect_equal(chrf("a b", "ab"), 1)
  expect_equal(chrf("  the cat  ", "the cat"), 1)
  expect_equal(chrf("the  cat", "the cat"), 1)
})

test_that("beta weights recall relative to precision", {
  # The candidate contains the reference as a prefix: recall is 1 while
  # precision is below it, so a recall-heavy beta scores higher.
  expect_gt(chrf("agreed today", "agreed", beta = 2), chrf("agreed today", "agreed", beta = 1))
})

test_that("a string with no characters scores 0", {
  expect_equal(chrf("", ""), 0)
  expect_equal(chrf("   ", "abc"), 0)
  expect_equal(chrf("abc", ""), 0)
})

test_that("a missing input gives a missing score", {
  expect_identical(chrf(NA_character_, "abc"), NA_real_)
  expect_identical(chrf("abc", NA_character_), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(chrf(c("ab", "cd"), c("ab", "cd")), "single string")
})

test_that("beta = Inf gives recall and beta = 0 gives precision", {
  # Every character n-gram of "agreed" appears in "agreed today", so recall is
  # 1 one way round and precision is 1 the other.
  expect_equal(chrf("agreed today", "agreed", beta = Inf), 1)
  expect_equal(chrf("agreed", "agreed today", beta = 0), 1)
  expect_lt(chrf("agreed today", "agreed", beta = 0), 1)
  expect_equal(chrf("abc", "xyz", beta = Inf), 0)
  expect_equal(chrf("", "abc", beta = Inf), 0)
  expect_identical(chrf(NA_character_, "abc", beta = Inf), NA_real_)
})

test_that("beta must be a single non-negative number", {
  expect_error(chrf("a b", "a b", beta = -1), "non-negative")
  expect_error(chrf("a b", "a b", beta = c(1, 2)), "non-negative")
  expect_error(chrf("a b", "a b", beta = NA_real_), "non-negative")
  expect_error(chrf("a b", "a b", beta = "1"), "non-negative")
})

test_that("measure returns the averaged recall or precision", {
  # Every character n-gram of "agreed" appears in "agreed today".
  expect_equal(chrf("agreed today", "agreed", measure = "recall"), 1)
  expect_equal(chrf("agreed", "agreed today", measure = "precision"), 1)
  cand <- "agreeing"
  ref <- "agreed"
  expect_equal(
    chrf(cand, ref, measure = "recall"),
    chrf(cand, ref, beta = Inf)
  )
  expect_equal(
    chrf(cand, ref, measure = "precision"),
    chrf(cand, ref, beta = 0)
  )
  p <- chrf(cand, ref, measure = "precision")
  r <- chrf(cand, ref, measure = "recall")
  expect_equal(chrf(cand, ref), 5 * p * r / (4 * p + r))
})

test_that("beta is ignored unless measure is the F-score", {
  expect_equal(
    chrf("agreed today", "agreed", measure = "precision", beta = Inf),
    chrf("agreed today", "agreed", beta = 0)
  )
  expect_equal(chrf("agreed today", "agreed", measure = "recall", beta = 0), 1)
})

test_that("recall and precision keep the NA and empty-string cases", {
  for (measure in c("recall", "precision")) {
    expect_identical(chrf(NA_character_, "abc", measure = measure), NA_real_)
    expect_equal(chrf("", "abc", measure = measure), 0)
    expect_equal(chrf("abc", "  ", measure = measure), 0)
    expect_equal(chrf("abc", "xyz", measure = measure), 0)
  }
})

test_that("an unknown measure errors", {
  expect_error(chrf("abc", "abc", measure = "accuracy"))
})
