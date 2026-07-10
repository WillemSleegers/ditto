test_that("identical strings score 1", {
  expect_equal(bleu("a b c d", "a b c d"), 1)
})

test_that("strings with no shared words score 0", {
  expect_equal(bleu("a b c d", "w x y z"), 0)
})

test_that("a partial match lands strictly between 0 and 1", {
  s <- bleu(
    "how much do you agree with the statement",
    "to what extent do you agree with the statement"
  )
  expect_gt(s, 0)
  expect_lt(s, 1)
})

test_that("a shorter candidate is penalized below a full match", {
  reference <- "the quick brown fox jumps over the lazy dog"
  expect_equal(bleu(reference, reference), 1)
  short <- bleu("the quick brown fox", reference)
  expect_lt(short, 1)
})

test_that("max_n is capped at the shorter token length", {
  # Two tokens cannot form a 4-gram; capping avoids an automatic zero.
  expect_gt(bleu("completely agree", "completely agree"), 0)
})

test_that("surrounding whitespace does not change the score", {
  expect_equal(bleu("  a b c d  ", "a b c d"), 1)
  expect_equal(bleu("a  b", "a b"), 1)
})

test_that("scores match the sacrebleu reference implementation", {
  # sacrebleu BLEU(effective_order = TRUE, smooth_method = "none",
  # tokenize = "none") on pre-tokenized text, rescaled from 0-100 to 0-1.
  # See dev/validation/text-metrics-reference.py.
  expect_equal(
    bleu(
      "how much do you agree with the statement",
      "to what extent do you agree with the statement"
    ),
    0.6004288,
    tolerance = 1e-6
  )
  expect_equal(bleu("the cat sat on the mat", "a cat was sitting on the mat"), 0)
})

test_that("max_n is capped at the shorter string, unlike sacrebleu", {
  # sacrebleu's effective_order caps at the *candidate's* length, so when the
  # reference is the shorter side and has fewer than 4 tokens, it scores the
  # impossible orders as 0 and returns 0. ditto caps at the shorter of the two
  # and returns a non-zero score. This is the one place bleu() knowingly
  # departs from the reference.
  expect_equal(bleu("a b c d e", "a b"), 0.3162278, tolerance = 1e-6)
  expect_gt(bleu("the cat sat on the mat", "the cat"), 0)

  # When the candidate is the shorter side, the two caps coincide again.
  expect_equal(bleu("a b", "a b c d e"), 0.2231302, tolerance = 1e-6)
})

test_that("punctuation is a token of its own", {
  # "agree." is the word "agree" plus a punctuation token, so the trigram
  # "do you agree" still matches. Glued to the word it would not.
  expect_gt(bleu("do you agree.", "do you agree?", max_n = 3), 0)

  # At max_n = 4 the differing punctuation is inside the only 4-gram, and
  # BLEU zeroes out whenever any order has no match at all.
  expect_equal(bleu("do you agree.", "do you agree?"), 0)
})

test_that("a string with no words scores 0", {
  # Splitting on "\\s+" would leave one empty token in each string, which
  # then match each other and score a perfect 1.
  expect_equal(bleu("", ""), 0)
  expect_equal(bleu("   ", "   "), 0)
  expect_equal(bleu("a b", ""), 0)
})

test_that("a missing input gives a missing score", {
  expect_identical(bleu(NA_character_, "a b"), NA_real_)
  expect_identical(bleu("a b", NA_character_), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(bleu(c("a b", "c d"), c("a b", "c d")), "single string")
})
