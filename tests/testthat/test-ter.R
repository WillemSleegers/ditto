test_that("identical strings score 0", {
  expect_equal(ter("a b c d", "a b c d"), 0)
})

test_that("one substitution costs one edit over the reference length", {
  expect_equal(ter("a b x d", "a b c d"), 1 / 4)
})

test_that("a candidate needing more edits than the reference has words can exceed 1", {
  expect_gt(ter("w x y z", "a b"), 1)
})

test_that("a reordering costs one shift, not one substitution per word", {
  # Swapping "a" and "b" moves a single word, so TER charges one edit where
  # word error rate charges two.
  expect_equal(ter("b a c d", "a b c d"), 1 / 4)
  expect_equal(wer("b a c d", "a b c d"), 2 / 4)
})

test_that("a moved block of words costs one edit however long it is", {
  # Moving "a b c" as a block is one shift, so the score does not grow with
  # the size of the block.
  expect_equal(ter("d e f a b c", "a b c d e f"), 1 / 6)
  expect_gt(wer("d e f a b c", "a b c d e f"), ter("d e f a b c", "a b c d e f"))
})

test_that("punctuation is tokenized separately from the word it follows", {
  # Without this, "statement." and "statement?" would be two totally
  # different tokens, hiding the fact that the word itself matches.
  cand <- "do you agree with this statement."
  ref <- "do you agree with this statement?"
  expect_equal(ter(cand, ref), 1 / 7)
})

test_that("surrounding whitespace does not change the score", {
  expect_equal(ter("  a b  ", "a b"), 0)
  expect_equal(ter("a  b", "a b"), 0)
})

test_that("scores match sacrebleu's TER", {
  # sacrebleu TER(case_sensitive = TRUE) on pre-tokenized text, rescaled from
  # 0-100 to 0-1. See dev/validation/text-metrics-reference.py.
  expect_equal(
    ter("the cat sat on the mat", "a cat was sitting on the mat"),
    0.4285714,
    tolerance = 1e-6
  )
  expect_equal(
    ter("she walked slowly to the store", "slowly she walked to the shop"),
    0.3333333,
    tolerance = 1e-6
  )
  expect_equal(ter("one two three four five", "five four three two one"), 0.8)
  expect_equal(
    ter(
      "how much do you agree with the statement",
      "to what extent do you agree with the statement"
    ),
    0.3333333,
    tolerance = 1e-6
  )
})

test_that("an empty reference is handled rather than dividing by zero", {
  # TER divides by the reference length, so an empty reference has no score
  # unless the candidate is empty too, in which case no edits are needed.
  expect_equal(ter("", ""), 0)
  expect_equal(ter("   ", ""), 0)
  expect_equal(ter("a b", ""), Inf)
  expect_equal(ter("a", "   "), Inf)
})

test_that("an empty candidate costs one deletion per reference word", {
  expect_equal(ter("", "a b"), 1)
})

test_that("a missing input gives a missing score", {
  expect_identical(ter(NA_character_, "a b"), NA_real_)
  expect_identical(ter("a b", NA_character_), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(ter(c("a b", "c d"), c("a b", "c d")), "single string")
})
