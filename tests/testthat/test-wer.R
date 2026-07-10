test_that("identical strings score 0", {
  expect_equal(wer("a b c d", "a b c d"), 0)
})

test_that("one substitution costs one edit over the reference length", {
  expect_equal(wer("a b x d", "a b c d"), 1 / 4)
})

test_that("a candidate needing more edits than the reference has words can exceed 1", {
  expect_gt(wer("w x y z", "a b"), 1)
})

test_that("a reordering is charged per word, unlike ter()", {
  # This is the whole difference between the two metrics: wer() has no shift
  # operation, so moving a word costs a deletion and an insertion.
  expect_equal(wer("b a c d", "a b c d"), 2 / 4)
  expect_lt(ter("b a c d", "a b c d"), wer("b a c d", "a b c d"))
})

test_that("scores match jiwer's word error rate", {
  # jiwer.wer on the same tokens. See dev/validation/text-metrics-reference.py.
  expect_equal(
    wer("the cat sat on the mat", "a cat was sitting on the mat"),
    0.4285714,
    tolerance = 1e-6
  )
  expect_equal(
    wer("she walked slowly to the store", "slowly she walked to the shop"),
    0.5
  )
  expect_equal(
    wer(
      "how much do you agree with the statement",
      "to what extent do you agree with the statement"
    ),
    0.3333333,
    tolerance = 1e-6
  )
})

test_that("punctuation is tokenized separately from the word it follows", {
  expect_equal(wer("do you agree.", "do you agree?"), 1 / 4)
})

test_that("surrounding whitespace does not change the score", {
  expect_equal(wer("  a b  ", "a b"), 0)
  expect_equal(wer("a  b", "a b"), 0)
})

test_that("an empty reference is handled rather than dividing by zero", {
  expect_equal(wer("", ""), 0)
  expect_equal(wer("a b", ""), Inf)
  expect_equal(wer("a", "   "), Inf)
})

test_that("an empty candidate costs one deletion per reference word", {
  expect_equal(wer("", "a b"), 1)
})

test_that("a missing input gives a missing score", {
  expect_identical(wer(NA_character_, "a b"), NA_real_)
  expect_identical(wer("a b", NA_character_), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(wer(c("a b", "c d"), c("a b", "c d")), "single string")
})
