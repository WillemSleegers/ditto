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
