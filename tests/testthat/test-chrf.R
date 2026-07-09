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
