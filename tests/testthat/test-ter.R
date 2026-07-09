test_that("identical strings score 0", {
  expect_equal(ter("a b c d", "a b c d"), 0)
})

test_that("one substitution costs one edit over the reference length", {
  expect_equal(ter("a b x d", "a b c d"), 1 / 4)
})

test_that("a candidate needing more edits than the reference has words can exceed 1", {
  expect_gt(ter("w x y z", "a b"), 1)
})

test_that("a pure reordering costs more than a single shift would", {
  # This implementation has no shift operation, so swapping two words costs
  # two substitutions rather than the one edit true TER would charge.
  expect_equal(ter("b a c d", "a b c d"), 2 / 4)
})
