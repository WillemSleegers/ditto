test_that("identical strings score close to 1", {
  # A perfect match is still a single chunk, so the fragmentation penalty is
  # not exactly 0; the score approaches 1 as the sentence gets longer rather
  # than hitting it exactly. This matches the published METEOR formula.
  expect_equal(meteor("the cat sat on the mat", "the cat sat on the mat"), 0.9977, tolerance = 1e-4)
  expect_gt(meteor("the cat sat on the mat", "the cat sat on the mat"), 0.99)
})

test_that("strings with no shared stems score 0", {
  expect_equal(meteor("a b c d", "w x y z"), 0)
})

test_that("a partial match lands strictly between 0 and 1", {
  s <- meteor("the cats are agreeing", "the cat agreed")
  expect_gt(s, 0)
  expect_lt(s, 1)
})

test_that("stemming credits an inflected form bleu misses entirely", {
  expect_equal(bleu("agreeing", "agreed"), 0)
  expect_gt(meteor("agreeing", "agreed"), 0)
})

test_that("a scattered match is penalized relative to a contiguous one", {
  # Both candidates share all 4 reference words, but "d c b a" matches them
  # out of order, breaking every word into its own chunk.
  contiguous <- meteor("a b c d", "a b c d e")
  scattered <- meteor("d c b a", "a b c d e")
  expect_gt(contiguous, scattered)
})
