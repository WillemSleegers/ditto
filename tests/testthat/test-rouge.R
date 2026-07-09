test_that("identical strings score 1 for every variant", {
  expect_equal(rouge("a b c d", "a b c d", variant = "1"), 1)
  expect_equal(rouge("a b c d", "a b c d", variant = "2"), 1)
  expect_equal(rouge("a b c d", "a b c d", variant = "l"), 1)
})

test_that("strings with no shared words score 0", {
  expect_equal(rouge("a b c d", "w x y z", variant = "1"), 0)
  expect_equal(rouge("a b c d", "w x y z", variant = "2"), 0)
  expect_equal(rouge("a b c d", "w x y z", variant = "l"), 0)
})

test_that("a partial match lands strictly between 0 and 1", {
  s <- rouge("the cat sat on the mat", "a cat was sitting on the mat")
  expect_gt(s, 0)
  expect_lt(s, 1)
})

test_that("variant l tolerates an insertion that variant 2 penalizes", {
  # "the cat sat on the mat" is a subsequence of "the cat was sat on the
  # mat", so the LCS covers the whole candidate. The inserted "was" breaks
  # the bigram "cat sat" into "cat was" and "was sat", which variant 2 does
  # not credit.
  cand <- "the cat sat on the mat"
  ref <- "the cat was sat on the mat"
  expect_gt(rouge(cand, ref, variant = "l"), rouge(cand, ref, variant = "2"))
})

test_that("a short candidate scores above 0 where bleu would score 0", {
  # ROUGE has no brevity penalty, unlike BLEU, so a short candidate that
  # shares individual words but no bigram with the reference still gets
  # partial unigram credit.
  cand <- "cat mat"
  ref <- "the cat sat on the mat"
  expect_equal(bleu(cand, ref), 0)
  expect_gt(rouge(cand, ref, variant = "1"), 0)
})
