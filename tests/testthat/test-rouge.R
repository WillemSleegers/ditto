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

test_that("surrounding whitespace does not change the score", {
  # Splitting on "\\s+" would leave a leading empty token, dropping this from
  # 1 to 2/3.
  expect_equal(rouge(" cat ", "cat", variant = "1"), 1)
  expect_equal(rouge("a  b", "a b", variant = "1"), 1)
  expect_equal(rouge("\ta b\n", "a b", variant = "l"), 1)
})

test_that("punctuation is tokenized separately from the word it follows", {
  # "agree." is the word "agree" plus a punctuation token, so it half-matches
  # "agree?" rather than missing it entirely.
  expect_equal(rouge("agree.", "agree?", variant = "1"), 0.5)
})

test_that("a string with no words scores 0", {
  # Two empty strings have no n-grams to match, so there is nothing to credit.
  expect_equal(rouge("", "", variant = "1"), 0)
  expect_equal(rouge("", "", variant = "l"), 0)
  expect_equal(rouge("   ", "a b", variant = "1"), 0)
  expect_equal(rouge("a b", "", variant = "l"), 0)
})

test_that("beta weights recall relative to precision", {
  # The candidate contains the reference plus an extra word: recall is 1 and
  # precision is 2/3, so a recall-heavy beta scores higher.
  cand <- "a b c"
  ref <- "a b"
  expect_gt(
    rouge(cand, ref, variant = "1", beta = 9),
    rouge(cand, ref, variant = "1", beta = 1)
  )
})

test_that("measure returns recall, precision, or the F-score", {
  # 6 of the candidate's 8 words and of the reference's 9 words match.
  cand <- "how much do you agree with the statement"
  ref <- "to what extent do you agree with the statement"
  r <- 6 / 9
  p <- 6 / 8
  expect_equal(rouge(cand, ref, variant = "1", measure = "recall"), r)
  expect_equal(rouge(cand, ref, variant = "1", measure = "precision"), p)
  expect_equal(rouge(cand, ref, variant = "1"), 2 * p * r / (p + r))
  expect_equal(rouge(cand, ref, variant = "1"), 0.7059, tolerance = 1e-4)
})

test_that("variant l recall is the LCS length over the reference length", {
  # The LCS is "to what extent do you agree with", 7 of the reference's 9
  # words; the moved "the statement" cannot also be matched in order.
  cand <- "the statement to what extent do you agree with"
  ref <- "to what extent do you agree with the statement"
  expect_equal(rouge(cand, ref, variant = "l", measure = "recall"), 7 / 9)
})

test_that("beta = Inf gives recall and beta = 0 gives precision", {
  cand <- "how much do you agree with the statement"
  ref <- "to what extent do you agree with the statement"
  for (variant in c("1", "2", "l")) {
    expect_equal(
      rouge(cand, ref, variant = variant, beta = Inf),
      rouge(cand, ref, variant = variant, measure = "recall")
    )
    expect_equal(
      rouge(cand, ref, variant = variant, beta = 0),
      rouge(cand, ref, variant = variant, measure = "precision")
    )
  }
})

test_that("beta is ignored unless measure is the F-score", {
  expect_equal(
    rouge("a b c", "a b", measure = "precision", beta = Inf),
    2 / 3
  )
  expect_equal(rouge("a b c", "a b", measure = "recall", beta = 0), 1)
})

test_that("beta must be a single non-negative number", {
  expect_error(rouge("a b", "a b", beta = -1), "non-negative")
  expect_error(rouge("a b", "a b", beta = c(1, 2)), "non-negative")
  expect_error(rouge("a b", "a b", beta = NA_real_), "non-negative")
  expect_error(rouge("a b", "a b", beta = "1"), "non-negative")
})

test_that("recall and precision keep the NA and empty-string cases", {
  for (measure in c("recall", "precision")) {
    expect_identical(rouge(NA_character_, "a b", measure = measure), NA_real_)
    expect_identical(
      rouge("a b", NA_character_, variant = "l", measure = measure),
      NA_real_
    )
    expect_equal(rouge("", "", measure = measure), 0)
    expect_equal(rouge("", "a b", variant = "l", measure = measure), 0)
    expect_equal(rouge("a b", "", measure = measure), 0)
  }
  expect_equal(rouge("", "a b", beta = Inf), 0)
  expect_equal(rouge("a b", "", beta = 0), 0)
  # A one-word string has no bigrams, so variant 2's denominator is 0.
  expect_equal(rouge("a", "a b", variant = "2", measure = "precision"), 0)
})

test_that("scores match Google's rouge-score reference implementation", {
  # rouge_scorer.RougeScorer(use_stemmer = False) with ditto's tokenizer,
  # F-measure (ditto's default beta = 1).
  # See dev/validation/text-metrics-reference.py.
  cand <- "the cat sat on the mat"
  ref <- "a cat was sitting on the mat"
  expect_equal(rouge(cand, ref, variant = "1"), 0.6153846, tolerance = 1e-6)
  expect_equal(rouge(cand, ref, variant = "2"), 0.3636364, tolerance = 1e-6)
  expect_equal(rouge(cand, ref, variant = "l"), 0.6153846, tolerance = 1e-6)
})

test_that("a missing input gives a missing score", {
  expect_identical(rouge(NA_character_, "a b"), NA_real_)
  expect_identical(rouge("a b", NA_character_, variant = "l"), NA_real_)
})

test_that("an unknown variant errors", {
  expect_error(rouge("a b", "a b", variant = "3"))
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(rouge(c("a b", "c d"), c("a b", "c d")), "single string")
})
