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

test_that("reordered words are matched, not dropped", {
  # Both words appear in both strings, so precision and recall are 1 and
  # f_mean is 1. The reordering splits them into 2 chunks over 2 matches, the
  # maximum penalty of 0.5 * (2/2)^3, giving exactly 0.5. An order-preserving
  # alignment would match only one of the two words instead, charging the
  # reordering twice: once to recall, and again to chunking.
  expect_equal(meteor("a b", "b a"), 0.5)
})

test_that("a rotation keeps every match and pays only a chunk penalty", {
  # All 3 words match, so f_mean is 1, across 2 chunks: 1 - 0.5 * (2/3)^3.
  expect_equal(meteor("the cat sat", "sat the cat"), 1 - 0.5 * (2 / 3)^3)
})

test_that("a scattered match is penalized relative to a contiguous one", {
  # Both candidates match all 4 shared reference words, so f_mean is the same
  # for both; only the chunking differs. "a b c d" is one chunk, "d c b a" is
  # four, which draws the maximum 0.5 penalty.
  f_mean <- 10 * 1 * 0.8 / (0.8 + 9 * 1)
  expect_equal(meteor("a b c d", "a b c d e"), f_mean * (1 - 0.5 * (1 / 4)^3))
  expect_equal(meteor("d c b a", "a b c d e"), f_mean * (1 - 0.5))
  expect_gt(meteor("a b c d", "a b c d e"), meteor("d c b a", "a b c d e"))
})

test_that("a repeated candidate word matches only as often as it occurs", {
  # Three "a"s cannot all match the two in the reference, so precision is 2/3.
  s <- meteor("a a a", "a a")
  expect_gt(s, 0)
  expect_lt(s, 1)
})

test_that("an exact match is aligned before a stem match competes for the word", {
  # "running" matches the reference's "running" exactly, so it takes that word
  # even though "runs" stems to the same "run" as the reference's "run". The
  # exact match crosses, giving 2 chunks over 2 matches and the full penalty:
  # f_mean 2/3 * (1 - 0.5) = 1/3. Stemming everything first would instead let
  # "running"/"runs" align contiguously for 1 chunk and score 0.625.
  # Value confirmed against nltk.translate.meteor_score with the Snowball
  # English stemmer and the synonym stage disabled.
  expect_equal(meteor("running runs runner", "run running ran"), 1 / 3)
})

test_that("staged_alignment claims exact matches before stem matches", {
  cand <- c("running", "runs")
  ref <- c("run", "running")
  pairs <- staged_alignment(cand, ref, c("run", "run"), c("run", "run"))
  expect_equal(nrow(pairs), 2)
  # "running" (candidate 1) takes the exact "running" (reference 2).
  expect_equal(unname(pairs[pairs[, "i"] == 1, "j"]), 2)
})

test_that("staged_alignment falls back to stems for the unclaimed words", {
  pairs <- staged_alignment(
    c("cats", "sat"), c("cat", "sat"),
    c("cat", "sat"), c("cat", "sat")
  )
  # "sat" matches exactly; "cats" is left over and matches "cat" by stem.
  expect_equal(nrow(pairs), 2)
  expect_equal(unname(pairs[, "j"]), c(1, 2))
})

test_that("language selects the stemmer", {
  # "katten" and "liepen" stem to "kat" and "liep" in Dutch, but the English
  # stemmer leaves them alone, so only the Dutch stemmer sees the match.
  dutch <- meteor("de katten liepen", "de kat liep", language = "dutch")
  english <- meteor("de katten liepen", "de kat liep", language = "en")
  expect_gt(dutch, 0.98)
  expect_lt(english, dutch)
})

test_that("an unsupported language errors", {
  expect_error(meteor("a b", "a b", language = "klingon"))
})

test_that("punctuation is a token of its own", {
  # "agree." is "agree" plus a punctuation token, not a wholly different word.
  expect_gt(meteor("do you agree.", "do you agree?"), 0)
})

test_that("surrounding whitespace does not change the score", {
  expect_equal(meteor("  the cat  ", "the cat"), meteor("the cat", "the cat"))
})

test_that("a string with no words scores 0", {
  expect_equal(meteor("", ""), 0)
  expect_equal(meteor("a b", ""), 0)
  expect_equal(meteor("   ", "a b"), 0)
})

test_that("a missing input gives a missing score", {
  expect_identical(meteor(NA_character_, "a b"), NA_real_)
  expect_identical(meteor("a b", NA_character_), NA_real_)
})

test_that("a vector of strings errors rather than scoring only the first pair", {
  expect_error(meteor(c("a b", "c d"), c("a b", "c d")), "single string")
})
