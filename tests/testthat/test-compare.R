candidates <- c("how much do you agree with the statement", "what is your age")
references <- rep("to what extent do you agree with the statement", 2)

test_that("the surface metrics are returned in one row per pair", {
  out <- compare_strings(candidates, references)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_named(out, c(
    "candidate", "reference", "levenshtein", "jaccard", "cosine",
    "bleu", "chrf", "rouge_1", "rouge_l", "ter", "wer", "meteor"
  ))
})

test_that("each column agrees with calling the metric directly", {
  out <- compare_strings(candidates, references)
  expect_equal(out$bleu, mapply(bleu, candidates, references, USE.NAMES = FALSE))
  expect_equal(out$chrf, mapply(chrf, candidates, references, USE.NAMES = FALSE))
  expect_equal(out$ter, mapply(ter, candidates, references, USE.NAMES = FALSE))
  expect_equal(out$wer, mapply(wer, candidates, references, USE.NAMES = FALSE))
  expect_equal(out$meteor, mapply(meteor, candidates, references, USE.NAMES = FALSE))
})

test_that("ter is never worse than wer, since a shift costs at most one edit", {
  out <- compare_strings(
    c("b a c d", "d e f a b c", "a b c d"),
    c("a b c d", "a b c d e f", "a b c d")
  )
  expect_true(all(out$ter <= out$wer))
  expect_true(any(out$ter < out$wer))
})

test_that("rouge_1 and rouge_l use their respective variants", {
  out <- compare_strings(candidates, references)
  expect_equal(out$rouge_1, vapply(candidates, rouge, numeric(1),
    reference = references[1], variant = "1", USE.NAMES = FALSE
  ))
  expect_equal(out$rouge_l, vapply(candidates, rouge, numeric(1),
    reference = references[1], variant = "l", USE.NAMES = FALSE
  ))
})

test_that("rouge_l sees word order where rouge_1 does not", {
  # Both words match either way, but only one of them is in the same order.
  out <- compare_strings("cat the", "the cat")
  expect_equal(out$rouge_1, 1)
  expect_equal(out$rouge_l, 0.5)
})

test_that("an identical pair scores 1 on the similarities and 0 on ter", {
  out <- compare_strings("the cat sat", "the cat sat")
  expect_equal(out$bleu, 1)
  expect_equal(out$chrf, 1)
  expect_equal(out$rouge_1, 1)
  expect_equal(out$rouge_l, 1)
  expect_equal(out$ter, 0)
  expect_gt(out$meteor, 0.9)
})

test_that("ter is an error rate, so it is not bounded above by 1", {
  # Every other column is a 0-1 similarity; ter is the odd one out.
  out <- compare_strings("w x y z", "a b")
  expect_gt(out$ter, 1)
  expect_lte(out$bleu, 1)
  expect_lte(out$chrf, 1)
})

test_that("language is passed through to the meteor column", {
  out <- compare_strings("de katten liepen", "de kat liep", language = "dutch")
  expect_equal(out$meteor, meteor("de katten liepen", "de kat liep", language = "dutch"))
  expect_gt(out$meteor, compare_strings("de katten liepen", "de kat liep")$meteor)
})

test_that("a missing input gives missing scores rather than scoring \"NA\"", {
  out <- compare_strings(c("a b", NA), c("a b", "a b"))
  expect_identical(out$bleu, c(1, NA_real_))
  expect_identical(out$ter, c(0, NA_real_))
  expect_identical(out$meteor[2], NA_real_)
})

test_that("the embedding columns are omitted unless bert = TRUE", {
  out <- compare_strings("a b", "a b")
  expect_false("bertscore_f1" %in% names(out))
  expect_false("cosine_emb" %in% names(out))
})
