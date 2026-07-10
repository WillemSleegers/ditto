# text-metrics-ditto.R ---------------------------------------------------------
#
# ditto's own bleu(), chrf(), rouge(), ter(), and meteor() for the validation
# pairs, the R counterpart to text-metrics-reference.py. It scores the same
# pairs, writes text_metrics_ditto_scores.csv, and diffs the two.
#
# Unlike the BERTScore validation, this needs no server and no model: both
# sides are pure text. Run text-metrics-reference.py first, then this.
#
# Run from the package root (working directory = repo root), the same
# convention as dev/try-ditto.R.

devtools::load_all(".")

here <- "dev/validation"
reference <- read.csv(
  file.path(here, "text_metrics_reference_scores.csv"),
  stringsAsFactors = FALSE
)

pairwise <- function(f, ...) {
  mapply(f, reference$candidate, reference$reference,
    MoreArgs = list(...), USE.NAMES = FALSE
  )
}

scores <- data.frame(
  candidate = reference$candidate,
  reference = reference$reference,
  bleu = pairwise(bleu),
  chrf = pairwise(chrf),
  rouge_1 = pairwise(rouge, variant = "1"),
  rouge_2 = pairwise(rouge, variant = "2"),
  rouge_l = pairwise(rouge, variant = "l"),
  ter = pairwise(ter),
  wer = pairwise(wer),
  meteor = pairwise(meteor)
)
write.csv(scores, file.path(here, "text_metrics_ditto_scores.csv"), row.names = FALSE)

# bleu() caps its n-gram order at the length of the *shorter* string, whereas
# sacrebleu's effective_order caps at the candidate's length. The two therefore
# only have to agree when the reference is not the shorter, under-4-token side.
n_tokens <- function(x) vapply(x, function(s) length(tokenize_words(s)), integer(1))
n_cand <- n_tokens(reference$candidate)
n_ref <- n_tokens(reference$reference)
bleu_comparable <- n_ref >= pmin(4L, n_cand)

check <- function(metric, ours, theirs, subset = rep(TRUE, length(ours))) {
  diff <- abs(ours - theirs)[subset]
  cat(sprintf(
    "%-10s pairs=%2d  max diff=%-11s %s\n",
    metric, sum(subset), format(max(diff), digits = 3),
    if (max(diff) < 1e-8) "agree" else "DIFFER"
  ))
  invisible(max(diff) < 1e-8)
}

cat("ditto vs. the reference implementations\n\n")
ok <- c(
  check("bleu", scores$bleu, reference$bleu, bleu_comparable),
  check("chrf", scores$chrf, reference$chrf),
  check("rouge_1", scores$rouge_1, reference$rouge_1),
  check("rouge_2", scores$rouge_2, reference$rouge_2),
  check("rouge_l", scores$rouge_l, reference$rouge_l),
  check("ter", scores$ter, reference$ter),
  check("wer", scores$wer, reference$wer),
  check("meteor", scores$meteor, reference$meteor)
)

# ter() and wer() differ exactly where a shift beats per-word edits. Reporting
# that keeps the shift search visibly doing something, rather than silently
# reducing to word error rate if it ever broke.
shifted <- scores$ter < scores$wer - 1e-9
cat(sprintf(
  "\nshift search: ter() beats wer() on %d of %d pairs (largest saving %.3f)\n",
  sum(shifted), nrow(scores), max(scores$wer - scores$ter)
))

stopifnot(all(ok))
cat("\nall validated metrics agree with their reference implementation\n")
