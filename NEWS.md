# ditto (development version)

* Initial version.
* `clean()` normalises text by lowercasing, removing punctuation, and
  collapsing whitespace.
* `bleu()` computes a sentence-level BLEU score from clipped n-gram precision
  and a brevity penalty.
* `chrf()` computes a character n-gram F-score. Whitespace is removed before
  the character n-grams are extracted, as in the reference implementation.
  Scores agree with `sacrebleu`'s CHRF.
* `rouge()` computes ROUGE-1, ROUGE-2, and ROUGE-L.
* `ter()` computes Translation Edit Rate, including TERCOM's greedy shift
  search, so a contiguous block of words moved elsewhere costs a single edit.
  It is an error rate rather than a similarity: 0 is a perfect match, and
  scores above 1 are possible.
* `wer()` computes word error rate, which is `ter()` without the shift search.
* `meteor()` computes a METEOR score with exact and stem matching; synonym
  matching is not included, as it requires a WordNet installation. Its
  `language` argument selects the stemmer, so non-English text can be scored.
  Scores agree with `nltk.translate.meteor_score` on text with no synonym
  matches.
* The word-level metrics share one tokenizer, which splits on whitespace and
  treats punctuation as a token of its own, matching TER's original convention
  and `sacrebleu`'s `13a` tokenizer.
* Every metric takes a single pair of strings, errors on longer vectors rather
  than silently scoring only the first pair, and returns `NA` when either
  input is `NA`.
* `bertscore()` and `token_embeddings()` compute token-level semantic
  similarity from per-token embeddings served by a local `llama.cpp` server
  started with `--pooling none`.
* `compare_strings()` returns Levenshtein, Jaccard, cosine, BLEU, CHRF,
  ROUGE-1, ROUGE-L, TER, WER, and METEOR scores in one table, with optional
  BERTScore F1 and embedding cosine columns.
* `bleu()`, `chrf()`, `rouge()`, `ter()`, `wer()`, and `meteor()` are validated
  against `sacrebleu`, `rouge-score`, `jiwer`, and `nltk`; see
  `vignette("metrics")` for the settings and the known departures, and
  `dev/validation/` to reproduce the comparison.
* Added the "Comparing strings with ditto" vignette, the "The metrics, and what
  they are validated against" vignette, and the "Validating bertscore() against
  the reference implementation" vignette.
