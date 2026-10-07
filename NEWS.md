# ditto (development version)

* Initial version.
* `clean()` normalises text by lowercasing, removing punctuation, and
  collapsing whitespace. Decimal marks between digits and percent signs after
  numbers are kept, so `"6.2 %"` becomes `"6.2%"` rather than `"62"`. Its
  `decimal_mark` argument sets whether `"."` (the default) or `","` is the
  decimal mark; the other is removed as a thousands separator.
* `bleu()` computes a sentence-level BLEU score from clipped n-gram precision
  and a brevity penalty.
* `chrf()` computes a character n-gram F-score. Whitespace is removed before
  the character n-grams are extracted, as in the reference implementation.
  Scores agree with `sacrebleu`'s CHRF. Its `measure` argument returns the
  F-score (the default), or the recall or precision averaged over n-gram
  orders. In the F-score, `beta = Inf` gives recall rather than `NaN`, and
  `beta = 0` gives precision.
* `rouge()` computes ROUGE-1, ROUGE-2, and ROUGE-L. Its `measure` argument
  returns the F-score (the default), recall (ROUGE-N as Lin (2004) defines
  it), or precision. In the F-score, `beta = Inf` gives recall rather than
  `NaN`, and `beta = 0` gives precision.
* `jaccard()` computes the Jaccard similarity of the sets of words, or of
  word n-grams with its `n` argument, in a candidate and a reference. Two
  strings without any n-grams score 1 if their words are identical and 0
  otherwise. Scores agree with `nltk`'s `jaccard_distance`.
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
  started with `--pooling none`. `token_embeddings()` determines how many
  special tokens the model adds at each end from the server's `/tokenize`
  endpoint rather than dropping a fixed row at each end, so models that add
  only a leading token, such as IBM's `granite-embedding-r2`, keep their final
  content token. The layout is cached per host, so this costs no extra requests
  once derived; `start_llama_server()` and `stop_llama_server()` clear the
  cache, as does a change in the embedding dimension.
* `compare_strings()` returns Levenshtein, character Jaccard (`jaccard_char`),
  cosine, word Jaccard (`jaccard_word`), BLEU, CHRF, ROUGE-1, ROUGE-L, TER,
  WER, and METEOR scores in one table, with optional BERTScore F1 and
  embedding cosine columns. The character Jaccard column was previously named
  `jaccard`.
* `bleu()`, `chrf()`, `rouge()`, `jaccard()`, `ter()`, `wer()`, and `meteor()`
  are validated against `sacrebleu`, `rouge-score`, `jiwer`, and `nltk`; see
  `vignette("metrics")` for the settings and the known departures, and
  `dev/validation/` to reproduce the comparison.
* Added the "Comparing strings with ditto" vignette, the "The metrics, and what
  they are validated against" vignette, and the "Validating bertscore() against
  the reference implementation" vignette.
