# ditto (development version)

* Initial version.
* `clean()` normalises text by lowercasing, removing punctuation, and
  collapsing whitespace.
* `bleu()` computes a sentence-level BLEU score from clipped n-gram precision
  and a brevity penalty.
* `bertscore()` and `token_embeddings()` compute token-level semantic
  similarity from per-token embeddings served by a local `llama.cpp` server
  started with `--pooling none`.
* `compare_strings()` returns Levenshtein, Jaccard, cosine, and BLEU scores in
  one table, with an optional BERTScore F1 column.
* Added the "Comparing strings with ditto" vignette.
