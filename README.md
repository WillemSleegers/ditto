# ditto

String and short-text similarity metrics in R.

`ditto` collects the surface-level, n-gram, and embedding-based similarity
metrics used for comparing strings such as survey questions, translations,
and free-text responses. It complements
[`stringdist`](https://github.com/markvanderloo/stringdist): the
edit-distance, Jaccard, and cosine metrics come from there, while `ditto`
adds from-scratch implementations of the standard translation metrics
(`bleu()`, `chrf()`, `rouge()`, `ter()`, `wer()`, and `meteor()`) and two
embedding metrics, a token-level BERTScore and a whole-string
`cosine_similarity()`, plus a single `compare_strings()` call that returns them
together.

## Installation

```r
# install.packages("remotes")
remotes::install_github("WillemSleegers/ditto")
```

## Usage

```r
library(ditto)

candidate <- clean("How much do you agree with the statement?")
reference <- clean("To what extent do you agree with the statement?")

# One metric
bleu(candidate, reference)

# All surface metrics at once
compare_strings(candidate, reference)
```

`compare_strings()` returns a tibble with Levenshtein, Jaccard, cosine, BLEU,
CHRF, ROUGE-1, ROUGE-L, TER, and METEOR columns. Inputs are compared as given;
call `clean()` first if case or punctuation should be ignored.

## The word and character metrics

Each is also available on its own, taking one candidate and one reference:

```r
chrf("agreeing", "agreed")                   # character n-gram F-score
rouge("the cat sat", "a cat was sitting")    # variant = "1", "2", or "l"
ter("d e f a b c", "a b c d e f")            # 0.167: one block moved
wer("d e f a b c", "a b c d e f")            # 1.0: no shifts, so six rewrites
meteor("the cats are agreeing", "the cat agreed")
```

They differ in what counts as a difference. `bleu()` is precision-only and
strict on word order. `chrf()` counts character n-grams, so it credits an
inflected form that shares no whole words. `rouge()` is recall-oriented, with
an LCS variant that tolerates interspersed words. `meteor()` matches by stem
and penalises reordering through fragmentation; pass `language` to stem
non-English text. `ter()` and `wer()` are the odd ones out: *error* rates, 0 for
a perfect match and unbounded above. `ter()` counts a displaced block of words
as a single edit; `wer()` charges one edit per word.

The word-level metrics share a tokenizer that splits on whitespace and treats
punctuation as a token of its own, matching TER's original convention and
`sacrebleu`'s `13a` tokenizer. `chrf()` strips whitespace before extracting
character n-grams, as its reference implementation does.

## Validation

Each metric is checked against an external reference implementation rather than
only unit-tested: `bleu()`, `chrf()`, and `ter()` against `sacrebleu`, `rouge()`
against Google's `rouge-score`, `wer()` against `jiwer`, `meteor()` against
`nltk`, and `bertscore()` against the original `bert-score`. All agree to
floating-point error under matched settings, including `ter()`'s greedy shift
search, ported from TERCOM by way of `sacrebleu`.

Two departures are deliberate: `bleu()` caps its n-gram order at the shorter
string rather than the candidate, and `meteor()` has no WordNet synonym stage.
`vignette("metrics")` records the settings, the departures, and how to
reproduce the comparison from `dev/validation/`.

## Embedding metrics and the backend

`bertscore()` (token-level) and `cosine_similarity()` (whole-string) compute
similarity from contextual embeddings. Both need per-token (un-pooled)
embeddings, served by a local
[`llama.cpp`](https://github.com/ggml-org/llama.cpp) server started with
`--pooling none`. The vignette uses [`bge-m3`](https://huggingface.co/BAAI/bge-m3),
a multilingual model that needs no task prefix; `all-MiniLM-L6-v2` is a smaller,
English-only alternative.

```sh
llama-server -m bge-m3-f16.gguf --embedding --pooling none
```

Rather than manage the server by hand, `ditto` can launch it for you and wait
until it is ready:

```r
start_llama_server(model = "path/to/bge-m3-f16.gguf")  # also finds it on PATH
on.exit(stop_llama_server())

bertscore("how much do you agree", "to what extent do you agree")

# Whole-string cosine; pass pooling = "cls" for BGE models, "mean" for others.
cosine_similarity("how much do you agree", "to what extent do you agree",
                  pooling = "cls")

# Include the embedding columns (bertscore_f1, cosine_emb) in the table.
compare_strings(candidate, reference, bert = TRUE, pooling = "cls")
```

The default server URL is `http://localhost:8080`; override it with the `host`
argument. `bertscore()` returns `precision`, `recall`, and `f1` (the vanilla
formulation, without IDF weighting). Some models put even unrelated text at a
high similarity; `bertscore_baseline()` estimates that floor so scores can be
rescaled via the `baseline` argument. Models that require a task prefix (e.g.
the E5 family's `"query: "`) are supported through the `prefix` argument.

> **Note:** the exact `/embeddings` response shape varies across `llama.cpp`
> versions. If `token_embeddings()` errors, check the response format your
> server returns and adjust the extraction in `R/bertscore.R`.

## License

MIT © Willem Sleegers
