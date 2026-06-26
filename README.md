# ditto

String and short-text similarity metrics in R.

`ditto` collects the surface-level, n-gram, and embedding-based similarity
metrics used for comparing strings such as survey questions, translations,
and free-text responses. It complements
[`stringdist`](https://github.com/markvanderloo/stringdist): the
edit-distance, Jaccard, and cosine metrics come from there, while `ditto`
adds a from-scratch BLEU implementation and two embedding metrics, a
token-level BERTScore and a whole-string `cosine_similarity()`, plus a single
`compare_strings()` call that returns them together.

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

`compare_strings()` returns a tibble with Levenshtein, Jaccard, cosine, and
BLEU columns. Inputs are compared as given; call `clean()` first if case or
punctuation should be ignored.

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
