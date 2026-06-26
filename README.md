# ditto

String and short-text similarity metrics in R.

`ditto` collects the surface-level, n-gram, and embedding-based similarity
metrics used for comparing strings such as survey questions, translations,
and free-text responses. It complements
[`stringdist`](https://github.com/markvanderloo/stringdist): the
edit-distance, Jaccard, and cosine metrics come from there, while `ditto`
adds a from-scratch BLEU implementation and a BERTScore metric, plus a single
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

## BERTScore and the embedding backend

`bertscore()` and `token_embeddings()` compute token-level similarity from
contextual embeddings. They need per-token (un-pooled) embeddings, which are
served by a local [`llama.cpp`](https://github.com/ggml-org/llama.cpp) server
started with `--pooling none`:

```sh
llama-server -m model.gguf --embedding --pooling none
```

Then:

```r
bertscore("how much do you agree", "to what extent do you agree")

# Include BERTScore F1 in the comparison table
compare_strings(candidate, reference, bert = TRUE)
```

The default server URL is `http://localhost:8080`; override it with the
`host` argument. `bertscore()` returns `precision`, `recall`, and `f1`. This
is the vanilla formulation, without IDF weighting or baseline rescaling.

> **Note:** the exact `/embeddings` response shape varies across `llama.cpp`
> versions. If `token_embeddings()` errors, check the response format your
> server returns and adjust the extraction in `R/bertscore.R`.

## License

MIT © Willem Sleegers
