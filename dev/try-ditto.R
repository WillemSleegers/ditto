# try-ditto.R ------------------------------------------------------------------
# A scratch script for exploring the {ditto} package during development.
# Run sections interactively (Ctrl/Cmd + Enter) rather than sourcing the whole
# file, since some parts (BERTScore) need a running llama.cpp server.

# 1. Setup ---------------------------------------------------------------------

# Install dev tooling if needed (uncomment as required):
# install.packages(c("devtools", "pkgload", "roxygen2", "testthat"))

# Load the package from source (no install needed). This sources everything in
# R/ and attaches the package, so the exported functions are available.
devtools::load_all(".")

# Confirm the exported API. Note that load_all() attaches the package's internal
# objects as well, so ls("package:ditto") shows helpers like tokenize_words()
# alongside the real exports. Ask the namespace instead:
sort(getNamespaceExports("ditto"))
# Expect these 18, in three groups:
#   text metrics:      bleu, chrf, rouge, ter, wer, meteor
#   embedding metrics: bertscore, cosine_similarity, token_embeddings,
#                      bertscore_baseline, bertscore_baselines
#   everything else:   clean, compare_strings, example_sentences,
#                      start_llama_server, stop_llama_server,
#                      llama_server_running, find_llama_server

# 2. Documentation -------------------------------------------------------------

# (Re)generate man pages and NAMESPACE from the roxygen comments. Run this after
# editing any #' documentation in R/.
devtools::document()

# Open help pages for the exported functions:
?clean
?bleu
?chrf
?rouge
?ter
?wer
?meteor
?compare_strings
?bertscore
?cosine_similarity
?token_embeddings

# Package-level docs:
?ditto

# 3. Vignettes -----------------------------------------------------------------

# List vignettes the package knows about:
tools::getVignetteInfo("ditto")          # works once the package is installed
list.files("vignettes")                  # source files in this repo

# vignettes/ditto.Rmd                 -- tour of every metric
# vignettes/metrics.Rmd               -- what each metric is validated against
# vignettes/bertscore-validation.Rmd  -- bertscore() vs. the Python reference

# Build + view a vignette from source:
# devtools::build_vignettes()
# rmarkdown::render("vignettes/ditto.Rmd"); browseURL("vignettes/ditto.html")

# 4. Text metrics --------------------------------------------------------------
# These need no model and no server. Each takes a single candidate and a single
# reference, and errors on longer vectors rather than silently scoring the first
# pair. A missing input gives a missing score.

## clean() -- normalise surface differences
clean("To what extent do you AGREE with the statement?")
clean(c("Hello,  World!", "multiple   spaces"))

## bleu() -- clipped n-gram precision with a brevity penalty, in [0, 1]
bleu("the cat sat on the mat", "the cat sat on the mat")   # identical -> 1
bleu(                                                      # partial  -> 0.60
  "how much do you agree with the statement",
  "to what extent do you agree with the statement"
)
bleu("short", "short", max_n = 2)                          # max_n caps at 2
# BLEU is 0 as soon as one n-gram order has no match at all. Both of these
# share words with the reference, yet neither shares a 4-gram, so BLEU cannot
# tell them apart or rank them against anything else:
bleu("the cat sat on the mat", "the cat is on the mat")
bleu("completely different text", "the cat sat on the mat")

## chrf() -- character n-gram F-score, so inflection is not a total mismatch
chrf("agreeing", "agreed")     # shares character n-grams
bleu("agreeing", "agreed")     # shares no whole word -> 0
chrf("ab cd", "abcd")          # whitespace is stripped first -> 1
chrf("agreeing", "agreed", n = 4, beta = 1)   # shorter n-grams, balanced F

## rouge() -- recall-oriented; "l" matches the longest common subsequence
rouge("the cat sat on the mat", "a cat was sitting on the mat")               # ROUGE-1
rouge("the cat sat on the mat", "a cat was sitting on the mat", variant = "2")
rouge("the cat sat on the mat", "a cat was sitting on the mat", variant = "l")
# ROUGE-1 ignores order; ROUGE-L does not:
rouge("cat the", "the cat", variant = "1")   # 1.0
rouge("cat the", "the cat", variant = "l")   # 0.5

## ter() and wer() -- error rates, not similarities: 0 is perfect, and both can
## exceed 1. ter() counts a moved block of words as a single edit (a "shift");
## wer() charges one edit per word. Read them together: where they diverge, the
## candidate has the right words in the wrong place.
ter("the cat sat on the mat", "a cat was sitting on the mat")
wer("the cat sat on the mat", "a cat was sitting on the mat")

c(ter = ter("d e f a b c", "a b c d e f"),    # one block moved  -> 0.167
  wer = wer("d e f a b c", "a b c d e f"))    # six rewrites     -> 1.0

c(ter = ter("b a c d", "a b c d"),            # one word displaced
  wer = wer("b a c d", "a b c d"))

## meteor() -- stem matching, recall-weighted, with a fragmentation penalty
meteor("the cats are agreeing", "the cat agreed")   # matches by stem
meteor("the cat sat", "sat the cat")                # matched, then penalised
# Stemming is language-specific; the default is English.
meteor("de katten liepen", "de kat liep", language = "dutch")
meteor("de katten liepen", "de kat liep")           # English stemmer sees less

## Bundled corpus, used by the baseline helpers below
head(example_sentences)
table(example_sentences$language)

## compare_strings() -- every non-embedding metric in one tibble
reference <- clean("To what extent do you agree with the following statements?")
variants <- clean(c(
  "To what extent do you agree with the following statement?", # near-identical
  "How much do you agree with each of the statements below?",  # reworded
  "How satisfied are you with the service you received?",       # unrelated
  "What is your highest level of education completed?"          # unrelated
))

compare_strings(
  candidate = variants,
  reference = rep(reference, length(variants))
)

# Every column is a 0-1 similarity except ter and wer, which run the other way.
# Pass `language` to pick the stemmer behind the meteor column.
compare_strings("de katten liepen", "de kat liep", language = "dutch")

# 5. Embedding metrics (need a local llama.cpp embedding server) ---------------
# The start_/stop_llama_server() helpers launch the server for you. Point them
# at your llama-server binary and a .gguf embedding model. You can also set
# these once in your .Rprofile via options(ditto.llama_server=, ditto.llama_model=)
# and then call start_llama_server() with no arguments.
#
# bge-m3 is multilingual, needs no task prefix, and uses CLS pooling (so pass
# pooling = "cls" to the cosine metric). all-MiniLM-L6-v2 is a smaller,
# English-only, mean-pooling alternative.

llama_exe   <- "C:/Users/wslee/tools/llama.cpp/llama-server.exe"
llama_model <- "C:/Users/wslee/tools/llama.cpp/models/bge-m3-f16.gguf"

find_llama_server()                                       # resolve from options/PATH
start_llama_server(model = llama_model, exe = llama_exe)  # waits until ready
llama_server_running()                                    # TRUE when up

token_embeddings("hello world")                 # matrix: tokens x dims
bertscore("the cat sat on the mat", "a cat is on the mat")
cosine_similarity("the cat sat on the mat", "a cat is on the mat", pooling = "cls")

# All metrics in one table, with the embedding columns (pooling = "cls" for bge-m3).
compare_strings(variants, rep(reference, length(variants)),
                bert = TRUE, pooling = "cls")

# Baseline rescaling: estimate the unrelated-text floor, then rescale.
baseline <- bertscore_baseline(seed = 1)        # defaults to English example_sentences
compare_strings(variants, rep(reference, length(variants)),
                bert = TRUE, pooling = "cls", baseline = baseline)

# One baseline per language, for a multilingual model.
baselines <- bertscore_baselines(seed = 1)
bertscore("in hoeverre bent u het eens", "hoeveel bent u het eens",
          baseline = baselines[["nl"]])

stop_llama_server()                              # shut it down when done

# 6. Validation ----------------------------------------------------------------
# The text metrics are checked against their reference implementations, not just
# unit-tested. See vignette("metrics") for the settings and the known
# departures. To reproduce the comparison:
#
#   pip install sacrebleu rouge-score jiwer nltk
#   python dev/validation/text-metrics-reference.py   # writes reference scores
#   Rscript dev/validation/text-metrics-ditto.R       # scores the same pairs
#
# The R script errors if any metric drifts from its reference. bertscore has its
# own pair of scripts, which additionally need the server from section 5:
#
#   pip install bert-score
#   python dev/validation/bertscore-reference.py
#   Rscript dev/validation/bertscore-ditto.R

# 7. Package-level checks ------------------------------------------------------

# Run the test suite:
devtools::test()

# Test coverage (requires the {covr} package):
# covr::report(covr::package_coverage())

# Full R CMD check (build, examples, tests, vignettes, docs):
# devtools::check()

# Session / package info for debugging:
sessionInfo()
