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

# Confirm the exported API:
ls("package:ditto")
# Expect: bertscore, bleu, clean, compare_strings, token_embeddings

# 2. Documentation -------------------------------------------------------------

# (Re)generate man pages and NAMESPACE from the roxygen comments. Run this after
# editing any #' documentation in R/.
devtools::document()

# Open help pages for the exported functions:
?clean
?bleu
?compare_strings
?bertscore
?token_embeddings

# Package-level docs:
?ditto

# 3. Vignette ------------------------------------------------------------------

# List vignettes the package knows about:
tools::getVignetteInfo("ditto")          # works once the package is installed
list.files("vignettes")                  # source files in this repo

# Build + view the vignette from source (renders vignettes/ditto.Rmd):
# devtools::build_vignettes()
# Or knit/preview directly:
# rmarkdown::render("vignettes/ditto.Rmd"); browseURL("vignettes/ditto.html")

# 4. Try the functions ---------------------------------------------------------

## clean() -- normalise surface differences
clean("To what extent do you AGREE with the statement?")
clean(c("Hello,  World!", "multiple   spaces"))

## bleu() -- word-sequence overlap, score in [0, 1]
bleu("the cat sat on the mat", "the cat sat on the mat")   # identical -> 1
bleu("the cat sat on the mat", "the cat is on the mat")    # partial overlap
bleu("completely different text", "the cat sat on the mat")
bleu("short", "short", max_n = 2)

## compare_strings() -- character- and word-level metrics in one tibble
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

# 5. BERTScore (needs a local llama.cpp embedding server) ----------------------
# The start_/stop_llama_server() helpers launch the server for you. Point them
# at your llama-server binary and a .gguf embedding model. You can also set
# these once in your .Rprofile via options(ditto.llama_server=, ditto.llama_model=)
# and then call start_llama_server() with no arguments.

llama_exe   <- "C:/Users/wslee/tools/llama.cpp/llama-server.exe"
llama_model <- "C:/Users/wslee/tools/llama.cpp/models/all-MiniLM-L6-v2-f16.gguf"

start_llama_server(model = llama_model, exe = llama_exe)  # waits until ready
llama_server_running()                                    # TRUE when up

token_embeddings("hello world")                 # matrix: tokens x dims
bertscore("the cat sat on the mat", "a cat is on the mat")
compare_strings(variants, rep(reference, length(variants)), bert = TRUE)

stop_llama_server()                              # shut it down when done

# 6. Package-level checks ------------------------------------------------------

# Run the test suite:
devtools::test()

# Test coverage (requires the {covr} package):
# covr::report(covr::package_coverage())

# Full R CMD check (build, examples, tests, vignettes, docs):
# devtools::check()

# Session / package info for debugging:
sessionInfo()
