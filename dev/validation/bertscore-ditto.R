# bertscore-ditto.R ------------------------------------------------------------
#
# ditto's own BERTScore for the validation pairs, the R counterpart to
# bertscore-reference.py. It runs the same pairs through ditto::bertscore()
# and writes ditto_scores.csv, which is diffed against reference_scores.csv
# (see "What has to match" in vignettes/bertscore-validation.Rmd).
#
# Setup
#
# Needs a local llama.cpp embedding server. The start_llama_server() helper
# launches it for you; point it at your llama-server binary and a .gguf
# embedding model. bge-m3 is the model the reference script loads via
# `model_type="BAAI/bge-m3"`, so use the matching .gguf here.
#
# Run this from the package root (working directory = repo root), the same
# convention as dev/try-ditto.R.

# The five settings that have to match the reference (see "What has to match"
# in the vignette) are flagged inline below.

devtools::load_all(".")

llama_exe   <- "C:/Users/wslee/tools/llama.cpp/llama-server.exe"
llama_model <- "C:/Users/wslee/tools/llama.cpp/models/bge-m3-f16.gguf"  # 1. same model
start_llama_server(model = llama_model, exe = llama_exe)  # waits until ready

# Same pairs as the reference script: near-identical, paraphrase, related,
# unrelated. 5. identical input strings: the same raw text goes to both tools.
candidates <- c(
  "to what extent do you agree with the statement",
  "how much do you agree with the statement",
  "how satisfied are you with the service",
  "what is your highest level of education"
)
references <- rep("to what extent do you agree with the statement", 4)

# bertscore() reads the final layer from llama.cpp (2. layer), is unweighted
# (3. no IDF), and returns raw scores with no baseline (4. no rescaling) --
# matching idf=False, rescale_with_baseline=False, num_layers=24 on the
# reference side. One pair at a time, mapped over the vectors.
ditto_scores <- as.data.frame(
  t(mapply(bertscore, candidates, references, USE.NAMES = FALSE))
)

print(round(ditto_scores, 5))

out_path <- file.path("dev", "validation", "ditto_scores.csv")
write.csv(ditto_scores, out_path, row.names = FALSE)
cat("\nWrote", out_path, "\n")

stop_llama_server()  # shut it down when done
