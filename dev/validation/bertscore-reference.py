# bertscore-reference.py -------------------------------------------------------
#
# Reference scores for validating ditto's bertscore() against the original
# `bert-score` package, as described in vignettes/bertscore-validation.Rmd.
#
# Positron setup
# 1. Create an isolated environment so bert-score's torch/transformers deps
# don't touch the system Python:
# Command Palette -> "Python: Create Environment" -> Venv, and pick
# a Python 3.x interpreter.
#
# 2. Install bert-score into it:
# pip install bert-score

import csv
from pathlib import Path

from bert_score import score

# Same pairs as the vignette: near-identical, paraphrase, related, unrelated.
candidates = [
    "to what extent do you agree with the statement",
    "how much do you agree with the statement",
    "how satisfied are you with the service",
    "what is your highest level of education",
]
references = ["to what extent do you agree with the statement"] * 4

P, R, F = score(
    candidates,
    references,
    model_type="BAAI/bge-m3",  # 1. same model ditto's llama.cpp server loads
    num_layers=24,  # 2. final layer, to match llama.cpp /embeddings
    idf=False,  # 3. unweighted, to match bertscore()
    rescale_with_baseline=False,  # 4. raw scores, to match bertscore()
)
# 5. identical input strings: the same raw text goes to both tools, uncleaned.

print(f"{'precision':>10} {'recall':>10} {'f1':>10}")
for p, r, fl in zip(P.tolist(), R.tolist(), F.tolist()):
    print(f"{p:>10.5f} {r:>10.5f} {fl:>10.5f}")

out_path = Path(__file__).resolve().parent / "reference_scores.csv"
with open(out_path, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["precision", "recall", "f1"])
    for p, r, fl in zip(P.tolist(), R.tolist(), F.tolist()):
        w.writerow([p, r, fl])

print(f"\nWrote {out_path}")
