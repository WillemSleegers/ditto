# text-metrics-reference.py ----------------------------------------------------
#
# Reference scores for validating ditto's bleu(), chrf(), rouge(), ter(), and
# meteor() against the Python implementations people actually check against.
# The BERTScore validation is separate (see bertscore-reference.py), because
# only that one needs a model server.
#
# Which reference is used for which metric, and why:
#
# bleu()   sacrebleu.BLEU        the machine-translation standard
# chrf()   sacrebleu.CHRF        the reference implementation of the metric
# rouge()  rouge_score           Google's implementation, used by the
#                                summarization literature
# ter()    sacrebleu.TER         includes TERCOM's greedy shift search, which
#                                ditto's ter() reproduces
# wer()    jiwer.wer             wer() is ter() without the shift search, so
#                                word error rate is its reference
# meteor() nltk                  the implementation most people run
#
# Every reference has to be configured to compute the same quantity ditto does.
# The settings that matter are flagged inline below.
#
# Setup
#
# 1. Create an isolated environment (Command Palette -> "Python: Create
#    Environment" -> Venv), then:
# pip install sacrebleu rouge-score jiwer nltk
#
# 2. Run from the package root, the same convention as dev/try-ditto.R:
# python dev/validation/text-metrics-reference.py
#
# It writes text_metrics_reference_scores.csv next to this script. Then run
# text-metrics-ditto.R to score the same pairs with ditto and diff the two.

import csv
import re
from pathlib import Path

import jiwer
from nltk.stem.snowball import SnowballStemmer
from nltk.translate.meteor_score import meteor_score
from rouge_score import rouge_scorer
from sacrebleu.metrics import BLEU, CHRF, TER

# 1. Tokenization. ditto's tokenize_words() splits on whitespace and peels
# punctuation off into its own token. Every reference below is either fed
# text already tokenized this way, or told to use this tokenizer, so that no
# difference can be blamed on tokenization.
TOKEN = re.compile(r"\w+|[^\w\s]", re.UNICODE)


def tokenize(text):
    return TOKEN.findall(text)


def pretokenized(text):
    return " ".join(tokenize(text))


class DittoTokenizer:
    def tokenize(self, text):
        return TOKEN.findall(text)


class NoWordnet:
    """WordNet stand-in that never finds a synonym."""

    def synsets(self, word):
        return []


# 2. BLEU: no smoothing (ditto applies none), and effective_order, which stops
# at the highest n-gram order the candidate can actually form -- ditto's
# `max_n` cap. tokenize="none" makes sacrebleu split the pre-tokenized text on
# whitespace instead of applying its own tokenizer.
bleu = BLEU(effective_order=True, smooth_method="none", tokenize="none")

# 3. CHRF: char_order 6 and beta 2 are the paper's defaults and ditto's;
# word_order 0 selects plain CHRF rather than chrF++; whitespace=False removes
# whitespace before extracting character n-grams; no epsilon smoothing.
# CHRF is fed the raw strings, since it does its own whitespace handling.
chrf = CHRF(char_order=6, beta=2, word_order=0, whitespace=False, eps_smoothing=False)

# 4. ROUGE: no stemmer (ditto's rouge() does not stem), and ditto's tokenizer.
# rouge_score reports an F-measure, which is ditto's default beta = 1.
rouge = rouge_scorer.RougeScorer(
    ["rouge1", "rouge2", "rougeL"], use_stemmer=False, tokenizer=DittoTokenizer()
)

# 5. TER: case_sensitive=True, because ditto compares the text as given while
# sacrebleu lowercases by default. With normalized=False and no_punct=False the
# tokenizer then only collapses whitespace, so the pre-tokenized text passes
# through untouched.
ter = TER(case_sensitive=True)

# 6. METEOR: the Snowball English stemmer (the stemmer behind
# SnowballC::wordStem, not nltk's default Porter), a stubbed WordNet to disable
# the synonym stage ditto omits, and alpha/beta/gamma equal to ditto's
# f_mean = 10PR / (R + 9P) and penalty = 0.5 * (chunks / matches) ^ 3.
stemmer = SnowballStemmer("english")


def meteor(candidate, reference):
    hyp, ref = tokenize(candidate.lower()), tokenize(reference.lower())
    if not hyp or not ref:
        return 0.0
    return meteor_score(
        [ref], hyp, stemmer=stemmer, wordnet=NoWordnet(), alpha=0.9, beta=3.0, gamma=0.5
    )


# Pairs chosen to exercise what is easy to get wrong: reordering and moved
# blocks (which TER's shift search must find), repeated words (whose
# tie-breaking decides METEOR's chunk count), stem-only matches, punctuation,
# whitespace, and candidates shorter or longer than the reference.
PAIRS = [
    ("b a c d", "a b c d"),
    ("d e f a b c", "a b c d e f"),
    ("the cat sat on the mat", "the cat sat on the mat"),
    ("the cat sat on the mat", "a cat was sitting on the mat"),
    ("the cat sat on the mat", "the cat was sat on the mat"),
    ("how much do you agree with the statement", "to what extent do you agree with the statement"),
    ("the quick brown fox jumps over the lazy dog", "a quick brown dog leaps over a lazy fox"),
    ("it is a guide to action which ensures that the military always obeys the commands",
     "it is a guide to action that ensures that the military will forever heed commands"),
    ("she walked slowly to the store", "slowly she walked to the shop"),
    ("the children are playing in garden", "the children are playing in the garden"),
    ("i went the market to buy bread", "i went to the market to buy bread"),
    ("climate climate change affects cities", "climate change affects cities"),
    ("running runs runner", "run running ran"),
    ("the cats are agreeing", "the cat agreed"),
    ("one two three four five", "five four three two one"),
    ("do you agree with this statement.", "do you agree with this statement?"),
    ("  the cat sat on the mat  ", "the cat sat on the mat"),
    ("the  cat  sat on the mat", "the cat sat on the mat"),
    ("a b c d e f", "w x y z p q"),
    ("the president spoke to the audience", "the president addressed the crowd"),
]

out = Path(__file__).parent / "text_metrics_reference_scores.csv"
with out.open("w", newline="", encoding="utf-8") as fh:
    # csv.writer defaults to CRLF line endings. The repo stores CSVs with LF
    # (see .gitattributes), so ask for LF rather than let git rewrite the file.
    writer = csv.writer(fh, lineterminator="\n")
    writer.writerow(
        ["candidate", "reference", "bleu", "chrf", "rouge_1", "rouge_2", "rouge_l",
         "ter", "wer", "meteor"]
    )
    for candidate, reference in PAIRS:
        cand_tok, ref_tok = pretokenized(candidate), pretokenized(reference)
        scores = rouge.score(target=reference, prediction=candidate)
        writer.writerow([
            candidate,
            reference,
            f"{bleu.sentence_score(cand_tok, [ref_tok]).score / 100:.10f}",
            f"{chrf.sentence_score(candidate, [reference]).score / 100:.10f}",
            f"{scores['rouge1'].fmeasure:.10f}",
            f"{scores['rouge2'].fmeasure:.10f}",
            f"{scores['rougeL'].fmeasure:.10f}",
            f"{ter.sentence_score(cand_tok, [ref_tok]).score / 100:.10f}",
            f"{jiwer.wer(ref_tok, cand_tok):.10f}",
            f"{meteor(candidate, reference):.10f}",
        ])

print(f"wrote {out}")
