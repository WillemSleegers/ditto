# TERCOM's greedy shift search, used by ter(). Not exported.
#
# TER counts a contiguous block of words moved to a new position as a single
# edit. Finding the optimal set of such shifts is NP-complete (Shapira &
# Storer, 2002), so the original tool, TERCOM, applies a greedy heuristic:
# repeatedly take the single shift that reduces the edit distance the most,
# charge it one edit, and stop when no shift helps.
#
# This is a port of `sacrebleu`'s `lib_ter.py`, which is itself a careful
# reimplementation of TERCOM. Much of the filtering and ranking below is
# inherited heuristic rather than principled, but reproducing it is the whole
# point: it is what makes ditto's ter() comparable to the scores everyone else
# reports. Deviating anywhere would silently change the metric.

# Edit operations, as small integers rather than the single characters
# sacrebleu uses.
OP_UNDEF <- 0L
OP_NOP <- 1L
OP_SUB <- 2L
OP_INS <- 3L
OP_DEL <- 4L

MAX_SHIFT_SIZE <- 10L
MAX_SHIFT_DIST <- 50L
BEAM_WIDTH <- 25L
MAX_SHIFT_CANDIDATES <- 1000L

# Levenshtein distance between a fixed reference and a varying hypothesis,
# returning both the distance and the trace of edit operations that produced
# it. Only a band of cells around the matrix's pseudo-diagonal is explored,
# the "beam"; cells outside it stay at infinity so the trace cannot route
# through them. For sequences shorter than the beam width the band covers the
# whole matrix and the result is an exact Levenshtein distance.
#
# Returns a function of the hypothesis, memoised because the shift search
# scores the same word sequences repeatedly.
beam_edit_distance <- function(words_ref) {
  n_ref <- length(words_ref)
  memo <- new.env(parent = emptyenv(), hash = TRUE)

  function(words_hyp) {
    # The leading marker keeps an empty hypothesis from producing the empty
    # string, which is not a usable environment name.
    key <- paste0("\r", paste(words_hyp, collapse = "\r"))
    hit <- memo[[key]]
    if (!is.null(hit)) {
      return(hit)
    }

    n_hyp <- length(words_hyp)
    cost <- matrix(Inf, nrow = n_hyp + 1L, ncol = n_ref + 1L)
    op <- matrix(OP_UNDEF, nrow = n_hyp + 1L, ncol = n_ref + 1L)

    # Row 0: rewriting the reference away, one insertion per reference word.
    cost[1L, ] <- seq.int(0L, n_ref)
    op[1L, ] <- OP_INS

    length_ratio <- if (n_hyp > 0L) n_ref / n_hyp else 1
    beam_width <- if (BEAM_WIDTH < length_ratio / 2) {
      ceiling(length_ratio / 2 + BEAM_WIDTH)
    } else {
      BEAM_WIDTH
    }

    for (i in seq_len(n_hyp)) {
      pseudo_diag <- floor(i * length_ratio)
      min_j <- max(0, pseudo_diag - beam_width)
      max_j <- min(n_ref + 1, pseudo_diag + beam_width)
      # The final row must reach the last column, or there is no trace to read.
      if (i == n_hyp) {
        max_j <- n_ref + 1
      }
      if (min_j > max_j - 1) {
        next
      }

      for (j in min_j:(max_j - 1L)) {
        if (j == 0L) {
          cost[i + 1L, 1L] <- cost[i, 1L] + 1
          op[i + 1L, 1L] <- OP_DEL
          next
        }

        if (identical(words_hyp[i], words_ref[j])) {
          cost_sub <- 0
          op_sub <- OP_NOP
        } else {
          cost_sub <- 1
          op_sub <- OP_SUB
        }

        # TERCOM prefers no-op/substitution, then insertion, then deletion.
        # The trace is flipped before it is read, so insertion and deletion
        # swap places here. Ties go to whichever comes first, hence the strict
        # comparison.
        best_cost <- cost[i, j] + cost_sub
        best_op <- op_sub
        del_cost <- cost[i, j + 1L] + 1
        if (best_cost > del_cost) {
          best_cost <- del_cost
          best_op <- OP_DEL
        }
        ins_cost <- cost[i + 1L, j] + 1
        if (best_cost > ins_cost) {
          best_cost <- ins_cost
          best_op <- OP_INS
        }
        cost[i + 1L, j + 1L] <- best_cost
        op[i + 1L, j + 1L] <- best_op
      }
    }

    trace <- integer(n_hyp + n_ref)
    k <- length(trace)
    i <- n_hyp
    j <- n_ref
    while (i > 0L || j > 0L) {
      o <- op[i + 1L, j + 1L]
      trace[k] <- o
      k <- k - 1L
      if (o == OP_NOP || o == OP_SUB) {
        i <- i - 1L
        j <- j - 1L
      } else if (o == OP_INS) {
        j <- j - 1L
      } else if (o == OP_DEL) {
        i <- i - 1L
      } else {
        stop("unknown edit operation in trace", call. = FALSE)
      }
    }

    out <- list(
      distance = cost[n_hyp + 1L, n_ref + 1L],
      trace = trace[(k + 1L):length(trace)]
    )
    memo[[key]] <- out
    out
  }
}

# The distance rewrites the hypothesis into the reference; the alignment is
# read the other way round, so insertions and deletions swap.
flip_trace <- function(trace) {
  flipped <- trace
  flipped[trace == OP_INS] <- OP_DEL
  flipped[trace == OP_DEL] <- OP_INS
  flipped
}

# Turn a trace into an alignment: for each reference position, the hypothesis
# position it lands on, plus which positions on each side are in error.
# Positions are 0-based, as in TERCOM, and `align` may hold -1 for a reference
# word that precedes every hypothesis word.
trace_to_alignment <- function(trace, n_hyp, n_ref) {
  align <- integer(n_ref)
  hyp_err <- integer(n_hyp)
  ref_err <- integer(n_ref)

  pos_hyp <- -1L
  pos_ref <- -1L
  for (o in trace) {
    if (o == OP_NOP || o == OP_SUB) {
      pos_hyp <- pos_hyp + 1L
      pos_ref <- pos_ref + 1L
      align[pos_ref + 1L] <- pos_hyp
      err <- if (o == OP_SUB) 1L else 0L
      hyp_err[pos_hyp + 1L] <- err
      ref_err[pos_ref + 1L] <- err
    } else if (o == OP_INS) {
      pos_hyp <- pos_hyp + 1L
      hyp_err[pos_hyp + 1L] <- 1L
    } else if (o == OP_DEL) {
      pos_ref <- pos_ref + 1L
      align[pos_ref + 1L] <- pos_hyp
      ref_err[pos_ref + 1L] <- 1L
    }
  }

  list(align = align, ref_err = ref_err, hyp_err = hyp_err)
}

# Every pair of positions at which the hypothesis and the reference share a
# run of words, up to MAX_SHIFT_SIZE long and MAX_SHIFT_DIST apart. Returned
# 0-based, in the order TERCOM generates them, because the shift ranking below
# breaks ties on that order.
find_shifted_pairs <- function(words_h, words_r) {
  n_h <- length(words_h)
  n_r <- length(words_r)
  starts_h <- integer(0)
  starts_r <- integer(0)
  lengths <- integer(0)

  for (start_h in seq_len(n_h) - 1L) {
    for (start_r in seq_len(n_r) - 1L) {
      if (abs(start_r - start_h) > MAX_SHIFT_DIST) {
        next
      }
      len <- 0L
      while (identical(words_h[start_h + len + 1L], words_r[start_r + len + 1L]) &&
        len < MAX_SHIFT_SIZE) {
        len <- len + 1L
        starts_h <- c(starts_h, start_h)
        starts_r <- c(starts_r, start_r)
        lengths <- c(lengths, len)
        if (n_h == start_h + len || n_r == start_r + len) {
          break
        }
      }
    }
  }

  list(start_h = starts_h, start_r = starts_r, length = lengths)
}

# Move `length` words starting at `start` so that they begin at `target`.
# All indices are 0-based.
perform_shift <- function(words, start, length, target) {
  take <- function(from, to) {
    if (from >= to) character(0) else words[(from + 1L):to]
  }
  moved <- take(start, start + length)

  if (target < start) {
    c(take(0L, target), moved, take(target, start), take(start + length, base::length(words)))
  } else if (target > start + length) {
    c(take(0L, start), take(start + length, target), moved, take(target, base::length(words)))
  } else {
    c(
      take(0L, start), take(start + length, length + target), moved,
      take(length + target, base::length(words))
    )
  }
}

# TERCOM's shift ranking: greatest reduction in edit distance, then the
# longest match, then the earliest source position, then the earliest target
# position. The shifted words themselves are the final tiebreak, purely so
# that the choice is deterministic.
candidate_beats <- function(a, b) {
  for (k in 1:4) {
    if (a[[k]] != b[[k]]) {
      return(a[[k]] > b[[k]])
    }
  }
  cmp <- which(a[[5]] != b[[5]])
  if (length(cmp) == 0L) {
    return(FALSE)
  }
  a[[5]][cmp[1L]] > b[[5]][cmp[1L]]
}

# The single best shift of `words_h` towards `words_r`, or no shift if none
# reduces the edit distance.
best_shift <- function(words_h, words_r, edit_distance, checked_candidates) {
  scored <- edit_distance(words_h)
  pre_score <- scored$distance

  n_h <- length(words_h)
  n_r <- length(words_r)
  aligned <- trace_to_alignment(flip_trace(scored$trace), n_h, n_r)
  align <- aligned$align

  best <- NULL
  pairs <- find_shifted_pairs(words_h, words_r)

  for (p in seq_along(pairs$length)) {
    start_h <- pairs$start_h[p]
    start_r <- pairs$start_r[p]
    len <- pairs$length[p]

    # Only shift words the hypothesis got wrong, into a place the reference
    # does not already match, and never within the run being moved.
    if (sum(aligned$hyp_err[(start_h + 1L):(start_h + len)]) == 0L) next
    if (sum(aligned$ref_err[(start_r + 1L):(start_r + len)]) == 0L) next
    landing <- align[start_r + 1L]
    if (start_h <= landing && landing < start_h + len) next

    prev_idx <- -1L
    for (offset in -1L:(len - 1L)) {
      sr <- start_r + offset
      if (sr == -1L) {
        idx <- 0L # insert before the beginning
      } else if (sr <= n_r - 1L) {
        # Unlike TERCOM, which inserts after the index, insert before it.
        idx <- align[sr + 1L] + 1L
      } else {
        break # past the end of the reference
      }

      if (idx == prev_idx) next
      prev_idx <- idx

      shifted <- perform_shift(words_h, start_h, len, idx)
      candidate <- list(
        pre_score - edit_distance(shifted)$distance,
        len,
        -start_h,
        -idx,
        shifted
      )
      checked_candidates <- checked_candidates + 1L

      if (is.null(best) || candidate_beats(candidate, best)) {
        best <- candidate
      }
    }

    if (checked_candidates >= MAX_SHIFT_CANDIDATES) break
  }

  if (is.null(best)) {
    list(delta = 0, words = words_h, checked = checked_candidates)
  } else {
    list(delta = best[[1L]], words = best[[5L]], checked = checked_candidates)
  }
}

# Total edits (shifts plus insertions, deletions, and substitutions) needed to
# turn the hypothesis into the reference, and the reference length.
translation_edit_rate <- function(words_hyp, words_ref) {
  n_ref <- length(words_ref)
  if (n_ref == 0L) {
    return(list(edits = length(words_hyp), length = 0L))
  }

  edit_distance <- beam_edit_distance(words_ref)
  shifts <- 0L
  words <- words_hyp
  checked <- 0L

  repeat {
    found <- best_shift(words, words_ref, edit_distance, checked)
    checked <- found$checked
    # TERCOM abandons the search once it has evaluated this many candidates,
    # discarding the shift it just found.
    if (checked >= MAX_SHIFT_CANDIDATES) break
    if (found$delta <= 0) break
    shifts <- shifts + 1L
    words <- found$words
  }

  list(edits = shifts + edit_distance(words)$distance, length = n_ref)
}
