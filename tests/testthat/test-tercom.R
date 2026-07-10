test_that("beam_edit_distance reproduces plain Levenshtein on short sequences", {
  # The beam is 25 wide, so it cannot prune anything here; the result must be
  # the exact edit distance.
  ed <- beam_edit_distance(c("a", "b", "c", "d"))
  expect_equal(ed(c("a", "b", "c", "d"))$distance, 0)
  expect_equal(ed(c("a", "x", "c", "d"))$distance, 1)
  expect_equal(ed(c("a", "b", "c"))$distance, 1)
  expect_equal(ed(c("w", "x", "y", "z"))$distance, 4)
  expect_equal(ed(character(0))$distance, 4)
})

test_that("beam_edit_distance returns a trace that spends both sequences", {
  ed <- beam_edit_distance(c("a", "b", "c"))
  trace <- ed(c("a", "x", "c"))$trace

  # Each operation consumes a hypothesis word, a reference word, or both.
  hyp_consumed <- sum(trace %in% c(OP_NOP, OP_SUB, OP_DEL))
  ref_consumed <- sum(trace %in% c(OP_NOP, OP_SUB, OP_INS))
  expect_equal(hyp_consumed, 3)
  expect_equal(ref_consumed, 3)
})

test_that("beam_edit_distance memoises repeated hypotheses", {
  ed <- beam_edit_distance(c("a", "b"))
  expect_identical(ed(c("a", "x")), ed(c("a", "x")))
})

test_that("flip_trace swaps insertions and deletions only", {
  trace <- c(OP_NOP, OP_SUB, OP_INS, OP_DEL)
  expect_equal(flip_trace(trace), c(OP_NOP, OP_SUB, OP_DEL, OP_INS))
})

test_that("trace_to_alignment maps reference positions onto hypothesis positions", {
  # A clean match: every reference word lands on the hypothesis word beneath it.
  aligned <- trace_to_alignment(c(OP_NOP, OP_NOP, OP_NOP), n_hyp = 3, n_ref = 3)
  expect_equal(aligned$align, c(0L, 1L, 2L))
  expect_equal(aligned$hyp_err, c(0L, 0L, 0L))
  expect_equal(aligned$ref_err, c(0L, 0L, 0L))
})

test_that("trace_to_alignment marks substitutions on both sides", {
  aligned <- trace_to_alignment(c(OP_NOP, OP_SUB, OP_NOP), n_hyp = 3, n_ref = 3)
  expect_equal(aligned$hyp_err, c(0L, 1L, 0L))
  expect_equal(aligned$ref_err, c(0L, 1L, 0L))
})

test_that("trace_to_alignment gives every reference position a landing spot", {
  # A deletion consumes a reference word without a hypothesis word, but the
  # shift search still needs somewhere to insert relative to it.
  aligned <- trace_to_alignment(c(OP_DEL, OP_NOP), n_hyp = 1, n_ref = 2)
  expect_equal(length(aligned$align), 2)
  expect_false(anyNA(aligned$align))
})

test_that("find_shifted_pairs finds every shared run, shortest first", {
  pairs <- find_shifted_pairs(c("a", "b"), c("a", "b"))
  found <- Map(c, pairs$start_h, pairs$start_r, pairs$length)
  # (0,0,1) "a", (0,0,2) "a b", (1,1,1) "b"
  expect_true(list(c(0L, 0L, 1L)) %in% list(found[[1]]))
  expect_equal(found[[2]], c(0L, 0L, 2L))
  expect_equal(found[[length(found)]], c(1L, 1L, 1L))
})

test_that("find_shifted_pairs caps run length at MAX_SHIFT_SIZE", {
  words <- rep("a", 15)
  pairs <- find_shifted_pairs(words, words)
  expect_equal(max(pairs$length), MAX_SHIFT_SIZE)
})

test_that("perform_shift moves a block backwards, forwards, and in place", {
  w <- c("a", "b", "c", "d", "e")
  # Move "c d" (start 2, length 2) to the front.
  expect_equal(perform_shift(w, 2L, 2L, 0L), c("c", "d", "a", "b", "e"))
  # Move "a b" (start 0, length 2) to sit after "c d".
  expect_equal(perform_shift(w, 0L, 2L, 4L), c("c", "d", "a", "b", "e"))
  # A target inside the moved block leaves the words where they are.
  expect_equal(perform_shift(w, 1L, 2L, 1L), w)
})

test_that("perform_shift preserves length and multiset of words", {
  w <- c("a", "b", "c", "d", "e", "f")
  shifted <- perform_shift(w, 1L, 3L, 5L)
  expect_equal(length(shifted), length(w))
  expect_equal(sort(shifted), sort(w))
})

test_that("translation_edit_rate charges one edit for a moved block", {
  result <- translation_edit_rate(c("d", "e", "f", "a", "b", "c"), c("a", "b", "c", "d", "e", "f"))
  expect_equal(result$edits, 1)
  expect_equal(result$length, 6)
})

test_that("translation_edit_rate falls back to plain edits when no shift helps", {
  result <- translation_edit_rate(c("a", "x", "c"), c("a", "b", "c"))
  expect_equal(result$edits, 1)
  expect_equal(result$length, 3)
})

test_that("translation_edit_rate treats an empty reference as zero length", {
  result <- translation_edit_rate(c("a", "b"), character(0))
  expect_equal(result$edits, 2)
  expect_equal(result$length, 0)
})

test_that("candidate_beats ranks by score, then length, then earliest positions", {
  base <- list(1, 2, -3, -4, c("a"))
  expect_true(candidate_beats(list(2, 2, -3, -4, c("a")), base)) # higher score
  expect_true(candidate_beats(list(1, 3, -3, -4, c("a")), base)) # longer match
  expect_true(candidate_beats(list(1, 2, -2, -4, c("a")), base)) # earlier source
  expect_true(candidate_beats(list(1, 2, -3, -3, c("a")), base)) # earlier target
  expect_false(candidate_beats(base, base))
})
