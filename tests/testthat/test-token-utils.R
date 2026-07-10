test_that("tokenize_words splits punctuation from adjacent words", {
  expect_equal(tokenize_words("do you agree?"), c("do", "you", "agree", "?"))
  expect_equal(tokenize_words("a, b"), c("a", ",", "b"))
})

test_that("tokenize_words leaves plain words untouched", {
  expect_equal(tokenize_words("a b c d"), c("a", "b", "c", "d"))
})

test_that("tokenize_words yields no tokens for empty or blank strings", {
  # A split on "\\s+" would return "" here, an empty token that then matches
  # the empty token of another blank string.
  expect_equal(tokenize_words(""), character(0))
  expect_equal(tokenize_words("   "), character(0))
})

test_that("tokenize_words ignores surrounding and repeated whitespace", {
  expect_equal(tokenize_words("  cat  "), "cat")
  expect_equal(tokenize_words("a  b"), c("a", "b"))
})

test_that("tokenize_chars removes whitespace before splitting", {
  expect_equal(tokenize_chars("ab cd"), c("a", "b", "c", "d"))
  expect_equal(tokenize_chars(" a\tb "), c("a", "b"))
})

test_that("tokenize_chars yields no tokens for a blank string", {
  expect_equal(tokenize_chars(""), character(0))
  expect_equal(tokenize_chars("   "), character(0))
})

test_that("get_ngrams returns overlapping windows joined by sep", {
  expect_equal(get_ngrams(c("a", "b", "c"), 2), c("a b", "b c"))
  expect_equal(get_ngrams(c("a", "b", "c"), 2, sep = ""), c("ab", "bc"))
})

test_that("get_ngrams returns an empty vector when n exceeds the token count", {
  expect_equal(get_ngrams(c("a", "b"), 3), character(0))
})

test_that("ngram_match_count clips repeated matches to the reference count", {
  expect_equal(ngram_match_count(c("a", "a", "a"), c("a", "a")), 2)
})

test_that("encode_tokens maps equal tokens to equal symbols", {
  encoded <- encode_tokens(c("a", "b", "a"), c("b", "a"))
  expect_equal(nchar(encoded$candidate), 3)
  expect_equal(nchar(encoded$reference), 2)
  chars <- strsplit(paste0(encoded$candidate, encoded$reference), "")[[1]]
  expect_equal(length(unique(chars)), 2)
})

test_that("encode_tokens keeps symbols distinct past the Private Use Area", {
  # The PUA holds only 6400 code points, so a larger vocabulary numbers past
  # it. That is harmless -- the symbols only have to differ from each other --
  # but a collision would silently equate two different words.
  vocab <- as.character(seq_len(7000))
  encoded <- encode_tokens(vocab, vocab[1])
  expect_equal(length(unique(utf8ToInt(encoded$candidate))), 7000)
})

test_that("greedy_alignment matches every token type it can", {
  pairs <- greedy_alignment(c("a", "x", "b", "c"), c("a", "b", "y", "c"))
  expect_equal(nrow(pairs), 3)
  expect_true(all(diff(pairs[, "i"]) > 0))
})

test_that("greedy_alignment matches reordered tokens, which an LCS cannot", {
  # The longest common subsequence of ("a", "b") and ("b", "a") has length 1.
  # METEOR needs both words matched so that the fragmentation penalty, rather
  # than a shrunken match set, charges for the reordering.
  pairs <- greedy_alignment(c("a", "b"), c("b", "a"))
  expect_equal(nrow(pairs), 2)
  expect_equal(unname(pairs[, "j"]), c(2, 1))
})

test_that("greedy_alignment uses each reference token at most once", {
  pairs <- greedy_alignment(c("a", "a", "a"), c("a", "a"))
  expect_equal(nrow(pairs), 2)
  expect_equal(anyDuplicated(pairs[, "j"]), 0)
})

test_that("greedy_alignment breaks ties on repeated tokens the way nltk does", {
  # Scanning right to left and taking the latest free partner leaves the
  # *first* of the two "a"s unmatched, which keeps the remaining matches in one
  # contiguous run. A left-to-right scan would strand the second "a" instead.
  pairs <- greedy_alignment(c("a", "a", "b"), c("a", "b"))
  expect_equal(unname(pairs[, "i"]), c(2, 3))
  expect_equal(unname(pairs[, "j"]), c(1, 2))
  expect_equal(count_chunks(pairs), 1)
})

test_that("greedy_alignment returns pairs ordered by candidate position", {
  pairs <- greedy_alignment(c("b", "a", "c"), c("a", "b", "c"))
  expect_equal(unname(pairs[, "i"]), c(1, 2, 3))
  expect_false(is.unsorted(pairs[, "i"]))
})

test_that("greedy_alignment returns an empty matrix for disjoint sequences", {
  pairs <- greedy_alignment(c("a", "b"), c("x", "y"))
  expect_equal(nrow(pairs), 0)
})

test_that("count_chunks counts contiguous runs, not total matches", {
  contiguous <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 2, dimnames = list(NULL, c("i", "j")))
  expect_equal(count_chunks(contiguous), 1)

  scattered <- matrix(c(1, 3, 5, 1, 3, 5), ncol = 2, dimnames = list(NULL, c("i", "j")))
  expect_equal(count_chunks(scattered), 3)
})

test_that("count_chunks treats a crossing alignment as separate chunks", {
  crossing <- matrix(c(1, 2, 2, 1), ncol = 2, dimnames = list(NULL, c("i", "j")))
  expect_equal(count_chunks(crossing), 2)
})

test_that("check_string rejects anything but a single string", {
  expect_error(check_string(c("a", "b"), "candidate"), "single string")
  expect_error(check_string(character(0), "candidate"), "single string")
  expect_error(check_string(1, "candidate"), "single string")
  expect_silent(check_string(NA_character_, "candidate"))
})
