test_that("clean lowercases, strips punctuation, and squishes whitespace", {
  expect_equal(clean("To what extent do you AGREE?"), "to what extent do you agree")
  expect_equal(clean("  multiple   spaces  "), "multiple spaces")
})

test_that("clean is vectorized", {
  expect_equal(clean(c("A!", "B?")), c("a", "b"))
})
