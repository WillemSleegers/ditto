test_that("clean lowercases, strips punctuation, and squishes whitespace", {
  expect_equal(clean("To what extent do you AGREE?"), "to what extent do you agree")
  expect_equal(clean("  multiple   spaces  "), "multiple spaces")
})

test_that("clean is vectorized", {
  expect_equal(clean(c("A!", "B?")), c("a", "b"))
})

test_that("clean keeps decimal points between digits", {
  expect_equal(clean("6.2"), "6.2")
  expect_equal(clean("It rose 6.2 points in 2020."), "it rose 6.2 points in 2020")
  expect_equal(clean("Done. 5 left"), "done 5 left")
})

test_that("clean keeps decimal commas when decimal_mark is a comma", {
  expect_equal(clean("6,2", decimal_mark = ","), "6,2")
  expect_equal(clean("1.000,5", decimal_mark = ","), "1000,5")
  expect_equal(clean("1,000.5"), "1000.5")
  expect_error(clean("x", decimal_mark = ";"))
})

test_that("clean keeps percent signs that follow a number", {
  expect_equal(clean("6.2%"), "6.2%")
  expect_equal(clean("It rose 6.2 % in 2020."), "it rose 6.2% in 2020")
  expect_equal(clean("6,2 %", decimal_mark = ","), "6,2%")
  expect_equal(clean("% of people"), "of people")
})
