context("floatExpand")

test_that("floatExpand01", {
  x <- 0.125
  digits <- floatExpand01(x)
  expect_equal(x, sum(digits/2^seq_along(digits)))
  b <- 3
  digits <- c(1L, 0L, 1L)
  expect_identical(digits, floatExpand01(sum(digits/b^seq_along(digits)), base=b))
})

test_that("floatExpand", {
  b <- 2
  x <- 1.125
  expansion <- floatExpand(x, base=b)
  expect_equal(x, b^expansion$exponent * sum((expansion$digits)/b^seq_along(expansion$digits)))
  #
  b <- 3
  x <- 41
  expansion <- floatExpand(x, base=b)
  expect_equal(x, b^expansion$exponent * sum((expansion$digits)/b^seq_along(expansion$digits)))
})
