context("ary expansion")

test_that("intToAry", {
  n <- 77
  sizes <- c(3,4,7)
  digits <- intToAry(n, sizes)
  expect_equal(n, sum(digits*cumprod(c(1, head(sizes,-1)))))
  #
  sizes <- c(3,4,5)
  expect_error(intToAry(prod(sizes), sizes), "n must be nonnegative and strictly lower than")
})
