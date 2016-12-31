context("sumadic")

test_that("sumadic", {
  expect_equal(sumadic(c(0,1), c(1,1)), c(1,0,1))
  b <- 5
  d <- c(0,1)
  expect_equal(sumadic(d, 1, base=b), odometer(d, base=b))
  n <- b-1
  expect_equal(sumadic(d, n, base=b), odometer_iterated(d, n=n, base=b))
})
