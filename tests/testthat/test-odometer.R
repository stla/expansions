context("odometer")

test_that("odometer", {
  expect_equal(odometer(c(0,1)), c(1,1))
  expect_equal(odometer(c(1,1)), c(0,0,1))
  expect_equal(odometer_iterated(c(0,1), n=2), c(0,0,1))
  x <- c(0,1,0,1)
  expect_equal(x, odometerBW(odometer(x)))
  n <- 13
  b <- 3
  expect_equal(odometer_iterated(0, n=n, base = b), intAtBase(n, base = b))
  expect_true(all(odometerBW_iterated(intAtBase(n), n=n) == 0))
})
