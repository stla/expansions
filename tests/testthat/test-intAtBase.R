context("integer in base")

test_that("intAtBase", {
  x <- 41
  b <- 3
  digits <- intAtBase(x, base=b)
  expect_equal(x, sum(digits*b^(seq_along(digits)-1L)))
})

test_that("intAtBaseFib", {
  fib <- function(n){
    if(n==0){
      return(0L)
    }else if(n==1){
      return(1L)
    }else{
      return(fib(n-2)+fib(n-1))
    }
  }
  n <- 6
  b <- 3
  expect_true(all(intAtBaseFib(n, base=b) == intAtBase(fib(n), base=b)))
})

test_that("intAtBasePower", {
  b <- 3
  m <- 2
  p <- 5
  expect_true(all(intAtBasePower(m, p, base=b) == intAtBase(m^p, base=b)))
})
