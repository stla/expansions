.odometer <- function(x, base){
  .C("odomR", b=as.integer(base), t=as.integer(x), nt=length(x), result=list(0L))$result[[1]]
}
.odometer_iterated <- function(x, n, base){
  .C("odom_iterR", b=as.integer(base), t=as.integer(x), nt=length(x), n=as.integer(n), result=list(0L))$result[[1]]
}
.sumadic <- function(x1, x2, base){
  .C("sumadicR", b=as.integer(base), t1=as.integer(x1), nt1=length(x1), t2=as.integer(x2), nt2=length(x2), result=list(0L))$result[[1]]
}
.odometerBW <- function(x, base){
  .C("odomBackwardR", b=as.integer(base), t=as.integer(x), nt=length(x), result=list(0L))$result[[1]]
}
.odometerBW_iterated <- function(x, n, base){
  .C("odomBackward_iterR", b=as.integer(base), t=as.integer(x), nt=length(x), n=as.integer(n), result=list(0L))$result[[1]]
}

#' @name odometer
#' @rdname odometer
#' @title Odometer
#' @description The odometer in a given integer base
#' @param x a sequence of digits in the given base
#' @param n integer, power of the iteration
#' @param base integer, the base of the expansion
#' @return \code{odometer} returns the transformation of \code{x} by the odometer; \code{odometer_iterated} returns the \code{n}-th iterate of the odometer; \code{odometerBW} returns the transformation of \code{x} by the backward odometer; \code{odometerBW_iterated} returns the \code{n}-th iterate of the backward odometer.
#' @examples
#' odometer(c(0,1))
#' odometer(c(1,1))
#' odometerBW(odometer(c(0,1))) == c(0,1)
#' odometer_iterated(c(0,1), n=2)
#' odometer_iterated(0, n=13) == intAtBase(13)
#' odometerBW_iterated(intAtBase(13), n=13) == 0
NULL

#' @rdname odometer
#' @export
odometer <- function(x, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop(sprintf("x is not an expansion in base %s", as.integer(base)))
  return(.odometer(x, base))
}

#' @rdname odometer
#' @export
odometer_iterated <- function(x, n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop(sprintf("x is not an expansion in base %s", as.integer(base)))
  if(n<0) stop("n must be a nonnegative integer")
  return(.odometer_iterated(x, n, base))
}

#' @rdname odometer
#' @export
odometerBW <- function(x, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop(sprintf("x is not an expansion in base %s", as.integer(base)))
  if(all(x==0)) stop(sprintf("The backward image of 0 is the infinite sequence (%s,%s,...)", base, base))
  return(.odometerBW(x, base))
}

#' @rdname odometer
#' @export
odometerBW_iterated <- function(x, n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop(sprintf("x is not an expansion in base %s", as.integer(base)))
  if(n<0) stop("n must be a nonnegative integer")
  xToInt <- sum(x*base^(seq_along(x)-1L))
  if(xToInt < n) stop(sprintf("Cannot go beyond n=%s, because the result is 0 for n=%s.", xToInt, xToInt))
  return(.odometerBW_iterated(x, n, base))
}

#' @title Sum of two adic integers
#' @description Sum of two adic integers in a given base
#' @export
#' @param x1,x2 the two adic integers to be added
#' @param base the base of the two adic integers (integer)
#' @examples
#' sumadic(c(0,1), c(1,1))
#' sumadic(c(0,1), 1) == odometer(c(0,1))
sumadic <- function(x1, x2, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x1 %in% 0:(as.integer(base-1L)))) stop(sprintf("x1 is not an expansion in base %s", as.integer(base)))
  if(any(!x2 %in% 0:(as.integer(base-1L)))) stop(sprintf("x2 is not an expansion in base %s", as.integer(base)))
  return(.sumadic(x1, x2, base))
}
