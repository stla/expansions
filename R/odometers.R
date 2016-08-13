.odometer <- function(x, base){
  .C("odomR", b=as.integer(base), t=as.integer(x), nt=length(x), result=list(0L))$result[[1]]
}
.odometer_iterated <- function(x, n, base){
  .C("odom_iterR", b=as.integer(base), t=as.integer(x), nt=length(x), n=as.integer(n), result=list(0L))$result[[1]]
}
.sumadic <- function(x1, x2, base){
  .C("sumadicR", b=as.integer(base), t1=as.integer(x1), nt1=length(x1), t2=as.integer(x2), nt2=length(x2), result=list(0L))$result[[1]]
}

#' @name odometer
#' @rdname odometer
#' @title Odometer
#' @description The odometer in a given integer base
#' @param x a sequence of digits in the given base
#' @param base the base of the expansion (integer)
#' @return \code{odometer} returns the transformation of \code{x} by the odometer; \code{odometer_iterated} returns the \code{n}-th iterate of the odometer.
#' @examples
#' odometer(c(0,1))
#' odometer(c(1,1))
#' odometer_iterated(c(0,1), n=2)
NULL

#' @rdname odometer
#' @export
odometer <- function(x, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop("x is not an expansion in base %s", as.integer(base))
  return(.odometer(x, base))
}

#' @rdname odometer
#' @export
odometer_iterated <- function(x, n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x %in% 0:(as.integer(base-1L)))) stop("x is not an expansion in base %s", as.integer(base))
  if(n<0) stop("n must be a nonnegative integer")
  return(.odometer_iterated(x, n, base))
}

#' @title Sum of two adic integers
#' @description Sum of two adic integers in a given base
#' @export
#' @examples
#' sumadic(c(0,1), c(1,1))
#' sumadic(c(0,1), 1)
#' odometer(c(0,1))
sumadic <- function(x1, x2, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(any(!x1 %in% 0:(as.integer(base-1L)))) stop("x is not an expansion in base %s", as.integer(base))
  if(any(!x2 %in% 0:(as.integer(base-1L)))) stop("x is not an expansion in base %s", as.integer(base))
  return(.sumadic(x1, x2, base))
}
