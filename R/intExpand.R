.intAtBase <- function(n, base){
  .C("intAtBaseR", b=as.integer(base), n=as.integer(n), result=list(0L))$result[[1]]
}
.intAtBasePower <- function(m, p, base){
  .C("intAtBasePowerR", b=as.integer(base), m=as.integer(m), p=as.integer(p), result=list(0L))$result[[1]]
}
.intAtBaseFib <- function(n, base){
  .C("intAtBaseFibR", b=as.integer(base), n=as.integer(n), result=list(0L))$result[[1]]
}

#' Integer expansion
#' @description Expansion of a positive integer in an integer base
#' @export
#' @param n the integer to be expanded
#' @param base the base of the expansion
#' @return The expansion of \code{n} in a vector.
#' @examples
#' intAtBase(41, base=3)
#' 2*1 + 1*3 + 1*3^2 + 1*3^3
#' floatExpand(41, base=3)
#' (1*1/3 + 1*1/3^2 + 1*1/3^3 + 2*1/3^4) * 3^4
intAtBase <- function(n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(n<0) stop("n must be a nonnegative integer")
  if(n > 2^31-1) stop("n is too big")
  return(.intAtBase(n, base))
}

#' Integer expansion of a power
#' @description Expansion of \code{m^p} in an integer base
#' @export
#' @param m the integer to be exponentiated
#' @param p the integer exponent
#' @param base the base of the expansion
#' @return The expansion of \code{m^p} in a vector.
#' @examples
#' intAtBasePower(2, 5, base=3) == intAtBase(2^5, base=3)
intAtBasePower <- function(m, p, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(m<0 || p <0) stop("m and p must be nonnegative integers")
  return(.intAtBasePower(m, p, base))
}

#' Integer expansion of a Fibonacci number
#' @description Expansion of a Fibonacci number in an integer base
#' @export
#' @param n the index of the Fibonacci number to be expanded
#' @param base the base of the expansion
#' @return The expansion of the \code{n}-th Fibonacci number in a vector.
#' @examples
#' intAtBaseFib(6, base=3) == intAtBase(8, base=3)
intAtBaseFib <- function(n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(n<0) stop("n must be a nonnegative integer")
  return(.intAtBaseFib(n, base))
}
