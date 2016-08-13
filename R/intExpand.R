.intAtBase <- function(n, base){
  .C("intAtBaseR", b=as.integer(base), n=as.integer(n), result=list(0L))$result[[1]]
}

#' Integer expansion
#' @description Expansion of a positive integer in a given base
#' @export
#' @param n the integer to be expanded
#' @param base the base of the expansion
#' @return the expansion of \code{n} in a vector
#' @examples
#' intAtBase(41, base=3)
#' 2*1 + 1*3 + 1*3^2 + 1*3^3
#' floatExpand(41, base=3)
#' (1*1/3 + 1*1/3^2 + 1*1/3^3 + 2*1/3^4) * 3^4
intAtBase <- function(n, base=2L){
  if(base<2) stop("base must be an integer >1")
  if(n<0) stop("n must be a nonnegative integer")
  return(.intAtBase(n, base))
}
