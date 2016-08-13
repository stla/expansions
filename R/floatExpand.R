.floatExpand01 <- function(x, base){
  .C("floatExpansion01R", base=as.integer(base), x=as.double(x), result=list(0L))$result[[1]]
}
.floatExpand <- function(x, base){
  .C("floatExpansionR", base=as.integer(base), x=as.double(x), result=list(0L, 0L))$result
}

#' Float expansion between 0 and 1
#' @description Expansion of a number between 0 and 1 in a given integer base.
#' @param x the number to be expanded
#' @param base the base of the expansion (integer)
#' @return The digits of the expansion in a vector.
#' @export
#' @useDynLib ExpansionsR
#' @examples
#' floatExpand01(0.125)
#' floatExpand01(1/3+1/27, base=3)
floatExpand01 <- function(x, base=2){
  if(x<0 || x>1) stop("x must be between 0 and 1")
  return(.floatExpand01(x, base))
}

#' Float expansion
#' @description Expansion of a real number in a given integer base.
#' @param x the number to be expanded
#' @param base the base of the expansion (integer)
#' @return A list representing the scientific notation of the expansion; the digits in the first component and the exponent in the second one.
#' @export
#' @examples
#' floatExpand(1.125)
#' (1*1/2 + 0*1/2^2 + 1*1/2^3) * 2^1
floatExpand <- function(x, base=2){
  if(base<2) stop("base must be >1")
  return(.floatExpand(x, base))
}

