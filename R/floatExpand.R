.floatExpand01 <- function(x, base){
  .C("floatExpansion01R", base=as.integer(base), x=as.double(x), result=list(0L))$result[[1]]
}
.floatExpand <- function(x, base){
  .C("floatExpansionR", base=as.integer(base), x=as.double(x), result=list(0L, 0L))$result
}

#' Float expansion between 0 and 1
#' @description Expansion of a number between 0 and 1 in a given integer base.
#' @param u real number between 0 and 1
#' @param base the base of the expansion (integer)
#' @return The digits of the expansion of \code{u} in a vector.
#' @export
#' @useDynLib ExpansionsR
#' @seealso \code{\link{floatExpand}} to expand a positive number.
#' @examples
#' floatExpand01(0.125)
#' 0*1/2 + 0*1/2^2 + 1*1/2^3
#' floatExpand01(1/3+1/3^3, base=3)
floatExpand01 <- function(u, base=2){
  if(u<0 || u>1) stop("u must be between 0 and 1")
  return(.floatExpand01(u, base))
}

#' Float expansion
#' @description Expansion of a positive real number in a given integer base.
#' @param x a nonnegative number
#' @param base the base of the expansion (integer)
#' @return A list representing the scientific notation of the expansion; the digits in the first component and the exponent in the second one.
#' @export
#' @importFrom stats setNames
#' @seealso \code{\link{floatExpand01}} to expand a number between 0 and 1.
#' @examples
#' floatExpand(1.125)
#' (1*1/2 + 0*1/2^2 + 0*1/2^3 + + 1*1/2^4) * 2^1
floatExpand <- function(x, base=2){
  if(base<2) stop("base must be >1")
  if(x<0) stop("x must be a nonnegative number")
  return(setNames(.floatExpand(x, base), c("digits", "exponent")))
}

