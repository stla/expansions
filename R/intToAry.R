
#' Integer to ary-expansion
#' @description Ary-expansion of an integer.
#' @useDynLib expansions
#' @importFrom Rcpp evalCpp
#' @export
#' @param n integer to be expanded
#' @param sizes the base of the expansion
#' @return A vector of the same length as \code{prod(sizes)}, the expansion of \code{n}.
#' @examples
#' intToAry(77, c(3,4,7))
#' 2*1 + 1*3 + 6*(3*4)
#' # Cartesian product {0,1}x{0,1}:
#' sapply(0:3, function(x) intToAry(x, sizes=c(2,2)))
intToAry <- function(n, sizes){
  if(n < 0 || n > prod(sizes)-1L) stop(sprintf("n must be nonnegative and strictly lower than %s (product of sizes)", prod(sizes)))
  return(rcpp_intToAry(n, sizes))
}
