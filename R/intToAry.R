
#' Integer to ary-expansion
#' @description Ary-expansion of an integer.
#' @useDynLib expansions
#' @importFrom Rcpp evalCpp
#' @export
#' @param n integer to be expanded
#' @param sizes the base of the expansion
#' @return A vector of length \code{prod(sizes)}, the expansion of \code{n}.
intToAry <- function(n, sizes){
  if(n < 0 || n > prod(sizes)-1L) stop(sprintf("n must be nonnegative and strictly lower than %s (product of sizes)", prod(s)))
  return(rcpp_intToAry(n, sizes))
}
