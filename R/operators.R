#' @title Concat/add infix operator.
#' @param x Left summand.
#' @param y Right summand.
#' @description Performs (possibly) a vectorized summation operation,
#'  which depends on the class of operators.
#'  Following methods are implemented:
#'  \code{character} + \code{character},
#'        1-to-1 vectorized, concatenation of strings.
#' Does the same as `%+%`.
#' @return Result of the aapropriate summation/concatenation.
#' @importFrom purrr map2_chr
#' @importFrom vctrs vec_ptype_common vec_recycle_common
#' @export
`%&%` <- function(x, y) {
    vec_cast_common(x = x, y = y, .to = character()) %->% c(x, y)
    vec_recycle_common(x = x, y = y) %->% c(x, y)
    paste0(x, y)
}