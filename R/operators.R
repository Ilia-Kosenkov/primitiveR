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

#' @title \code{is} interfix operator
#' @param object Object to test.
#' @param class Target type (supports \code{rlang} quosure).
#' @description Works atop of \code{vctrs}
#' @return \code{logical} \code{TRUE} if
#' \code{object} is of class \code{class}, \code{FALSE} otherwise.
#' @importFrom rlang quo_squash enquo sym exec
#' @importFrom vctrs vec_ptype vec_is
#' @export
`%is%` <- function(object, class) {
    class <- sym(quo_squash(enquo(class)))
    ptype <- vec_ptype(exec(class))

    vec_is(object, ptype)
}


`%win%` <- function(lhs, rhs) {
    vec_assert(rhs, size = 2L)
    vec_cast_common(lhs, rhs) %->% c(lhs, rhs)
    lhs > rhs[1] & lhs < rhs[2]
}

`%wini%` <- function(lhs, rhs) {
    vec_assert(rhs, size = 2L)
    vec_cast_common(lhs, rhs) %->% c(lhs, rhs)
    lhs >= rhs[1] & lhs <= rhs[2]
}