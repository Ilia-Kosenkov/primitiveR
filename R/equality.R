#' @title equals
#' @param x LHS
#' @param y RHS
#' @param eps Precision of floating-point comparison
#'
#' @return Logical vector
#' @export
are_equal_f <- function(x, y, eps = 1) {
    eps <- vec_assert_numeric(eps, size = 1L)
    assert(eps >= 0)
    delta <- eps * .Machine$double.eps

    vec_cast_common(x, y, .to = double()) %->% c(x, y)

    vec_recycle_common(x, y, TRUE) %->% c(x, y, mask)


    na <- mask & (is.na(x) | is.na(y))
    mask <- mask & (!na)


    infs <- mask & ((is.infinite(x) | is.infinite(y)) & (x == y))
    mask <- mask & (!infs)


    raw_eq <- mask & (abs(x - y) < delta * sqrt(x ^ 2 + y ^ 2))

    return(raw_eq | infs)
}


# Double-dispatched equality
`%==%` <- function(x, y) UseMethod("%==%")

`%==%.double` <- function(x, y) are_equal_f(x, y)
`%==%.name` <- function(x, y) is_symbol(y) & x == y
`%==%.default` <- function(x, y) UseMethod("%==%.default", y)

`%==%.default.default` <- function(x, y) vec_equal(x, y)
`%==%.default.double` <- function(x, y) are_equal_f(x, y)
`%==%.default.name` <- function(x, y) is_symbol(x) & x == y

`%!=%` <- function(x, y)!(x %==% y)

`%===%` <- function(x, y) all(x %==% y)
`%!==%` <- function(x, y) any(x %!=% y)