#' @title equals
#' @param x LHS
#' @param y RHS
#' @param eps Precision of floating-point comparison
#'
#' @return Logical vector
#' @export
are_equal_f <- function(x, y, eps = 1) {
    vec_assert(eps, size = 1L)
    eps <- vec_cast(double())
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

#' @title Equality operators
#' @rdname equality
#' @param x Lhs.
#' @param y Rhs.
#'
#' @return Either a logical vector of the same as input size, or a single logical value.
#' @export
`%==%` <- function(x, y) UseMethod("%==%")

#' @rdname equality
#' @export
`%==%.double` <- function(x, y) are_equal_f(x, y)
#' @rdname equality
#' @export
`%==%.name` <- function(x, y) is_symbol(y) & x == y
#' @rdname equality
#' @method %==% default
#' @export
`%==%.default` <- function(x, y) UseMethod("%==%.default", y)

#' @rdname equality
#' @method %==%.default default
#' @export
`%==%.default.default` <- function(x, y) vec_equal(x, y)
#' @rdname equality
#' @method %==%.default double
#' @export
`%==%.default.double` <- function(x, y) are_equal_f(x, y)
#' @rdname equality
#' @method %==%.default name
#' @export
`%==%.default.name` <- function(x, y) is_symbol(x) & x == y

#' @rdname equality
#' @export
`%!=%` <- function(x, y)!(x %==% y)

#' @rdname equality
#' @export
`%===%` <- function(x, y) all(x %==% y)
#' @rdname equality
#' @export
`%!==%` <- function(x, y) any(x %!=% y)