#' @title equals
#' @param x LHS
#' @param y RHS
#' @param eps Precision of floating-point comparison
#'
#' @return Logical vector
#' @export
are_equal_f <- function(x, y, eps = 1) {
    vec_assert(eps, size = 1L)
    vec_cast(eps, double()) -> eps
    vec_recycle_common(vec_cast(x, double()), vec_cast(y, double())) -> tmp

    are_equal_f_(tmp[[1]], tmp[[2]], .Machine$double.eps * eps)
}

replace_na <- function(x, val = FALSE) {
    x[is.na(x)] <- val
    x
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
`%==%.double` <- function(x, y) replace_na(are_equal_f(x, y))
#' @rdname equality
#' @export
`%==%.name` <- function(x, y) replace_na(is_symbol(y) & x == y)
#' @rdname equality
#' @method %==% default
#' @export
`%==%.default` <- function(x, y) UseMethod("%==%.default", y)

#' @rdname equality
#' @method %==%.default default
#' @export
`%==%.default.default` <- function(x, y) replace_na(vec_equal(x, y))
#' @rdname equality
#' @method %==%.default double
#' @export
`%==%.default.double` <- function(x, y) replace_na(are_equal_f(x, y))
#' @rdname equality
#' @method %==%.default name
#' @export
`%==%.default.name` <- function(x, y) replace_na(is_symbol(x) & x == y)

#' @rdname equality
#' @export
`%!=%` <- function(x, y)!(x %==% y)

#' @rdname equality
#' @export
`%===%` <- function(x, y) all(x %==% y)
#' @rdname equality
#' @export
`%!==%` <- function(x, y) any(x %!=% y)


are_equal_f_ <- function(x, y, eps) {
    .Call("primR_are_equal_f", x, y, eps)
}