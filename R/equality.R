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

    # re-do
    #vec_cast_common_flatten(x, y) %->% c(x, y)

    vec_assert_numeric(x, arg = as_label(substitute(x)))
    vec_assert_numeric(y, arg = as_label(substitute(y)))

    # re-do
    #vctrs::vec_recycle_common(x, y) %->% c(x, y)

    comparator <- function(p, q) {
        if (is_na(p) || is_na(q))
            return(FALSE)

        if (is.infinite(p) || is.infinite(q))
            return(p == q)

        if (p == q)
            return(TRUE)

        p_abs <- abs(p)
        q_abs <- abs(q)
        diff <- abs(p - q)

        delta <- eps * .Machine$double.eps
        # According to IEEE-754 https://en.wikipedia.org/wiki/IEEE_754
        # -0 and 0 are equal, therefore p_abs and q_abs
        # cannot be 0 at the same time
        if (p_abs == 0 && q_abs == 0)
            abort("Should not happen", "primitiveR_should_not_happen")
        else if (p_abs == 0)
            fact <- q_abs
        else if (q_abs == 0)
            fact <- p_abs
        else
            fact <- min(cc(p_abs, q_abs))

        return(diff < fact * delta)
    }

    map2_lgl(x, y, comparator)
}

# Double-dispatched equality
`%==%` <- function(x, y) UseMethod("%==%")
`%==%.double` <- function(x, y) are_equal_f(x, y)
`%==%.default` <- function(x, y) UseMethod("%==%.default", y)
`%==%.default.default` <- function(x, y) x == y
`%==%.default.double` <- function(x, y) are_equal_f(x, y)

`%!=%` <- function(x, y)!(x %==% y)

`%===%` <- function(x, y) all(x %==% y)
`%!==%` <- function(x, y) any(x %!=% y)