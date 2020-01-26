#' @title Deconstruction operators
#' @rdname deconst
#' @description Mimicks \code{zeallot}'s beahviour
#' @param lhs,rhs Left- and right-hand side of the operator
#'
#' @return Data (invisibly)
#' @export
`%->%` <- function(lhs, rhs) {
    deconstruct(lhs, {{ rhs }})
}

#' @rdname deconst
#' @export
`%<-%` <- function(lhs, rhs) {
    deconstruct(rhs, {{ lhs }})
}

deconstruct <- function(what, into) {
    q <- enquo(into)
    env <- quo_get_env(q)
    expr <- as.list(quo_get_expr(q))

    assert(expr[[1]] == sym("c"), "Only `c` can be used to combine names")
    names <- expr[-1]

    # `length` instead of `vec_size` to be consistent with mapping
    assert(length(what) == length(names), msg = "LHS and RHS should have equal length")

    invisible(walk2(what, names, ~ assign(as.character(.y), .x, envir = env)))
}