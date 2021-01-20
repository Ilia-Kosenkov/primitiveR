#' @title Deconstruction operators
#' @rdname deconst
#' @description Mimics \code{zeallot}'s behavior
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

    assert(expr[[1]] == sym("c"), "Only `c` can be used to combine names.")
    names <- expr[-1]

    # `length` instead of `vec_size` to be consistent with mapping
    assert(
        length(what) == length(names), 
        msg = glue_fmt_chr("`what` and `into` have different sizes.\n X `what` has length {length(what)}.\n X `into` has lenth {length(names)}."))

    invisible(walk2(what, names, ~ assign(as.character(.y), .x, envir = env)))
}