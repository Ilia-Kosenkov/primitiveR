#' @title Compose functions
#' @rdname composer
#' @param x Lhs.
#' @param y Rhs.
#' @description Composes two functions using \code{purrr::compose}, in different directions.
#' Supports \code{rlang}-style lambdas (in parentheses).
#' @return A composed function
#'
#' @examples
#' (~.x ^ 2) %>>% (~.x + 5)
#' @export
`%>>%` <- function(x, y) {
    enquo(x) -> q_x
    ex <- as.list(quo_get_expr(q_x))
    env <- quo_get_env(q_x)

    if (ex[[1]] %===% sym("%>>%")) {
        lhs <- list()
        while ((length(ex) > 1L) && ex[[1]] %===% sym("%>>%")) {
            lhs <- append(lhs, list(ex[[3]]))
            ex <- ex[[2]]
        }
        lhs <- append(lhs, ex) %>%
            map(as_quosure, env)


        return(eval_tidy(quo(compose(y, !!!lhs, .dir = "backward"))))
    }
    return(compose(x, y, .dir = "backward"))
}

#' @rdname composer
#' @export
`%<<%` <- function(x, y) {
    enquo(x) -> q_x
    ex <- as.list(quo_get_expr(q_x))
    env <- quo_get_env(q_x)

    if (ex[[1]] %===% sym("%<<%")) {
        lhs <- list()
        while ((length(ex) > 1L) && ex[[1]] %===% sym("%<<%")) {
            lhs <- append(lhs, list(ex[[3]]))
            ex <- ex[[2]]
        }
        lhs <- append(lhs, ex) %>%
            map(as_quosure, env)

        return(eval_tidy(quo(compose(y, !!!lhs, .dir = "forward"))))
    }
    return(compose(x, y, .dir = "forward"))
}
