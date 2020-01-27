#' @title are_same_all
#' @param x Vector to test
#' @param eps Floating-point comparison tolerance
#'
#' @return \code{TRUE} if all elements are equal
#' @export
are_same_all <- function(x, eps = 1) {
    # `eps` is tested in `are_equal_f`
    x <- vec_cast(x, double())

    if (len(x) == 0L)
        return(FALSE)
    if (len(x) == 1L)
        return(TRUE)

    all(are_equal_f(vec_slice(x, 1L), vec_slice(x, 2L:len(x)), eps = eps))
}

#' @title unique_which_f
#'
#' @param x Vector to test.
#' @param eps Floating-point comparison tolerance
#'
#' @return Indices of items that are unique within the source vector.
#' @export
unique_which_f <- function(x, eps = 1L) {
    x <- vec_cast(x, double())
    vec_as_location(
        cc(TRUE, imap_int(x[-1], ~ sum(are_equal_f(.x, vec_slice(x, 1:.y), eps))) %==% 0L),
        len(x))
}

#' @title unique_f
#'
#' @param x Vector to test.
#' @param eps Floating-point comparison tolerance
#'
#' @return Vector of unique items tkaen from the source vector.
#' @export
unique_f <- function(x, eps = 1) {
    vec_slice(x, unique_which_f(x, eps))
}

#' @title distinct_which_f
#'
#' @param x First vector.
#' @param y Second vector.
#' @param eps Floating-point comparison tolerance
#'
#' @return List with two items, \code{x} and \code{y}, which contain indices of values that are not present in another collection
#' @export
distinct_which_f <- function(x, y, eps = 1L) {
    vec_cast_common(x, y, .to = double()) %->% c(x, y)

    prod <- !outer(x, y, are_equal_f, eps = eps)

    list_of(x = vec_as_location(apply(prod, 1, all), len(x)),
            y = vec_as_location(apply(prod, 2, all), len(y)))
}

#' @title distinct_f
#'
#' @param x First vector.
#' @param y Second vector.
#' @param eps Floating-point comparison tolerance
#'
#' @return List with two items, \code{x} and \code{y}, which contain values that are not present in another collection
#' @export
distinct_f <- function(x, y, eps = 1L) {
    id_x <- NULL
    id_y <- NULL
    c(id_x, id_y) %<-% distinct_which_f(x, y, eps)
    list_of(x = vec_slice(x, id_x), y = vec_slice(y, id_y))
}
