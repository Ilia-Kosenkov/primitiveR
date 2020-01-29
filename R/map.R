vec_as_location_inv <- function(
    i, n, names = NULL, ...,
    missing = c("propagate", "error"),
    arg = NULL) {

    loc <- vec_as_location(i, n, names, ..., missing = missing, arg = arg)
    full <- seq_len(n)
    diff <- setdiff(full, unique(loc))

    vec_as_location(diff, n, names, ..., missing = missing, arg = arg)
}


vec_as_mapper <- function(.f, ...) {
    function(x)
        (function(i) vec_slice(x, i)[[1]]) %>>% as_mapper(.f, ...)
}
vec_as_mapper2 <- function(.f, ...) {
    f <- as_mapper(.f, ...)
    function(x, y)
        function(i) {
            f(vec_slice(x, i)[[1]], vec_slice(y, i)[[1]])
        }
}

#' @title Mappers
#' @rdname mappers
#'
#' @param .x,.y Vectors to iterate over.
#' @param .p Predicate function.
#' @param .f Mapper function.
#' @param .else Alternative mapper for \code{vmap_if}.
#' @param .at Location to map at.
#' @param .fb_ptype Fallback \code{ptype} for type-stability of empty collections.
#' @param ... Additional paramters passed to mappers
#' @description Performs \code{vctrs} - compatibel mapping.
#' Requries strongly typed input and output. By default returns \code{vctrs::list_of}.
#' \code{*_pt} version performs conversion to the common \code{ptype}.
#' @return Either \code{list_of} or vector of common \code{ptype}.
#' @export
vmap <- function(.x, .f, ..., .fb_ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.fb_ptype)))
    }

    .f <- vec_as_mapper(.f, ...)(.x)

    set_names(as_list_of(map(vec_seq_along(.x), .f)), names(.x))
}

#' @rdname mappers
#' @export
vmap_pt <- function(.x, .f, ..., .fb_ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(vec_init(vec_ptype(.fb_ptype), 0L))
    }
    .f <- vec_as_mapper(.f, ...)(.x)
    vec_c(!!!map(vec_seq_along(.x), .f))
}

#' @rdname mappers
#' @export
vmap2 <- function(.x, .y, .f, ..., .fb_ptype = NULL) {
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("One of the input sequences is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.fb_ptype)))
    }
    .f <- vec_as_mapper2(.f, ...)(.x, .y)
    set_names(as_list_of(map(vec_seq_along(.x), .f)), names(.x))
}

#' @rdname mappers
#' @export
vmap2_pt <- function(.x, .y, .f, ..., .fb_ptype = NULL) {
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("One of the input sequences is empty.", "primitiveR_invalid_arg")
        return(vec_init(vec_ptype(.fb_ptype), 0L))
    }
    .f <- vec_as_mapper2(.f, ...)(.x, .y)
    set_names(vec_c(!!!map(vec_seq_along(.x), .f)), names(.x))
}

#' @rdname mappers
#' @export
vmap_if <- function(.x, .p, .f, ..., .else = NULL, .fb_ptype = NULL) {
    sel <- vec_cast(vmap_pt(.x, .p), logical())
    out <- vec_init(list(), vec_size(.x))
    if (is_null(.else))
        .else <- ~.x

    loc <- vec_as_location(sel, vec_size(.x), names(.x))
    inv <- vec_as_location(!sel, vec_size(.x), names(.x))

    if (!vec_is_empty(loc))
        out[loc] <- vmap(.x[loc], .f, ..., .fb_ptype = .fb_ptype)

    if (!vec_is_empty(inv))
        out[inv] <- vmap(.x[inv], .else, ..., .fb_ptype = .fb_ptype)

    return(set_names(as_list_of(out), names(.x)))
}

#' @rdname mappers
#' @export
vmap_at <- function(.x, .at, .f, ..., .fb_ptype = NULL) {
    if (vec_is_empty(.at)) {
        if (is_null(.fb_ptype))
            abort("Position sequence is empty.", "primitiveR_invalid_arg")
        return(vec_cast(.x, list_of(.ptype = vec_ptype(.fb_ptype))))
    }

    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.fb_ptype)))
    }

    loc <- vec_as_location(.at, vec_size(.x), names(.x))
    inv <- vec_as_location_inv(.at, vec_size(.x), names(.x))

    out <- vec_init(list(), vec_size(.x))

    if (!vec_is_empty(loc))
        out[loc] <- vmap(.x[loc], .f, ..., .fb_ptype = .fb_ptype)

    if (!vec_is_empty(inv))
        out[inv] <- vmap(.x[inv], ~.x, .fb_ptype = .fb_ptype)

    return(set_names(as_list_of(out), names(.x)))
}