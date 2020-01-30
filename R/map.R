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

    as_list_of(map(
        vec_rips(.x, vec_seq_along(.x)),
        .f, ...))
}

#' @rdname mappers
#' @export
vmap_pt <- function(.x, .f, ..., .fb_ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(vec_init(vec_ptype(.fb_ptype), 0L))
    }
    as_vec(map(
        vec_rips(.x, vec_seq_along(.x)),
        .f, ...))
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
    as_list_of(map2(
         vec_rips(.x, vec_seq_along(.x)),
         vec_rips(.y, vec_seq_along(.y)),
         .f, ...))
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
    as_vec(map2(
         vec_rips(.x, vec_seq_along(.x)),
         vec_rips(.y, vec_seq_along(.y)),
         .f, ...))
}

#' @rdname mappers
#' @export
vmap_if <- function(.x, .p, .f, ..., .else = NULL, .fb_ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.fb_ptype)))
    }

    as_list_of(map_if(
        vec_rips(.x, vec_seq_along(.x)),
        vec_cast(vmap_pt(.x, .p), logical()),
        .f,..., .else = .else))
}

#' @rdname mappers
#' @export
vmap_at <- function(.x, .at, .f, ..., .fb_ptype = NULL) {
    if (vec_is_empty(.at)) {
        if (is_null(.fb_ptype))
            abort("Position sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(!!!.x, .ptype = vec_ptype(.fb_ptype)))
    }

    if (vec_is_empty(.x)) {
        if (is_null(.fb_ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.fb_ptype)))
    }

    seq <- vec_rips(.x, vec_seq_along(.x))
    nms <- names(seq)
    as_list_of(
        map_at(
            seq,
            vec_as_location(.at, vec_size(.x), nms),
            .f, ...))
}