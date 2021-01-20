#' @title Mappers
#' @rdname mappers
#'
#' @param .x,.y Vectors to iterate over.
#' @param .p Predicate function.
#' @param .f Mapper function.
#' @param .else Alternative mapper for \code{vmap_if}.
#' @param .at Location to map at.
#' @param .ptype Fallback \code{ptype} for type-stability of empty collections.
#' @param ... Additional paramters passed to mappers
#' @description Performs \code{vctrs} - compatibel mapping.
#' Requries strongly typed input and output. By default returns \code{vctrs::list_of}.
#' \code{*_pt} version performs conversion to the common \code{ptype}.
#' @return Either \code{list_of} or vector of common \code{ptype}.
#' @export
vmap <- function(.x, .f, ..., .ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.ptype))
            abort("Input sequence is empty.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.ptype)))
    }

    result <- map(vec_rips(.x, vec_seq_along(.x)), .f, ...)

    # Temporary solution because *_common(!!!) does not work
    .ptype <- .ptype %||% vec_ptype(result[[1]])

    return(vec_cast(as_list_of(result), list_of(.ptype = .ptype)))
}

#' @rdname mappers
#' @export
vmap_pt <- function(.x, .f, ..., .ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.ptype))
            abort("Invalid input.\n X `.x` is empty.\n X `.ptype` is `NULL`.", "primitiveR_invalid_arg")
        return(vec_init(vec_ptype(.ptype), 0L))
    }
    as_vec(map(
        vec_rips(.x, vec_seq_along(.x)),
        .f, ...), .ptype = .ptype)
}

#' @rdname mappers
#' @export
vmap2 <- function(.x, .y, .f, ..., .ptype = NULL) {
    if (vec_is_empty(.x) || vec_is_empty(.y)) {
        if (is_null(.ptype))
            abort(
                glue_fmt_chr("One of the input sequences is empty.\n X `.x` has length {vec_size(.x)}.\n X `.y` has length {vec_size(.y)}."), 
                "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.ptype)))
    }
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    result <- map2(
         vec_rips(.x, vec_seq_along(.x)),
         vec_rips(.y, vec_seq_along(.y)),
         .f, ...)

    # Temporary solution because *_common(!!!) does not work
    .ptype <- .ptype %||% vec_ptype(result[[1]])

    return(vec_cast(as_list_of(result), list_of(.ptype = .ptype)))
}

#' @rdname mappers
#' @export
vmap2_pt <- function(.x, .y, .f, ..., .ptype = NULL) {
    if (vec_is_empty(.x) || vec_is_empty(.y)) {
        if (is_null(.ptype))
            abort(
                glue_fmt_chr("One of the input sequences is empty.\n X `.x` has length {vec_size(.x)}.\n X `.y` has length {vec_size(.y)}."), 
                "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.ptype)))
    }
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    as_vec(map2(
         vec_rips(.x, vec_seq_along(.x)),
         vec_rips(.y, vec_seq_along(.y)),
         .f, ...), .ptype = .ptype)
}

#' @rdname mappers
#' @export
vmap_if <- function(.x, .p, .f, ..., .else = NULL, .ptype = NULL) {
    if (vec_is_empty(.x)) {
        if (is_null(.ptype))
            abort("Invalid input.\n X `.x` is empty.\n X `.ptype` is `NULL`.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.ptype)))
    }

    result <- map_if(
        vec_rips(.x, vec_seq_along(.x)),
        vmap_pt(.x, .p, .ptype = logical()),
        .f, ..., .else = .else)

    # Temporary solution because *_common(!!!) does not work
    .ptype <- .ptype %||% vec_ptype(result[[1]])

    return(vec_cast(as_list_of(result), list_of(.ptype = .ptype)))
}

#' @rdname mappers
#' @export
vmap_at <- function(.x, .at, .f, ..., .ptype = NULL) {
    if (vec_is_empty(.at)) {
        if (is_null(.ptype))
            abort("Invalid input.\n X `.at` is empty.\n X `.ptype` is `NULL`.", "primitiveR_invalid_arg")
        return(vec_cast(as_list_of(.x), list_of(.ptype = vec_ptype(.ptype))))
    }

    if (vec_is_empty(.x)) {
        if (is_null(.ptype))
            abort("Invalid input.\n X `.x` is empty.\n X `.ptype` is `NULL`.", "primitiveR_invalid_arg")
        return(list_of(.ptype = vec_ptype(.ptype)))
    }

    seq <- vec_rips(.x, vec_seq_along(.x))
    nms <- names(seq)
    result <- map_at(
            seq,
            vec_as_location(.at, vec_size(.x), nms),
            .f, ...)

    # Temporary solution because *_common(!!!) does not work
    .ptype <- .ptype %||% vec_ptype(result[[1]])

    return(vec_cast(as_list_of(result), list_of(.ptype = .ptype)))
}

#' @rdname mappers
#' @export
vkeep <- function(.x, .p, ..., .ptype = NULL) {
    .p <- as_mapper(.p, ...)
    sel <- vmap_pt(.x, .p, .ptype = logical())
    loc <- vec_as_location(sel, vec_size(.x))
    result <- vec_rips(.x, loc)

    # `vec_rips` is type-stable
    if (!is_null(.ptype))
        result <- vec_cast(as_list_of(result), list_of(.ptype = .ptype))

    return(result)
}

#' @rdname mappers
#' @export
vdiscard <- function(.x, .p, ..., .ptype = NULL) {
    .p <- as_mapper(.p, ...)
    sel <- vmap_pt(.x, .p, .ptype = logical())
    loc <- vec_as_location(!sel, vec_size(.x))
    result <- vec_rips(.x, loc)

    # `vec_rips` is type-stable
    if (!is_null(.ptype))
        result <- vec_cast(as_list_of(result), list_of(.ptype = .ptype))

    return(result)
}

#' @rdname mappers
#' @export
vimap <- function(.x, .f, ..., .ptype = NULL) {
    vmap2(.x, vec_names(.x) %||% vec_seq_along(.x), .f, ..., .ptype = .ptype)
}

#' @rdname mappers
#' @export
vimap_pt <- function(.x, .f, ..., .ptype = NULL) {
    vmap2_pt(.x, vec_names(.x) %||% vec_seq_along(.x), .f, ..., .ptype = .ptype)
}