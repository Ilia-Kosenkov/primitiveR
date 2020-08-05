#' @title Vec_c
#' @param ... Items to concatenate.
#' @param .ptype,.name_spec,.name_repair Passed to \code{vctrs::vec_c}.
#' @description A shortcut for \code{vctrs::vec_c}.
#' @export
cc <- vec_c
#' @title Vec_in
#' @rdname vin
#' @param needles Needles.
#' @param haystack Haystack.
#' @description A shortcut for \code{vctrs::vec_in}.
#' @export
`%vin%` <- function(needles, haystack) vec_in(needles = needles, haystack = haystack)
#' @rdname vin
#' @export
`%!vin%` <- function(needles, haystack) !vec_in(needles = needles, haystack = haystack)

#' @title len
#' @rdname len
#' @param x Object to measure.
#'
#' @return Length of the object.
#' @export
len <- function(x) UseMethod("len")
#' @rdname len
#' @export
len.default <- function(x) vctrs::vec_size(x)
#' @rdname len
#' @export
len.unit <- function(x) length(x)
#' @rdname len
#' @export
len.quosures <- function(x) length(x)


#' @title Get factor values
#' @param f Factor.
#' @export
fct_get <- function(f) {
    assert(is.factor(f))
    levels(f)[f]
}


#' @title As converters
#' @rdname as_conv
#' @param x Collection to convert.
#' @param ... Unused args, for compatibility.
#' @param .ptype Fallback type.
#' @return \code{list_of}/\code{vector} as a result of conversion.
#' @export
as_list_of.default <- function(x, ..., .ptype = NULL) {
    as_list_of(
        set_names(
            map(vec_seq_along(x), vec_rip, x = x, strip_names = TRUE),
            vec_names(x)),
        .ptype = .ptype)
}

#' @title As converters
#' @rdname as_conv
#' @param x Collection to convert.
#' @param ... Unused args, for compatibility.
#' @param .ptype Fallback type.
#' @method as_list_of data.frame
#' @return \code{list_of}/\code{vector} as a result of conversion.
#' @export
as_list_of.data.frame <- function(x, ..., .ptype = NULL) {
    vec_cast(
        as_list_of(set_names(
            map(vec_seq_along(x), vec_rip, x = x, strip_names = FALSE),
            vec_names(x))),
        to = list_of(.ptype = .ptype %||% vec_ptype(x)))
}

#' @rdname as_conv
#' @export
as_vec <- function(x, ..., .ptype = NULL)
    # Temporary solution
    vec_c(!!!x, .ptype = .ptype %||% vec_ptype(x[[1]]))

#' @title Vector accessor
#' @rdname vec_rips
#' @param x Vector to slice.
#' @param i Indexes.
#' @param strip_names Whether to strip names.
#' @description Temporary solution.
#' @return \code{list_of<item_ptype>}.
#' @export
vec_rips <- function(x, i) {
    nms <- vec_names(x)

    i <- vec_as_location(i, vec_size(x), vec_names(x))


    if (is.data.frame(x)) {
        if (!is_null(nms))
            nms <- vec_slice(nms, i)
        result <- vec_cast(as_list_of(set_names(vec_chop(x, as_list_of(i)), nms)), to = list_of(.ptype = vec_ptype(x)))
    }
    else
        result <- as_list_of(vec_slice(x, i))

    return(result)
}

#' @rdname vec_rips
#' @export
vec_rip <- function(x, i, strip_names = FALSE) {
    i <- vec_as_location(i, vec_size(x), vec_names(x))
    vec_assert(i, integer(), 1L)

    result <- vec_slice(x, i)
    if (strip_names)
        result <- set_names(result, NULL)
    if (is_bare_list(x) || is_list_of(x))
        result <- as_vec(result)

    return(result)
}

#' @title Vector names
#' @rdname vec_names
#' @param x Vector to get names from.
#' @param ... Placeholder parameters.
#' @return Names of the vector.
#' @export
vec_names <- function(x, ...) UseMethod("vec_names")
#' @rdname vec_names
#' @method vec_names data.frame
#' @export
vec_names.data.frame <- function(x, ...) rownames(x)
#' @rdname vec_names
#' @method vec_names default
#' @export
vec_names.default <- function(x, ...) names(x)

#' @title Item ptype
#'
#' @param x Container to test.
#'
#' @return \code{ptype} of the container.
#' @export
vec_item_ptype <- function(x) {
    # For scalars (like vectors and also data.frames/tibbles),
    # item_ptype == ptype;
    # For lists/list_ofs, which act as generic containers,
    # item_ptype != ptype

    if (is_bare_list(x))
        return(vec_ptype_common(!!!x))
    if (is_list_of(x))
        return(vec_ptype(x %@% "ptype"))

    return(vec_ptype(x))
}


#' @title lin
#' @param x Where to interpolate.
#' @param x0 Arguments (size 2).
#' @param y0 Values (size 2).
#' @return Interpolated value between two provided.
#' @export
lin <- function(x, x0, y0) {

    data <- vec_cast_common(x0, y0)
    vctrs::vec_recycle_common(!!!data, .size = 2L) %->% c(x0, y0)

    dx <- diff(x0)
    dy <- diff(y0)
    sz <- len(x)
    if (sz %===% 0L)
        return(x)
    else if (sz %===% 1L)
        (x - x0[1]) * dy / dx + y0[1]
    else {
        vmap_pt(x, ~ (.x - x0[1]) * dy / dx + y0[1])
    }
}
