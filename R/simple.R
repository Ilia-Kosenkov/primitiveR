#' @title Vec_c
#' @param ... Items to concatenate.
#' @param .ptype,.name_spec,.name_repair Passed to \code{vctrs::vec_c}.
#' @description A shortcut for \code{vctrs::vec_c}.
#' @export
cc <- vec_c
#' @title Vec_in
#' @param needles Needles.
#' @param haystack Haystack.
#' @description A shortcut for \code{vctrs::vec_in}.
#' @export
`%vin%` <- vec_in
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
#' @param .fb_ptype Fallback type.
#' @return \code{list_of}/\code{vector} as a result of conversion.
#' @export
as_list_of.default <- function(x, ..., .fb_ptype = NULL)
    list_of(!!!x, .ptype = .fb_ptype)

#' @rdname as_conv
#' @export
as_vec <- function(x, ...)
    vec_c(!!!x)

#' @title Vector accessor
#' @param x Vector to slice.
#' @param i Indexes.
#' @description Temporary solution.
#' @return \code{list_of<item_ptype>}.
#' @export
vec_rip <- function(x, i) {
    i <- vec_cast(i, integer())

    if (is.data.frame(x))
        result <- vec_chop(x, as_list_of(i))
    else
        result <- vec_slice(x, i)

    as_list_of(result)
}