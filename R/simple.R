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