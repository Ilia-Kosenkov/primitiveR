cc <- vec_c
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
#' @export
fct_get <- function(f) {
    assert(is.factor(f))
    levels(f)[f]
}