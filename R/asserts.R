assert <- function(expr, msg = NULL, subclass = NULL) {
    q <- enquo(expr)
    if (!expr) {

        if (!is_null(msg)) {
            if (!vec_is(msg, character())) {
                warn("Assert message is not a string", "primitiveR_unsupported_arg")
                msg <- paste0("Assertion `", quo_text(q), "` has failed")
            }
            else
                msg <- paste0(
                    "Assertion `",
                    quo_text(q),
                    "` has failed: ",
                    paste(msg, collapse = "; "))
        }
        else
            msg <- paste0("Assertion `", quo_text(q), "` has failed")


        if (!is_null(subclass) && !vec_is(subclass, character())) {
                warn("Assert subclass is not a string", "primitiveR_unsupported_arg")
                subclass <- NULL
        }
        class <- cc(subclass, "primitiveR_assert_failed")

        abort(msg, class)
    }
}

#' vec_assert_numeric
#' @rdname vec_ext
#' @param x Vector to test
#' @param size Desired size. Can be \code{NULL}.
#' @param arg Arg names, defaults to the name of \code{X}.
#'
#' @return Invisibly returns \code{x} cast to \code{double()}
#' @export
vec_assert_numeric <- function(x, size = NULL, arg = as_label(substitute(x))) {
    if (!vec_is(x, integer(), size = size) && !vec_is(x, double(), size = size))
        stop_incompatible_type(x, numeric(), x_arg = arg)

    invisible(vec_cast(x, double(), x_arg = arg))
}

#' vec_assert_integerish
#' @rdname vec_ext
#' @param size Desired size. Can be \code{NULL}.
#'
#' @return Invisibly returns \code{x} cast to \code{integer()}.
#' @export
vec_assert_integerish <- function(x, size = NULL, arg = as_label(substitute(x))) {
    if (vec_is(x, integer()))
        result <- x

    if (vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            result <- allow_lossy_cast(vec_cast(x, integer(), x_arg = arg))
        else
            stop_incompatible_cast(x[inds[1]], integer(), x_arg = as_label(substitute(x[inds[1]])))
        }
    if (!is_null(size)) {
        vec_assert(result, size = size)
    }

    invisible(result)
}

#' vec_is_integerish
#' @rdname vec_ext
#' @return Returns \code{TRUE} if the input can be losslessly coerced to \code{integer()}.
#' @export
vec_is_integerish <- function(x, size = NULL, arg = as_label(substitute(x))) {
    if (vec_is(x, integer(), size))
        return(TRUE)

    if (vec_is(x, double(), size)) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            return(TRUE)
        else
            return(FALSE)
    }

    return(FALSE)
}