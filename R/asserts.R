#' @title Assert
#'
#' @param expr Expression to test. Should evaluate to logical.
#' @param msg Optional message to include in the error.
#' @param subclass Additional error subclasses.
#'
#' @return Invisibly \code{TRUE} if successful.
#' @export
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

    invisible(TRUE)
}