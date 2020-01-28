# ADOPTED FROM [glue] package; vignettes

sprintf_transformer <- function(text, envir) {
    expr <- gsub("^(.*?)(?::(\\ *%-?.+))?$", "\\1", text)

    frmt <- gsub("^(.*?)(?::(\\ *%-?.+))?$", "\\2", text)

    vals <- eval_tidy(parse_expr(expr), env = envir)

    if (!is_na(frmt) && !is_empty(frmt) && nzchar(frmt))
        return(sprintf(frmt, vals))

    return(as.character(vals))
}

#' @title String interpolation
#' @rdname glue_fmt
#' @param ... Parameters passed to \code{glue::glue}.
#' @param .envir Evaluation environment.
#'
#' @return Interpolated \code{glue} strings.
#' @export
glue_fmt <- function(..., .envir = parent.frame()) {
    glue::glue(..., .envir = .envir, .transformer = sprintf_transformer)
}

#' @rdname glue_fmt
#' @export
glue_fmt_chr <- function(..., .envir = parent.frame()) {
    as.character(glue::glue(..., .envir = .envir, .transformer = sprintf_transformer))
}