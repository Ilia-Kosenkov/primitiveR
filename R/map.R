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

vmap <- function(.x, .f, ...) {
    .f <- vec_as_mapper(.f, ...)(.x)

    set_names(as_list_of(map(vec_seq_along(.x), .f)), names(.x))
}

vmap_pt <- function(.x, .f, ...) {
    .f <- vec_as_mapper(.f, ...)(.x)
    vec_c(!!!map(vec_seq_along(.x), .f))
}


vmap2 <- function(.x, .y, .f, ...) {
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    .f <- vec_as_mapper2(.f, ...)(.x, .y)
    set_names(as_list_of(map(vec_seq_along(.x), .f)), names(.x))
}

vmap2_pt <- function(.x, .y, .f, ...) {
    vec_recycle_common(.x, .y) %->% c(.x, .y)
    .f <- vec_as_mapper2(.f, ...)(.x, .y)
    set_names(vec_c(!!!map(vec_seq_along(.x), .f)), names(.x))
}


vmap_if <- function(.x, .p, .f, ..., .else = NULL) {
    sel <- vec_cast(vmap_pt(.x, .p), logical())
    out <- vec_init(list(), vec_size(.x))

    out[sel] <- vmap(.x[sel], .f, ...)
    if (is_null(.else))
        out[!sel] <- .x[!sel]
    else
        out[!sel] <- vmap(.x[!sel], .else, ...)

    return(set_names(as_list_of(out), names(.x)))
}

vmap_at <- function(.x, .at, .f, ...) {
    loc <- vec_as_location(.at, vec_size(.x), names(.x))
    inv <- vec_as_location_inv(.at, vec_size(.x), names(.x))

    out <- vec_init(list(), vec_size(.x))

    out[loc] <- vmap(.x[loc], .f, ...)
    out[inv] <- .x[inv]

    return(set_names(as_list_of(out), names(.x)))
}