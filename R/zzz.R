#' @useDynLib primitiveR
#' @import     Rcpp
#' @importFrom rlang    enquo quo_get_env quo_get_expr sym abort warn quo exec %@%
#' @importFrom rlang    is_null as_label is_na !!! eval_tidy as_quosure is_symbol
#' @importFrom rlang    quo_text parse_expr is_empty set_names is_bare_list %|% %||%
#'
#' @importFrom vctrs    vec_c vec_in vec_size vec_is stop_incompatible_type is_list_of
#' @importFrom vctrs    vec_assert vec_cast vec_is_empty allow_lossy_cast vec_chop
#' @importFrom vctrs    vec_cast_common vec_recycle_common vec_equal list_of
#' @importFrom vctrs    vec_as_location vec_slice as_list_of vec_init vec_seq_along
#'
#' @importFrom purrr    walk2 map2_lgl map compose imap_int as_mapper
#' @importFrom purrr    map2 map_at map_if
#'
#' @importFrom glue     glue
NULL