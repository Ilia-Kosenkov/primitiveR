if (interactive()) {
    library(testthat)
    is_null <- rlang::is_null
    test_dir(file.path("tests", "testthat"))

} else {
    library(purrr)
    library(testthat)
    library(primitiveR)
    # Needed for testthat
    library(Rcpp)
    test_check("primitiveR")
}
