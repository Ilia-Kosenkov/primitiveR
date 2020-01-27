if (interactive()) {
    library(testthat)
    test_dir(file.path("tests", "testthat"))

}else{
    library(testthat)
    library(primitiveR)
    # Needed for testthat
    library(Rcpp)
    test_check("primitiveR")
}
