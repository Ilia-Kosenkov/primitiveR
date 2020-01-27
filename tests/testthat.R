if (interactive()) {
    library(testthat)
    test_dir(fs::path("tests", "testthat"))

}else{
    library(testthat)
    library(primitiveR)

    test_check("primitiveR")
}
