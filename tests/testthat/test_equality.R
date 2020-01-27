context("Equality tests")

test_that("Special case 0.1 + 0.2 == 0.3", {
   expect_false((0.1 + 0.2) == 0.3)
   expect_true((0.1 + 0.2) %==% 0.3)
    expect_true((0.1 + 0.2) %===% 0.3)

})

test_that("Special case 0.1 ^ 2 == 0.01", {
    expect_false((0.1 ^ 2) == 0.01)
    expect_true((0.1 ^ 2) %==% 0.01)
    expect_true((0.1 ^ 2) %===% 0.01)
})

test_that("Special case sin(pi) == 0", {
    expect_false(sin(pi) == 0)
    expect_true(sin(pi) %==% 0)
    expect_true(sin(pi) %===% 0)
})