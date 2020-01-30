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

test_that("Vectorized equality", {
    set.seed(1)
    x <- runif(1000L, 1e-15, 1)
    expect_false(all(x == (x + 0.9 * 10 ^ floor(log10(x)) * .Machine$double.eps)))
    expect_true(all(x %==% (x + 0.9 * 10 ^ floor(log10(x)) * .Machine$double.eps)))
    expect_true(x %===% (x + 0.9 * 10 ^ floor(log10(x)) * .Machine$double.eps))
})

test_that("Recycling", {
    expect_true(all(rep(10.1, 100) %==% 10.1))
    expect_true(rep(10.1, 100) %===% 10.1)
})

test_that("Special cases 0 == 0", {
    expect_true(0.0 %===% 0.0)
    expect_false(0.0 %===% .Machine$double.eps)
    expect_true((0.9 * .Machine$double.eps) %===% 0.0)
})

test_that("Special cases 1000 == 1000", {
    expect_true(1000.0 %===% 1000.0)
    expect_false(1000.0 %===% (1000 + 1e3 * .Machine$double.eps))
    expect_true((5e2 * .Machine$double.eps + 1000) %===% 1000.0)
    expect_false((5e2 * .Machine$double.eps + 1000) == 1000.0)
})

test_that("`NA` equality", {
    expect_false(NA %==% 5)
    expect_false(NA %==% NA)
    expect_false(5 %==% NA)
})