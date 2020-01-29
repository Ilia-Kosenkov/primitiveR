context("Composition tests")

test_that("%>>% behaves correctly", {
    (~.x + 1) %>>%
        (~.x + 2) %>>%
        identity %>>%
        (function(x) x + 3) -> f


    set.seed(1)

    rnorm(1000, 1, 20) -> x

    expect_true((x + 6) %===% f(x))
})

test_that("%<<% behaves correctly", {
    (~.x + 1) %<<%
        (~.x + 2) %<<%
        identity %<<%
        (function(x) x + 3) -> f


    set.seed(1)

    rnorm(1000, 1, 20) -> x

    expect_true((x + 6) %===% f(x))
})

test_that("%>>% with only two functions", {
    (~.x + 5) %>>% (~.x * 2) -> f

    set.seed(1)
    rnorm(1000, 1, 20) -> x

    expect_true((2 * (x + 5)) %===% f(x))
})

test_that("%<<% with only two functions", {
    (~.x * 2) %<<% (~.x + 5) -> f

    set.seed(1)
    rnorm(1000, 1, 20) -> x

    expect_true((2 * (x + 5)) %===% f(x))
})