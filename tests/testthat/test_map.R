context("{vctrs}-compatible mappers")

test_that("map_* vs vmap_pt", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(map_dbl(x, ~.x ^ 2) %===% vmap_pt(x, ~.x ^ 2))
})

test_that("map_vs vmap", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(as_list_of(map(x, ~ .x ^ 2)) %===% vmap(x, ~ .x ^ 2))
})


test_that("map_if vmap_if", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(as_list_of(map_if(x, ~ .x > 0, ~ .x ^ 2)) %===% vmap_if(x, ~ .x > 0, ~ .x ^ 2))
})

test_that("map_at vmap_at", {
    set.seed(1)

    x <- rnorm(1000)
    at <- sample(1:1000, 100)

    expect_true(
        as_list_of(map_at(x, at, ~ .x ^ 2)) %===% vmap_at(x, at, ~ .x ^ 2))
})