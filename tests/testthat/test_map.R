context("{vctrs}-compatible mappers")

test_that("`map_*` vs `vmap_pt`", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(map_dbl(x, ~.x ^ 2) %===% vmap_pt(x, ~.x ^ 2))
})

test_that("`map` vs `vmap`", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(as_list_of(map(x, ~ .x ^ 2)) %===% vmap(x, ~ .x ^ 2))
})


test_that("`map_if` vs `vmap_if`", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(as_list_of(map_if(x, ~ .x > 0, ~ .x ^ 2)) %===% vmap_if(x, ~ .x > 0, ~ .x ^ 2))
})

test_that("`map_if` vs `vmap_if` with anonymous function", {
    set.seed(1)

    x <- rnorm(1000)

    expect_true(as_list_of(map_if(x, ~ .x > 0, ~ .x ^ 2)) %===% vmap_if(x, function(x) x > 0, ~ .x ^ 2))
})

test_that("`map_at` vs `vmap_at`", {
    set.seed(1)

    x <- rnorm(1000)
    at <- sample(1:1000, 100)

    expect_true(
        as_list_of(map_at(x, at, ~ .x ^ 2)) %===% vmap_at(x, at, ~ .x ^ 2))
})

test_that("`vmap` over `data.frame`", {
   expect_true(pmap_dbl(mtcars, sum) %===% vmap_pt(mtcars, sum))
})

test_that("`lin` behaviour", {
    expect_true(lin(-10:10, cc(0, 1), cc(0, 2)) %===% (2 * (-10 : 10)))
})

test_that("`vkeep` & `vdiscard` work similarly", {
    expect_true(vkeep(mtcars, ~.x$hp > 200) %===% vdiscard(mtcars, ~.x$hp <= 200))
})

test_that("`vkeep` works as `keep`", {
    expect_true(vkeep(mtcars, ~ .x$hp > 200) %===% keep(as_list_of(mtcars), ~.x$hp > 200))
})

test_that("`vdiscard` works as `discard`", {
    expect_true(vdiscard(mtcars, ~ .x$hp > 200) %===% discard(as_list_of(mtcars), ~ .x$hp > 200))
})

test_that("`vimap` vs `imap`", {
    expect_true(vimap(set_names(LETTERS, letters), vec_c) %===% as_list_of(imap(set_names(LETTERS, letters), vec_c)))
})

test_that("`vimap_pt` vs `imap_chr`", {
    expect_true(vimap_pt(set_names(LETTERS, letters), paste0) %===% imap_chr(set_names(LETTERS, letters), paste0))
})