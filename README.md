primitiveR readme
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# primitiveR

## Installation

Development version can be installed from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("Ilia-Kosenkov/primitiveR")
```

## Example \#1

``` r
library(purrr)
library(vctrs)
library(primitiveR)
```

`primitiveR` allows for type-preserving and structure-preserving
mappings (at least while `purrr` does not fully support it).

``` r
mtcars[1:5,1:3] %>% map(~.x)
#> $mpg
#> [1] 21.0 21.0 22.8 21.4 18.7
#> 
#> $cyl
#> [1] 6 6 4 6 8
#> 
#> $disp
#> [1] 160 160 108 258 360
mtcars[1:5,1:3] %>% vmap(~.x)
#> <list_of<
#>   data.frame<
#>     mpg : double
#>     cyl : double
#>     disp: double
#>   >
#> >[5]>
#> $`Mazda RX4`
#>           mpg cyl disp
#> Mazda RX4  21   6  160
#> 
#> $`Mazda RX4 Wag`
#>               mpg cyl disp
#> Mazda RX4 Wag  21   6  160
#> 
#> $`Datsun 710`
#>             mpg cyl disp
#> Datsun 710 22.8   4  108
#> 
#> $`Hornet 4 Drive`
#>                 mpg cyl disp
#> Hornet 4 Drive 21.4   6  258
#> 
#> $`Hornet Sportabout`
#>                    mpg cyl disp
#> Hornet Sportabout 18.7   8  360
```

The `*map_pt` version has an optional `.ptype` argument that can be used
to select the output type. By default, it will result into the
following:

``` r
mtcars[1:3,1:3] %>% vmap(~.x) -> x
x
#> <list_of<
#>   data.frame<
#>     mpg : double
#>     cyl : double
#>     disp: double
#>   >
#> >[3]>
#> $`Mazda RX4`
#>           mpg cyl disp
#> Mazda RX4  21   6  160
#> 
#> $`Mazda RX4 Wag`
#>               mpg cyl disp
#> Mazda RX4 Wag  21   6  160
#> 
#> $`Datsun 710`
#>             mpg cyl disp
#> Datsun 710 22.8   4  108
x %>% vmap_pt(~.x)
#>                mpg cyl disp
#> Mazda RX4     21.0   6  160
#> Mazda RX4 Wag 21.0   6  160
#> Datsun 710    22.8   4  108
```

``` r
1:10 %>% vmap_if(~.x > 5, ~1L, .else = ~0L) %>% as_vec
#>  [1] 0 0 0 0 0 1 1 1 1 1
```

## Example \#2

`primitiveR` provides floating-point equality extension using `%==%` and
`%!=%`, which operate as normal `==` and `!=` for vector types.

``` r
0.1 + 0.2 == 0.3
#> [1] FALSE
(0.1 + 0.2) %==% 0.3
#> [1] TRUE
```

`%===%` and `%!==%` are particular useful in conditions, because these
operators apply `all`/`any` to the output of `%==%` or `%!=%`.

``` r
(1:5) %!=% 1
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE
(1:5) %!==% 1
#> [1] TRUE
(1:5) %==% (1:5)
#> [1] TRUE TRUE TRUE TRUE TRUE
(1:5) %===% (1:5)
#> [1] TRUE
```
