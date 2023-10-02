
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broadcast

<!-- badges: start -->
<!-- badges: end -->

The R package `broadcast` provides
[numpy](https://numpy.org/doc/stable/index.html) style
[broadcasting](https://numpy.org/doc/stable/user/basics.broadcasting.html)
ability to R matrices (not yet on arrays more than 2D!).

``` r
# remotes::install_github("talegari/broadcast") # not (yet) on CRAN
library("broadcast")
```

``` r
m1 = matrix(1:6, nrow = 2)
m1
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

m2 = matrix(7:9, nrow = 1)
m2
#>      [,1] [,2] [,3]
#> [1,]    7    8    9

# without `broadbast`
m1 + m2
#> Error in m1 + m2: non-conformable arrays

# with broadcast
m1 %a+% m2
#>      [,1] [,2] [,3]
#> [1,]    8   11   14
#> [2,]    9   12   15
```

## What’s inside

- `%a+%` (aka `array_add`)
- `%a-%` (aka `array_sub`)
- `%a*%` (aka `array_mul`)
- `%a/%` (aka `array_div`)
- `array_pairwise` : Apply any function operating pairwise
- `is_broadcastable`: Whether a matrix or a dinmension can be
  broadcasted to a new dimension
- `get_pairwise_broadcast_dim`: Common broadcastable dimension given a
  pair of matrices
- `broadcast_to`: broadcast a matrix to a given dimension

## Caveats

1.  Written in R, not C++ (still plenty fast!)
2.  Works only on matrices and not generalized to arrays (as of now)
3.  Won’t go on CRAN until (1) an (2) are resolved :)

## Installation

    remotes::install_github("talegari/broadcast")

------------------------------------------------------------------------

    ನರಕಕ್ಕ್ ಇಳ್ಸಿ ನಾಲ್ಗೆ ಸೀಳ್ಸಿ
    ಬಾಯ್ ಒಲಿಸಾಕಿದ್ರೂನೆ-
    ಮೂಗ್ನಲ್ ಕನ್ನಡ್ ಪದವಾಡ್ತೀನಿ!

[ರತ್ನನ್
ಪದಗಳು.](https://sites.google.com/site/kavanasangraha/%E0%B2%95%E0%B2%B5%E0%B2%A8-%E0%B2%B8%E0%B2%97%E0%B2%B0%E0%B2%B9/%E0%B2%9C-%E0%B2%AA-%E0%B2%B0%E0%B2%9C%E0%B2%B0%E0%B2%A4%E0%B2%A8/%E0%B2%B0%E0%B2%A4%E0%B2%A8%E0%B2%A8-%E0%B2%AA%E0%B2%A6%E0%B2%97%E0%B2%B3)
