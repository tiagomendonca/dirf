
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirf - Distances with Random Forest <a href="https://github.com/tiagomendonca/dirf"><img src="man/figures/hex-dirf01.png" align="right" height="175" /></a>

<!-- badges: start -->

<!-- badges: end -->

`dirf` implements two tree-based kernel smoothers—**DiNo (Distance with
Nodes)** and **RanBu (Random Bushes)**—that operate *post hoc* on a
shallow random forest. Both methods reuse the leaf assignments of a
fixed-depth forest (typically ~50 trees, max depth 5) and turn
**tree-based distances** into **similarity weights** through **Gaussian
kernel smoothing weighting** with bandwidth `h`. DiNo relies on the
**MRCA distance**, while RanBu uses **Breiman’s distance (1 − RF
proximity)**.

The implementation builds on forests fitted with the `ranger` package:
once a shallow forest is trained, predictions are obtained without
retraining by weighting training responses according to the kernelized
distances. This supports **conditional expectation (distance-weighted
mean) for point prediction** and extends naturally to **quantile
regression via a weighted empirical CDF** to estimate conditional
quantiles.

## Installation

You can install the development version of `dirf` from Github using
`devtools`:

``` r
# Install devtools if necessary
install.packages("devtools")

# Install dirf
devtools::install_github("tiagomendonca/dirf")
```

**Dependencies:** Ensure you have the following packages installed:
`ranger`, `data.table`, `purrr`, `tibble`, `RcppAlgos`, `ape`, and
`stringr`.

## Examples

Below are examples of how to use the key functionalities of the `dirf`
package.

## Data preparation

``` r
library(dirf)

# Create a training dataset
set.seed(123)

df1 <- tibble::tibble(
  x1 = stats::rnorm(300, 0, 1),
  x2 = stats::rnorm(300, 0, 1),
  x3 = stats::rnorm(300, 0, 1),
  y = 2 * x1 + 1.5 * x2 + x3 + stats::rnorm(300, 0, 1)
)

df2 <- tibble::tibble(
  x1 = stats::rnorm(5, 0, 1),
  x2 = stats::rnorm(5, 0, 1),
  x3 = stats::rnorm(5, 0, 1),
  y = 2 * x1 + 1.5 * x2 + x3 + stats::rnorm(5, 0, 1)
)
```

## Mean prediction

The following example demonstrates how to use **DiNo** and **RanBu** to
predict the mean response:

``` r
# Train a Random Forest model
rf <- ranger::ranger(
  y ~ .,
  max.depth = 5,
  write.forest = TRUE,
  data = df1,
  num.trees = 10
)

# DiNo mean prediction
dino(
  rf = rf,
  df1 = df1,
  y = df1$y,
  df = df2,
  h = 0.1,
  type = "mean"
)
#> # A tibble: 5 × 1
#>   predicted
#>       <dbl>
#> 1      3.14
#> 2     -2.17
#> 3      3.47
#> 4     -4.69
#> 5      3.85

# RanBu mean prediction
ranbu(
  rf = rf,
  df1 = df1,
  y = df1$y,
  df = df2,
  h = 0.1,
  type = "mean"
)
#> # A tibble: 5 × 1
#>   predicted
#>       <dbl>
#> 1      3.12
#> 2     -2.69
#> 3      4.03
#> 4     -4.58
#> 5      4.11
```

## Quantile regression

The next example shows how to predict quantiles using **DiNo** and
**RanBu**:

``` r
# Train a Random Forest model with quantile regression
rf <- ranger::ranger(
  y ~ .,
  max.depth = 5,
  write.forest = TRUE,
  quantreg = TRUE,
  data = df1,
  num.trees = 10
)

# DiNo quantile prediction
dino(
  rf = rf,
  df1 = df1,
  y = df1$y,
  df = df2,
  h = 0.1,
  type = "quantile",
  quantiles = c(.025, .5, .975)
)
#> # A tibble: 5 × 3
#>   q_0.025 q_0.5 q_0.975
#>     <dbl> <dbl>   <dbl>
#> 1   1.72   3.44    3.44
#> 2  -4.73  -4.51   -2.85
#> 3   3.44   3.62    4.12
#> 4  -6.06  -4.45   -3.11
#> 5  -0.179  4.24    4.24

# RanBu quantile prediction
ranbu(
  rf = rf,
  df1 = df1,
  y = df1$y,
  df = df2,
  h = 0.1,
  type = "quantile",
  quantiles = c(.025, .5, .975)
)
#> # A tibble: 5 × 3
#>   q_0.025 q_0.5 q_0.975
#>     <dbl> <dbl>   <dbl>
#> 1    3.00  3.00    3.44
#> 2   -3.35 -3.35   -3.35
#> 3    4.12  4.12    4.12
#> 4   -5.88 -4.45   -4.42
#> 5    1.80  4.24    4.24
```
