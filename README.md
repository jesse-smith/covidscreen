
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidtest

<!-- badges: start -->

[![CI-CD](https://github.com/jesse-smith/covidtest/workflows/CI-CD/badge.svg)](https://github.com/jesse-smith/covidtest/actions)
[![Codecov test
coverage](https://codecov.io/gh/jesse-smith/covidtest/branch/master/graph/badge.svg)](https://codecov.io/gh/jesse-smith/covidtest?branch=master)
<!-- badges: end -->

{covidtest} is an R package and Shiny app designed to help organizations
evaluate their risk from COVID-19 and the potential benefits of regular
asymptomatic testing.

## Installation

You can install the development version of {covidtest} from Github with:

``` r
if (!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("jesse-smith/covidtest")
```

If you are using R on Windows, you will need to first install Rtools
[here](https://cran.r-project.org/bin/windows/Rtools/).

## Example

``` r
library(covidtest)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
```

To run the Shiny app, you can use `run_app()` or visit an [online
version](https://jesse-shiny.shinyapps.io/covidtest/).

To access the underlying model in **R**, you can use `ct_dist()` with
the desired parameters. Running with no inputs uses the defaults; the
output is a `data.table`:

## Code of Conduct

Please note that the {covidtest} project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.