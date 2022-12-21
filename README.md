
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parseC5

<!-- badges: start -->
<!-- badges: end -->

The goal of parseC5 is to parse rules from fitted C5.0 models

## Installation

You can install the development version of parseC5 like so:

``` r
remotes::install_github("emilhvitfeldt/parseC5")
```

## Example

``` r
library(parseC5)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(C50)

data(ames, package = "modeldata")

ad_mod <- C5.0(Street ~ ., data = ames, trials = 10)

res <- parse_model(ad_mod)

res
#> # A tibble: 17 × 4
#>    tree   node rule       freqs    
#>    <chr> <int> <list>     <list>   
#>  1 1         1 <language> <int [2]>
#>  2 1         2 <language> <int [2]>
#>  3 1         3 <language> <int [2]>
#>  4 1         4 <language> <int [2]>
#>  5 2         1 <language> <int [3]>
#>  6 2         2 <language> <int [4]>
#>  7 2         3 <language> <int [3]>
#>  8 3         1 <language> <int [4]>
#>  9 3         2 <language> <int [4]>
#> 10 3         3 <language> <int [4]>
#> 11 3         4 <language> <int [4]>
#> 12 3         5 <language> <int [4]>
#> 13 4         1 <language> <int [4]>
#> 14 4         2 <language> <int [4]>
#> 15 4         3 <language> <int [4]>
#> 16 4         4 <language> <int [4]>
#> 17 4         5 <language> <int [4]>

res |>
  filter(tree == "1") |>
  pull(rule)
#> [[1]]
#> MS_Zoning %in% c("Floating_Village_Residential", "Residential_High_Density", 
#>     "Residential_Low_Density", "Residential_Medium_Density", 
#>     "A_agr")
#> 
#> [[2]]
#> MS_Zoning %in% c("C_all", "I_all") & Longitude <= -93.610268
#> 
#> [[3]]
#> MS_Zoning %in% c("C_all", "I_all") & Longitude > -93.610268 & 
#>     Latitude <= 42.022625
#> 
#> [[4]]
#> MS_Zoning %in% c("C_all", "I_all") & Longitude > -93.610268 & 
#>     Latitude > 42.022625

res |>
  filter(tree == "1") |>
  pull(freqs)
#> [[1]]
#> [1]    6 2897
#> 
#> [[2]]
#> [1]  0 16
#> 
#> [[3]]
#> [1] 6 0
#> 
#> [[4]]
#> [1] 0 5
```
