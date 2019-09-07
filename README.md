
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->
globe4r
=======

Interactive globes for R via [globe.gl](https://github.com/vasturiano/globe.gl).

Visit the [website](https://globe4r.john-coene.com) for more details.

<img src="./man/figures/logo.png" height="250" align="right" />

Installation
------------

You can install the globe4r from Github:

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/globe4r")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(globe4r)

data("population") # sample data

create_globe() %>% # initialise
  globe_bars(
    coords(lat, lon, altitude = value, color = value),
    data = population
  ) %>% 
  scale_bars_altitude() %>% 
  scale_bars_color()
```
