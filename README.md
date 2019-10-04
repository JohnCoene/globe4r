
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JohnCoene/globe4r.svg?branch=master)](https://travis-ci.org/JohnCoene/globe4r)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/globe4r?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/globe4r)
<!-- badges: end -->

# globe4r

Interactive globes for R via
[globe.gl](https://github.com/vasturiano/globe.gl).

Visit the [website](https://globe4r.john-coene.com) for more details.

<img src="./man/figures/logo.png" height="250" align="right" />

1.  bars
2.  arcs
3.  polygons
4.  points
5.  hex bin

Visit the website for the [full list of
functions](https://globe4r.john-coene.com/reference/)

## Installation

You can install the globe4r from Github:

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/globe4r")
```

## Example

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
