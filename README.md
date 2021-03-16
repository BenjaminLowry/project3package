
# project3package

<!-- badges: start -->
[![R-CMD-check](https://github.com/BenjaminLowry/project3package/workflows/R-CMD-check/badge.svg)](https://github.com/BenjaminLowry/project3package/actions)
[![codecov](https://codecov.io/gh/BenjaminLowry/project3package/branch/master/graph/badge.svg?token=0JW1T3BHTI)](https://codecov.io/gh/BenjaminLowry/project3package)
<!-- badges: end -->

The R package is a package for my STAT 302 class at UW for Winter Quarter 2021. It contains implementations of 4 statistics functions: t test, linear model fit, k-nearest-neighbors cross validation prediction, and random forest cross validation prediction.

## Installation

You can install this package from GitHub:

``` {r, eval = FALSE}
# install.packages("devtools")
devtools::install_github("BenjaminLowry/project3package", build_vignette = TRUE, build_opts = c())
library(project3package)
```

## Use

This package contains a vignette that gives usage examples for all the functions contained in this package. Below are the ways you can view this vignette:

```{r, eval = FALSE}
# Use this to view the vignette in the project3package HTML help
help(package = "project3package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "project3package")
```



