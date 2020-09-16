# champs

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test coverage](https://codecov.io/gh/EGHI-CHAMPS/champs-L2-statistics/branch/master/graph/badge.svg)](https://codecov.io/gh/EGHI-CHAMPS/champs-L2-statistics?branch=master)
[![R build status](https://github.com/EGHI-CHAMPS/champs-L2-statistics/workflows/R-CMD-check/badge.svg)](https://github.com/EGHI-CHAMPS/champs-L2-statistics/actions)
<!-- badges: end -->

R package that provides utilities to read and transform [CHAMPS](https://champshealth.org) L2 study data into convenient formats, as well as functions to compute several statistics of interest. Also includes some utilities for presenting these statistics in various formats such as plots and HTML tables.

## Installation

You can install this package using the `remotes` package's `install_github()` function.

If you don't already have the `remotes` package, you can install it with:

```r
install.packages("remotes")
```

Then you can install the `champs` package with:

```r
remotes::install_github("EGHI-CHAMPS/champs-L2-statistics")
```
