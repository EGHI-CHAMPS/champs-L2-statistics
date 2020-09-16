
# champs

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test coverage](https://codecov.io/gh/EGHI-CHAMPS/champs-L2-statistics/branch/master/graph/badge.svg)](https://codecov.io/gh/EGHI-CHAMPS/champs-L2-statistics?branch=master)
[![R build status](https://github.com/EGHI-CHAMPS/champs-L2-statistics/workflows/R-CMD-check/badge.svg)](https://github.com/EGHI-CHAMPS/champs-L2-statistics/actions)
<!-- badges: end -->

This website documents the "champs" R package. This package provides utilities to read and transform [CHAMPS](https://champshealth.org) L2 study data into convenient formats, functions to compute several statistics of interest, and some utilities for presenting these statistics in various formats such as plots and HTML tables.

## Installation

You can install this package using the `remotes` package's `install_github()` function. If you don't already have the `remotes` package, you can install it with:

```r
install.packages("remotes")
```

Then you can install the `champs` package with:

```r
remotes::install_github("EGHI-CHAMPS/champs-L2-statistics")
```

## Documentation

There are several documents that will help you get acquainted with CHAMPS data and how to use the functionality of this package to analyze it. These can be accessed using the "Articles" menu at the top of this website.

- [CHAMPS Introduction](articles/data_intro.html)
- [Downloading CHAMPS data](articles/download.html)
- [Loading CHAMPS data and understanding its structure](articles/data_load.html)
- [Computing summaries](articles/usage.html)
- [Plots and tables](articles/plots_tables.html)
- [Deep dive into a calc function](articles/calc_deepdive.html)
- [Deep dive custom analysis](articles/custom_analyses_h_influenzae.html)

A reference for all of the functions in this package can be found [here](reference/index.html) or by clicking the "Reference" link at the top of this website.

