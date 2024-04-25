
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabularise - Create tabular outputs from R (using flextable) <a href="https://www.sciviews.org/chart"><img src="man/figures/logo.png" alt="tabularise website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/tabularise/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/tabularise/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/SciViews/tabularise/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SciViews/tabularise?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/tabularise)](https://CRAN.R-project.org/package=tabularise)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

With {tabularise} you should be able to obtain publication-ready
(rich-formatted) tabular output from different R objects. It uses and
enhances the excellent {flextable} package to build these tables and
allow to output them in HTML, LaTeX/PDF, Word or PowerPoint.

## Installation

You can install the development version of {tabularise} from the
[r-universe/sciviews](https://sciviews.r-universe.dev) or
[GitHub](https://github.com/)

- [r-universe/sciviews](https://sciviews.r-universe.dev)

``` r
install.packages("tabularise", repos = "https://sciviews.r-universe.dev")
```

- [GitHub](https://github.com/)

``` r
# install.packages("remotes")
remotes::install_github("SciViews/tabularise")
```

## Short example

Let’s take the well-known iris dataframe, to which we add labels and
units.

``` r
data("iris")

# add labels and units
iris <- data.io::labelise(iris,
  label = list(
    Sepal.Length = "Length of the sepals", Sepal.Width = "Width of the sepals",
    Petal.Length  = "Length of the petals", Petal.Width = "Width of the petals", 
    Species  = "Iris species"),
  units = list(
    Sepal.Length = "cm", Sepal.Width = "cm", 
    Petal.Length  = "cm", Petal.Width = "cm"))
```

Once the labels and units have been added to our initial data,
tabularise will use them to display publication-ready (rich-formatted)
tabular output.

``` r
library(tabularise)
tabularise$headtail(iris) # or tabularise(iris, type = "headtail")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

{tabularise} is a central package on which several other packages in the
SciViews universe. Numerous methods are available for {tabularise} and
they are further enhanced by other packages, such as
{[modelit](https://www.sciviews.org/modelit/)}.

- [modelit](https://www.sciviews.org/modelit/)

## Code of Conduct

Please note that the {tabularise} package is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
