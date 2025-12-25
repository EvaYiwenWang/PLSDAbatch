
<!-- PLEASE DO NOT EDIT ./README.md BY HAND, EDIT ./inst/README.Rmd AND RENDER TO CREATE ./README.md -->

[![](https://img.shields.io/github/last-commit/EvaYiwenWang/PLSDAbatch.svg)](https://github.com/EvaYiwenWang/PLSDAbatch/commits/master)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/github/languages/code-size/EvaYiwenWang/PLSDAbatch.svg)](https://github.com/EvaYiwenWang/PLSDAbatch)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# PLSDAbatch

A multivariate and non-parametric batch effect correction framework
based on Projection to Latent Structures Discriminant Analysis for
microbiome data. This repository contains the `R` package hosted on
Bioconductor.

## Maintainer

[Yiwen (Eva) Wang](mailto:yiwen.wang@unimelb.edu.au)

## Installation

(**macOS users only:** Ensure you have installed
[XQuartz](https://www.xquartz.org/) first.)

Make sure you have the latest R version and the latest `BiocManager`
package installed following [these
instructions](https://www.bioconductor.org/install/).

``` r
## install BiocManager if not installed
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
```

Ensure the following returns `TRUE`, or follow the guidelines provided
by the output.

``` r
BiocManager::valid()
```

#### a) Latest `Bioconductor` Release

You can install `PLSDAbatch` using the following code:

``` r
BiocManager::install('PLSDAbatch')
```

#### b) `GitHub` Version

Install the GitHub version with:

``` r
# without vignette
BiocManager::install("EvaYiwenWang/PLSDAbatch") 

# with vignette
## Install CRAN packages for vignette
cran_pkgs <- c("pheatmap", "vegan")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

## Install Bioconductor packages for vignette
bioc_pkgs <- c("Biobase", "SummarizedExperiment")
to_install <- bioc_pkgs[!bioc_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) BiocManager::install(to_install)

devtools::install_github("https://github.com/EvaYiwenWang/PLSDAbatch", build_vignettes = T)
```

## Functions preview

``` r
library(PLSDAbatch)

## names
ls('package:PLSDAbatch')

## names and details
lsf.str('package:PLSDAbatch')
```

## Vignette View

``` r
browseVignettes("PLSDAbatch")
```

## Bug reports/Feature requests

[Open a new issue](https://github.com/EvaYiwenWang/PLSDAbatch/issues).

## Reference

Wang, Y., & Lê Cao, K. A. (2023). PLSDA-batch: a multivariate framework
to correct for batch effects in microbiome data. Briefings in
Bioinformatics, 24(2), bbac622.

<https://academic.oup.com/bib/article/24/2/bbac622/6991121> (The
mentioned simulations and analyses in the paper are separately stored
[here](https://evayiwenwang.github.io/PLSDAbatch_workflow/).)

## What’s New

#### April 2023

- submitted to Bioconductor.

#### September 2024

- fixed bugs: the clash of functions from dependencies.

#### December 2025

- Added a `mode` argument to `PLSDA_batch()`.
- Added a `criterion` argument to `linear_regres()` to select P-values
  from the optimal model based on the specified criterion.
- Added a `return.model` arugument to `linear_regres()` to reduce memory
  usage when set to `FALSE`.
- Extended `Scatter_Density()` to support any multivariate method that
  returns component scores, including PCA and PLS, with corresponding
  arguments updated.
- Added `lighten()` and `darken()` functions for enhanced color
  generation.
- Refined multiple functions to improve usability.
- Updated the vignette accordingly.
