# representr

[![Travis-CI Build Status](https://travis-ci.org/cleanzr/representr.svg?branch=master)](https://travis-ci.org/cleanzr/representr)

Create representative records post-record linkage for use in downstream tasks after record linkage is performed. Multiple methods for creating the records are provided, including two methods based on the posterior distribution of linkage resulting from a Bayesian analysis.

## Installation

```r
devtools::install_github("cleanzr/representr", build_vignettes = TRUE)
```

## Citation

This package implements the methods introduced in the following paper:

> Kaplan, Andee, Brenda Betancourt, and Rebecca C. Steorts. "Posterior Prototyping: Bridging the Gap between Bayesian Record Linkage and Regression." arXiv preprint arXiv:1810.01538 (2020).


## Background

Record linkage (entity resolution or de-duplication) is used to join multiple databases to remove duplicate entities. While record linkage removes the duplicate entities from the data, many researchers are interested in performing inference, prediction, or post-linkage analysis on the linked data (e.g., regression or capture-recapture), which we call the *downstream task*. Depending on the downstream task, one may wish to find the most representative record before performing the post-linkage analysis. For example, when the values of features used in a downstream task differ for linked data, which values should be used? This is where `representr` comes in.

## Main functions

The two main functions in `representr` are `represent` and `pp_weights`, which perform pointwise and fully Bayesian prototyping, respectively. Additionally, we have added a function aid in the evaluation of prototyping methods by estimating an empirical KL divergence through the function `emp_kl`. To read more about the specific prototyping functions available, see the help pages.

```r
help(representr)
```

For more extensive documentation of the use of this package, please see the vignette.

```r
vignette("representr")
```

## Acknowledgments

This work was partially supported by the National Science Foundation through NSF-1652431 and NSF-1534412 and the [Laboratory for Analytic Sciences at NC State University](https://ncsu-las.org).



