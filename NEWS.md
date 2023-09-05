# representr 0.1.5

- Fix PKGNAME-package \alias as per "Documenting packages" in R-exts

# representr 0.1.4

- Fix binary comparison using `||` instead of `|`
- Update error checks to use `is` and `inherits` instead of class comparisons
- Update references to reflect current paper citation

# representr 0.1.3

- Update distance and other internal functions to be Rcpp for speed improvements

# representr 0.1.2

- Remove dependencies on `blink` and `stringdist` packages which are no longer supported on CRAN
- Don't test long running examples


# representr 0.1.1

- Updated to include support for noLD (without long double)

# representr 0.1.0

- First release of `representr`
- Includes functionality for creating representative records post-record linkage
- Support for point prototyping as well as posterior prototyping in `represent` and `pp_weights`
- Function for evaluating representative records against "truth" using empirical KL divergence in `emp_kl`
