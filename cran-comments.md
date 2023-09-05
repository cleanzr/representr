## Summary of new changes
* Fix PKGNAME-package \alias as per "Documenting packages" in
R-exts, which was broken as a consequence of the breaking change:

 Using @docType package no longer automatically adds a -package alias.
 Instead document _PACKAGE to get all the defaults for package
 documentation.

## Test environments
* local OS X 12.6 install, R 4.2.1, x86_64
* win-builder (devel and release)
* r-hub Windows Server 2022, R-devel, 64 bit
* r-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r-hub Fedora Linux, R-devel, clang, gfortran
* r-hub Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.




