## Summary of new changes
* Remove dependencies on `blink` and `stringdist` packages

## Test environments
* local OS X install, R 4.1.0
* win-builder (devel and release)
* r-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* r-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r-hub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

** running examples for arch 'x64' ... [17s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
            user system elapsed
pp_weights 10.01    0.2   10.52

## Downstream dependencies
There are currently no downstream dependencies for this package.




