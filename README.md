
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SticsRtests

A package to make integration tests for the SticsRPacks packages

-----

<!-- badges: start -->

[![R-CMD-check](https://github.com/SticsRPacks/SticsRTests/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsRTests/actions)
<!-- badges: end -->

This package is used to test the functions from all the distributed R
packages from SticsRPacks for tests that cannot be integrated directly
in the packages because they have to use external programs such as STICS
or JavaSTICS (not distributed freely).

The package uses the `testthat` package to make all tests, a version of
JavaSTICS and STICS from an always up-to-date google drive, and the
[data repository](https://github.com/SticsRPacks/data) from the
SticsRPacks organisation.

To test the packages functions all at once, simply press `ctrl+shift+T`,
or execute this command in the R console:

``` r
testthat::test_package()
```

## Useage

To use this package, follow these steps:

  - clone it from Github (<https://github.com/SticsRPacks/data.git>);

  - download JavaSTICS and place it at the root of the package, with the
    name of type “JavaSTICS-v85” (for version 8.5 here);

  - download the data by executing these commands in the R console:

<!-- end list -->

``` r
data_dir= file.path(getwd(),"data")
dir.create(data_dir)
data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
unzip(data_dir_zip, exdir = data_dir)
unlink(data_dir_zip)
```

The data is now in the data folder, folder “data-master”. For exemple
the data `study_case_1` for the version 9 of the model is located at:

``` r
data_dir= file.path("data-master","study_case_1","V9.0")
data_dir
#> [1] "data-master/study_case_1/V9.0"
```

## Further information

For more information about unit testing, please read [this
vignette](https://sticsrpacks.github.io/sandbox/articles/make-a-package.html#test-your-functions)
in the `sandbox project`.
