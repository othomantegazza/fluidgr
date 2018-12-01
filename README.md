# An R Package to Analyze Fluidigm qPCR Data for Gene Expression

This R package loads, normalizes, scales and visualizes Fluidigm qPCR data.

## Install

You can install `fluidgr` from Github with `devtools`:

```r
install.packages("devtools")
devtools::install_github("othomantegazza/fluidgr")
```

## Use

You can read how to use this package in the package vignette.

### Online Documentation

Access here **online** the [full documentation](https://othomantegazza.github.io/fluidgr/) and the [package vignette](https://othomantegazza.github.io/fluidgr/articles/introduction.html) 

### Local

To read the documentation locally you need to: 

- Install all the dependencies stated in the `Suggests` field of the file [DESCRIPTION](DESCRIPTION).
- Install this package specifying the option `build_opts = c("--no-resave-data", "--no-manual")`

```r
devtools::install_github("othomantegazza/fluidgr",
                         build_opts = c("--no-resave-data", "--no-manual"))
```

Then you can access the documentation with:

```r
browseVignettes("fluidgr")
```

## Warranty

This package is under active development and comes with no warranty.

