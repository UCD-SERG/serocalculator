serocalculator package
=====================

------------------------------------------------------------------------

<!-- badges: start -->
[![R-CMD-check](https://github.com/UCD-SERG/serocalculator/workflows/R-CMD-check/badge.svg)](https://github.com/UCD-SERG/serocalculator/actions)
<!-- badges: end -->

Antibody levels measured in a (cross–sectional) population sample can be
translated into an estimate of the frequency with which seroconversions
(infections) occur in the sampled population. Formulated simply: the
presence of many high titres indicates that many subjects likely
experienced infection recently, while low titres indicate a low
frequency of infections in the sampled population.

The serocalculator script was designed to use the longitudinal
response characteristics by means of a set of parameters characterizing
the longitudinal response of the selected serum antibodies.

## Installation

You can install the development version from
[GitHub](https://github.com/) with the following code:

``` r
install.packages("devtools")
devtools::install_github("ucd-serg/serocalculator")
```

**A Note for Windows Users**


  Windows users will need to install Rtools, which contains a collection of tools for building    and employing R packages that are still in development. This can be done either during the      *devtools* package installation, or independently if *devtools* is already installed. 


*During devtools installation:*

When prompted to install additional build tools, select "Yes" and Rtools will be installed. 

![Click Yes to install Rtools along with the *devtools* package][id]

[id]: vignettes/fig/Rtools1.png

*Independently:*
1. Download Rtools from https://cran.r-project.org/bin/windows/Rtools/
2. Run the installer

    * During the Rtools installation you may see a window asking you to “Select Additional Tasks”.
    * Do **not** select the box for “Edit the system PATH”. devtools and RStudio should put Rtools on the PATH automatically when it is needed.
    * **Do** select the box for “Save version information to registry”. It should be selected by default.

## Getting Help

If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/UCD-SERG/serocalculator/issues). 

Another great resource is **The Epidemiologist R Handbook**, which includes an introductory page on asking for help with R packages via GitHub: https://epirhandbook.com/en/getting-help.html 
