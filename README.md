serocalculator package
=====================

------------------------------------------------------------------------

<!-- badges: start -->
[![R-CMD-check](https://github.com/UCD-SERG/serocalculator/workflows/R-CMD-check/badge.svg)](https://github.com/UCD-SERG/serocalculator/actions)
<!-- badges: end -->


Antibody levels measured in a cross–sectional population sample can be translated into an estimate of the frequency with which seroconversions (infections) occur in the sampled population. IN other words, the presence of many high antibody titres indicates that many individuals likely experienced infection recently and the burden of disease is high in the population, while low titres indicate a low frequency of infections in the sampled population and therefore a lower burden of disease.

The **serocalculator** package was designed to use the longitudinal response characteristics using a set of modeled parameters characterizing the longitudinal response of the selected serum antibodies. 

## Installing R

Package **serocalculator** is written in R and the end user must have access to a working installation of R engine. This document describes the most common setup with R installed 
locally on the user's computer. We recommend installing base R and a Graphical User Interfaces for R like e.g. 
[RStudio](http://www.rstudio.com/products/RStudio/).

R is a free software program and can be downloaded from [http://cran.r-project.org/](http://cran.r-project.org/).
After downloading the appropriate version for your computer's operating system, install R on your computer following the standard procedure applicable to the operating system. For Windows the file to be downloaded is the so-called *base* distribution: [http://cran.r-project.org/bin/windows/base/](http://cran.r-project.org/bin/windows/base/).

### R Installation Instructions

Start the R installer and follow the presented steps:

![][setup1]

[setup1]: vignettes/fig/setup1.png

It is advised to have R installed in folder that does not contain spaces, therefore please adjust 
the destination location accordingly:

![][setup2]

[setup2]: vignettes/fig/setup2.png

The **serocalculator** package is compatible with both the 32-bit and the 64-bit version of R. 
Choose the preferred platform (or both). If unsure, install the 32-bit version only, however
on compatible devices the 64-bit version may provide better performance:

![][setup3]

[setup3]: vignettes/fig/setup3.png


It is advised to select *Registry entries* in the next step for best experience:

![][setup4]

[setup4]: vignettes/fig/setup4.png


R interpreter, when installed on Windows, can be invoked from the start menu folder named **R**.

Start the preferred version of R (if both the 32-bit: *R i386* and the 64-bit: *x64* are installed). Graphical user interface for R interpreter will start in a new window:


![][RGui1]

[RGui1]: vignettes/fig/RGui1.png


## Installing the Serocalculator R Package

Since this is a new installation of R, the **serocalculator** package must be installed before first use. As of November 21, 2023, **serocalculator** is still in development. To install the development version, you must install the **devtools** package and then download **serocalculator** from [GitHub](https://github.com/). 


```{eval=FALSE}
# install package "devtools"
install.packages("devtools")
devtools::install_github("ucd-serg/serocalculator")

```

### Post-installation

Successful installation can be confirmed by loading the package into the RStudio workspace 
and exploring help files and manuals distributed with the package:

```r{eval=FALSE}
# Load package "seroincidence".
library(serocalculator)

# Show R help for the package.
?serocalculator

# Show tutorial for the package.
vignette(topic = "entericfevertutorial", package = "serocalculator")
```

Additionally, most package details can be found when executing the following commands:

```r{eval=FALSE}
# Show description.
packageDescription("serocalculator")

# Show citation.
citation("serocalculator")
```

### A Note for Windows Users

  Windows users will need to install Rtools, which contains a collection of tools for building and employing R packages that are still in development. This can be done either during the  *devtools* package installation, or independently if *devtools* is already installed. 


#### During devtools installation:

When prompted to install additional build tools, select "Yes" and Rtools will be installed. 

![Click Yes to install Rtools along with the *devtools* package][id]

[id]: vignettes/fig/Rtools1.png

#### Independently:

1. Download Rtools from https://cran.r-project.org/bin/windows/Rtools/
2. Run the installer

    * During the Rtools installation you may see a window asking you to “Select Additional Tasks”.
    * Do **not** select the box for “Edit the system PATH”. devtools and RStudio should put Rtools on the PATH automatically when it is needed.
    * **Do** select the box for “Save version information to registry”. It should be selected by default.

## Getting Help

If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/UCD-SERG/serocalculator/issues). 

Another great resource is **The Epidemiologist R Handbook**, which includes an introductory page on asking for help with R packages via GitHub: https://epirhandbook.com/en/getting-help.html 
