serocalculator
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

<!-- badges: start -->

[![R-CMD-check](https://github.com/UCD-SERG/serocalculator/workflows/R-CMD-check/badge.svg)](https://github.com/UCD-SERG/serocalculator/actions)
<!-- badges: end -->

Antibody levels measured in a cross–sectional population sample can be
translated into an estimate of the frequency with which seroconversions
(infections) occur in the sampled population. In other words, the
presence of many high antibody titers indicates that many individuals
likely experienced infection recently and the burden of disease is high
in the population, while low titers indicate a low frequency of
infections in the sampled population and therefore a lower burden of
disease.

The **serocalculator** package was designed to use the longitudinal
response characteristics using a set of modeled parameters
characterizing the longitudinal response of the selected serum
antibodies.

## Methods

The **serocalculator** R package provides a rapid and computationally
simple method for calculating seroconversion rates, as originally
published in Simonsen et al. (2009) and Teunis et al. (2012), and
further developed in subsequent publications by deGraaf et al. (2014),
Teunis et al. (2016), and Teunis and Eijkeren (2020). In short,
longitudinal seroresponses from confirmed cases with a known symptom
onset date are assumed to represent the time course of human serum
antibodies against a specific pathogen. Therefore, by using these
longitudinal antibody dynamics with any cross–sectional sample of the
same antibodies in a human population, an incidence estimate can be
calculated. Further details are below.

### A Proxy for Infection

While the exact time of infection is impossible to measure in an
individual, antibody levels measured in a cross–sectional population
sample can be translated into an estimate of the frequency with which
seroconversions (infections) occur in the sampled population. So the
presence of many high antibody concentrations indicates that many people
in the population likely experienced infection recently, while mostly
low concentrations indicate a low frequency of infections in the sampled
population.

In order to interpret the measured cross-sectional antibody
concentrations in terms of incidence, we must define the antibody
dynamic over time to understand the generalized antibody response at
different times since infection. This dynamic must be quantified over
time to include an initial increase in serum antibody concentration when
seroconversion occurs, followed by a gradual decrease as antibodies
wane. In published studies, this information on the time course of the
serum antibody response has been obtained from longitudinal follow–up
data in cases who had a symptomatic episode following infection. In this
case, the onset of symptoms then provides a proxy for the time that
infection occurred.

### The Seroincidence Estimator

The **serocalculator** package was designed to calculate the incidence
of seroconversion by using the longitudinal seroresponse
characteristics. The distribution of serum antibody concentrations in a
cross–sectional population sample is calculated as a function of the
longitudinal seroresponse and the frequency of seroconversion (or
seroincidence). Given the seroresponse, this marginal distribution of
antibody concentrations can be fitted to the cross-sectional data and
thereby providing a means to estimate the seroincidence.

## Installing R

The **serocalculator** package is written in R, a free, open-source
software program. The end user of this package must have access to a
working installation of the R software. We recommend installing [base
R](https://cran.r-project.org/) and a Graphical User Interfaces (GUI)
for R such as [RStudio](http://www.rstudio.com/products/RStudio/).

If you need to download and install R and/or RStudio, we recommend
following the tutorial below from *Hands On Programming in R* by Garrett
Grolemund:

**Installing R and RStudio**:
<https://rstudio-education.github.io/hopr/starting.html>

## Installing the Serocalculator Package

The **serocalculator** package must be installed in R before first use.
As of November 21, 2023, **serocalculator** is still in development. To
install the development version, you must install the **devtools** R
package and then download **serocalculator** from
[GitHub](https://github.com/). Enter the code below into the R console
to install both packages:

``` r
# Install the devtools package and the development version of serocalculator
install.packages("devtools")
devtools::install_github("ucd-serg/serocalculator")
```

### Post-installation

Successful installation can be confirmed by loading the package into the
RStudio workspaceand exploring help files and manuals distributed with
the package:

``` r
# Load package "seroincidence".
library(serocalculator)

# Show R help for the package.
?serocalculator
```

Additionally, most package details can be found when executing the
following commands:

``` r
# Show description.
packageDescription("serocalculator")

# Show citation.
citation("serocalculator")
```

### A Note for Windows Users

Windows users will need to install Rtools, which contains a collection
of tools for building and employing R packages that are still in
development. This can be done either during the *devtools* package
installation, or independently if *devtools* is already installed.

#### During devtools installation:

When prompted to install additional build tools, select “Yes” and Rtools
will be installed.

<figure>
<img src="man/figures/Rtools1.png"
alt="Click Yes to install Rtools along with the devtools package" />
<figcaption aria-hidden="true">Click Yes to install Rtools along with
the <em>devtools</em> package</figcaption>
</figure>

#### Independently:

1.  Download Rtools from
    <https://cran.r-project.org/bin/windows/Rtools/>

2.  Run the installer

    - During the Rtools installation you may see a window asking you to
      “Select Additional Tasks”.
    - Do **not** select the box for “Edit the system PATH”. devtools and
      RStudio should put Rtools on the PATH automatically when it is
      needed.
    - **Do** select the box for “Save version information to registry”.
      It should be selected by default.

## Getting Help

If you need assistance or encounter a clear bug, please file an issue
with a minimal reproducible example on
[GitHub](https://github.com/UCD-SERG/serocalculator/issues).

Another great resource is **The Epidemiologist R Handbook**, which
includes an introductory page on asking for help with R packages via
GitHub: <https://epirhandbook.com/en/getting-help.html>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-de_Graaf_2014" class="csl-entry">

deGraaf, W. F., M. E. E. Kretzschmar, P. F. M. Teunis, and O. Diekmann.
2014. “A Two-Phase Within-Host Model for Immune Response and Its
Application to Serological Profiles of Pertussis.” *Epidemics* 9
(December): 1–7. <https://doi.org/10.1016/j.epidem.2014.08.002>.

</div>

<div id="ref-Simonsen_2009" class="csl-entry">

Simonsen, J., K. Mølbak, G. Falkenhorst, K. A. Krogfelt, A. Linneberg,
and P. F. M. Teunis. 2009. “Estimation of Incidences of Infectious
Diseases Based on Antibody Measurements.” *Statistics in Medicine* 28
(14): 1882–95. <https://doi.org/10.1002/sim.3592>.

</div>

<div id="ref-Teunis_2020" class="csl-entry">

Teunis, P. F. M., and J. C. H. van Eijkeren. 2020. “Estimation of
Seroconversion Rates for Infectious Diseases: Effects of Age and Noise.”
*Statistics in Medicine* 39 (21): 2799–2814.
<https://doi.org/10.1002/sim.8578>.

</div>

<div id="ref-Teunis_2016" class="csl-entry">

Teunis, P. F. M., J. C. H. van Eijkeren, W. F. de Graaf, A. Bonačić
Marinović, and M. E. E. Kretzschmar. 2016. “Linking the Seroresponse to
Infection to Within-Host Heterogeneity in Antibody Production.”
*Epidemics* 16 (September): 33–39.
<https://doi.org/10.1016/j.epidem.2016.04.001>.

</div>

<div id="ref-Teunis_2012" class="csl-entry">

Teunis, P. F. M., JCH van Eijkeren, CW Ang, YTHP van Duynhoven, JB
Simonsen, MA Strid, and W van Pelt. 2012. “Biomarker Dynamics:
Estimating Infection Rates from Serological Data.” *Statistics in
Medicine* 31 (20): 2240–48. <https://doi.org/10.1002/sim.5322>.

</div>

</div>
