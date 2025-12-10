# Introduction to serocalculator

## Overview

The **serocalculator** R package provides a rapid and computationally
simple method for calculating seroconversion rates, as originally
published in Simonsen et al. (2009) and Teunis, Eijkeren, et al. (2012),
and further developed in deGraaf et al. (2014), Teunis et al. (2016),
and Teunis and Eijkeren (2020). In short, longitudinal seroresponses
from confirmed cases with a known symptom onset date are assumed to
represent the time course of human serum antibodies against a specific
pathogen. Therefore, by using these longitudinal antibody dynamics with
any cross–sectional sample of the same antibodies in a human population,
an incidence estimate can be calculated. Further details are below.

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

### The Serocalculator App

The **serocalculator app** is a web based tool that takes the 5 curve
parameters (y0, y1, t1, alpha, and r) to draw a single curve on antibody
concentration.

## Further reading

### Methods for estimating seroincidence

- Teunis and Eijkeren (2020)
- Teunis et al. (2016)

### Applications

- Aiemjoy et al. (2022)
- Aiemjoy, Rumunu, and Juma John Hassen (2022)
- Monge et al. (2018)
- Kretzschmar, Teunis, and Pebody (2010)
- Simonsen et al. (2007)
- Simonsen et al. (2010)
- Falkenhorst et al. (2012)
- Teunis, Falkenhorst, et al. (2012)
- Demelker et al. (2006)

### Quantification of seroresponse

- deGraaf et al. (2014)
- Berbers et al. (2013)
- Versteegh et al. (2005)
- Teunis et al. (2002)

## References

Aiemjoy, K., Seidman J. C., Saha S., Munira S. J., Islam Sajib M. S.,
and Sarkar Sium S. M. al. 2022. “Estimating Typhoid Incidence from
Community-Based Serosurveys: A Multicohort Study.” *The Lancet Microbe*
3 (8): e578–87. <https://doi.org/10.1016/S2666-5247(22)00114-8>.

Aiemjoy, K., John Rumunu, and Denise Garrett Juma John Hassen Kirsten E.
Wiens. 2022. “Seroincidence of Enteric Fever,juba, South Sudan.”
*Emerging Infectious Diseases* 28 (11): 2316–20.
<https://doi.org/10.3201/eid2811.220239>.

Berbers, G. A. M., M. S. E. van de Wetering, P. G. M. van Gageldonk, J.
F. P. Schellekens, F. G. A. Versteegh, and P. F. M. Teunis. 2013. “A
Novel Method for Evaluating Natural and Vaccine Induced Serological
Responses to Bordetella Pertussis Antigens.” *Vaccine* 31 (36): 3732–38.
<https://doi.org/10.1016/j.vaccine.2013.05.073>.

deGraaf, W. F., M. E. E. Kretzschmar, P. F. M. Teunis, and O. Diekmann.
2014. “A Two-Phase Within-Host Model for Immune Response and Its
Application to Serological Profiles of Pertussis.” *Epidemics* 9
(December): 1–7. <https://doi.org/10.1016/j.epidem.2014.08.002>.

Demelker, H, F Versteegh, J Schellekens, P Teunis, and M Kretzschmar.
2006. “The Incidence of Bordetella Pertussis Infections Estimated in the
Population from a Combination of Serological Surveys.” *Journal of
Infection* 53 (2): 106–13. <https://doi.org/10.1016/j.jinf.2005.10.020>.

Falkenhorst, Gerhard, Jacob Simonsen, Tina H Ceper, Wilfrid van Pelt,
Henriette de Valk, Malgorzata Sadkowska-Todys, Lavinia Zota, et al.
2012. “Serological Cross-Sectional Studies on Salmonella Incidence in
Eight European Countries: No Correlation with Incidence of Reported
Cases.” *BMC Public Health* 12 (1).
<https://doi.org/10.1186/1471-2458-12-523>.

Kretzschmar, Mirjam, Peter F. M. Teunis, and Richard G. Pebody. 2010.
“Incidence and Reproduction Numbers of Pertussis: Estimates from
Serological and Social Contact Data in Five European Countries.” Edited
by Megan Murray. *PLoS Medicine* 7 (6): e1000291.
<https://doi.org/10.1371/journal.pmed.1000291>.

Monge, Susana, Peter Teunis, Ingrid Friesema, Eelco Franz, Wim Ang,
Wilfrid van Pelt, and Lapo Mughini-Gras. 2018. “Immune
Response-Eliciting Exposure to Campylobacter Vastly Exceeds the
Incidence of Clinically Overt Campylobacteriosis but Is Associated with
Similar Risk Factors: A Nationwide Serosurvey in the Netherlands.”
*Journal of Infection* 77 (3): 171–77.
<https://doi.org/10.1016/j.jinf.2018.04.016>.

Simonsen, J., K. Mølbak, G. Falkenhorst, K. A. Krogfelt, A. Linneberg,
and P. F. M. Teunis. 2009. “Estimation of Incidences of Infectious
Diseases Based on Antibody Measurements.” *Statistics in Medicine* 28
(14): 1882–95. <https://doi.org/10.1002/sim.3592>.

Simonsen, J., M. A. Strid, K. Mølbak, K. A. Krogfelt, A. Linneberg, and
P. Teunis. 2007. “Sero-Epidemiology as a Tool to Study the Incidence of
Salmonella Infections in Humans.” *Epidemiology and Infection* 136 (7):
895–902. <https://doi.org/10.1017/s0950268807009314>.

Simonsen, J., P. Teunis, W. van Pelt, Y. Van Duynhoven, K. A. Krogfelt,
M. Sadkowska-Todys, and K. Mølbak. 2010. “Usefulness of Seroconversion
Rates for Comparing Infection Pressures Between Countries.”
*Epidemiology and Infection* 139 (4): 636–43.
<https://doi.org/10.1017/s0950268810000750>.

Teunis, P. F. M., and J. C. H. van Eijkeren. 2020. “Estimation of
Seroconversion Rates for Infectious Diseases: Effects of Age and Noise.”
*Statistics in Medicine* 39 (21): 2799–2814.
<https://doi.org/10.1002/sim.8578>.

Teunis, P. F. M., J. C. H. van Eijkeren, W. F. de Graaf, A. Bonačić
Marinović, and M. E. E. Kretzschmar. 2016. “Linking the Seroresponse to
Infection to Within-Host Heterogeneity in Antibody Production.”
*Epidemics* 16 (September): 33–39.
<https://doi.org/10.1016/j.epidem.2016.04.001>.

Teunis, P. F. M., JCH van Eijkeren, CW Ang, YTHP van Duynhoven, JB
Simonsen, MA Strid, and W van Pelt. 2012. “Biomarker Dynamics:
Estimating Infection Rates from Serological Data.” *Statistics in
Medicine* 31 (20): 2240–48. <https://doi.org/10.1002/sim.5322>.

Teunis, P. F. M., G. Falkenhorst, C. W. Ang, M. A. Strid, H. de Valk, M.
Sadkowksa-Todys, L. Zota, et al. 2012. “Campylobacterseroconversion
Rates in Selected Countries in the European Union.” *Epidemiology and
Infection* 141 (10): 2051–57.
<https://doi.org/10.1017/s0950268812002774>.

Teunis, P. F. M., O. G. van der Heijden, H. E. de Melker, J. F. P.
Schellekens, F. G. A. Versteegh, and M. E. E. Kretzshmar. 2002.
“Kinetics of the IgG Antibody Response to Pertussis Toxin After
Infection with b. Pertussis.” *Epidemiology and Infection* 129 (3):
479–89. <https://doi.org/10.1017/s0950268802007896>.

Versteegh, F. G. A., P. L. J. M. Mertens, H. E. de Melker, J. J. Roord,
J. F. P. Schellekens, and P. F. M. Teunis. 2005. “Age-Specific Long-Term
Course of IgG Antibodies to Pertussis Toxin After Symptomatic Infection
with Bordetella Pertussis.” *Epidemiology and Infection* 133 (4):
737–48. <https://doi.org/10.1017/s0950268805003833>.
