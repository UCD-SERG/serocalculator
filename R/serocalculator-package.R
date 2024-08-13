#' _PACKAGE
#'
#' @name serocalculator
#'
#' @aliases serocalculator serocalculator-package
#'
#' @title
#' Estimating Infection Rates from Serological Data
#'
#' @description
#' This package translates antibody levels measured in a (cross-sectional) population sample into an
#' estimate of the frequency with which seroconversions (infections) occur in the sampled population.
#'
#' The API for this package includes the following functions:
#'
#' Function Name                          | Purpose
#' -----------------                      | --------------------------------------------
#' [load_pop_data()]                      | loading cross-sectional antibody survey data
#' [check_pop_data()]                     | checking antibody data
#' [summary.pop_data()]                   | numerical summaries of antibody data
#' [autoplot.pop_data()]                  | graphs of antibody data distributions
#' [load_curve_params()]                  | loading antibody decay curve models
#' [autoplot.curve_params()]              | graphing antibody decay curves
#' [log_likelihood()]                               | computing log-likelihoods
#' [graph.loglik()]                       | graphing log-likelihood functions
#' [autoplot.seroincidence()]             | graphing log-likelihood functions
#' [autoplot.seroincidence.by()]          | graphing log-likelihood functions
#' [est.incidence()]                      | estimating incidence rates
#' [est.incidence.by()]                   | estimating incidence rates by strata
#' [summary.seroincidence.by()]           | summarizing stratified incidence rate estimates
#' [autoplot.summary.seroincidence.by()]  | graphing incidence rate estimates
#' [sim.cs()]                             | simulating cross-sectional population antibody data using longitudinal seroresponse models
#'
#'
#' @author
#' * Peter Teunis \email{p.teunis@@emory.edu}
#' * Doug Ezra Morrison \email{demorrison@@ucdavis.edu}
#' * Kristen Aiemjoy \email{kaiemjoy@@ucdavis.edu}
#' * Kristina Lai \email{kwlai@@ucdavis.edu}
#'
#' @references
#'
#' ***Methods for estimating seroincidence***
#'
#' * Teunis, P. F. M., and J. C. H. van Eijkeren. "Estimation of seroconversion rates for infectious diseases: Effects of age and noise." Statistics in Medicine 39, no. 21 (2020): 2799-2814.
#'
#' * Teunis, P. F. M., J. C. H. van Eijkeren, W. F. de Graaf, A. Bonačić Marinović, and M. E. E. Kretzschmar. "Linking the seroresponse to infection to within-host heterogeneity in antibody production." Epidemics 16 (2016): 33-39.
#'
#'
#' ***Applications***
#'
#' * Aiemjoy, Kristen, Jessica C. Seidman, Senjuti Saha, Sira Jam Munira, Mohammad Saiful Islam Sajib, Syed Muktadir Al Sium, Anik Sarkar et al. "Estimating typhoid incidence from community-based serosurveys: a multicohort study." The Lancet Microbe 3, no. 8 (2022): e578-e587.
#'
#' * Aiemjoy, Kristen, John Rumunu, Juma John Hassen, Kirsten E. Wiens, Denise Garrett, Polina Kamenskaya, Jason B. Harris et al. "Seroincidence of enteric fever, Juba, South Sudan." Emerging infectious diseases 28, no. 11 (2022): 2316.
#'
#' * Monge, S., Teunis, P. F., Friesema, I., Franz, E., Ang, W., van Pelt, W., Mughini-Gras, L.
#' "Immune response-eliciting exposure to Campylobacter vastly exceeds the incidence of clinically
#' overt campylobacteriosis but is associated with similar risk factors: A nationwide serosurvey in the Netherlands"
#' Journal of Infection, 2018, 1--7, doi:10.1016/j.jinf.2018.04.016
#'
#' * Kretzschmar, M., Teunis, P. F., Pebody, R. G.
#' "Incidence and reproduction numbers of pertussis: estimates from serological and social contact data in five European countries"
#' PLoS Medicine 7, no. 6 (June 1, 2010):e1000291. doi:10.1371/journal.pmed.1000291.
#'
#' * Simonsen, J., Strid, M. A., Molbak, K., Krogfelt, K. A., Linneberg, A., Teunis, P.
#' "Sero-epidemiology as a tool to study the incidence of Salmonella infections in humans"
#' Epidemiology and Infection 136, no. 7 (July 1, 2008): 895--902. doi:10.1017/S0950268807009314
#'
#' * Simonsen, J., Teunis, P. F., van Pelt, W., van Duynhoven, Y., Krogfelt, K. A., Sadkowska-Todys, M., Molbak K.
#' "Usefulness of seroconversion rates for comparing infection pressures between countries"
#' Epidemiology and Infection, April 12, 2010, 1--8. doi:10.1017/S0950268810000750.
#'
#' * Falkenhorst, G., Simonsen, J., Ceper, T. H., van Pelt, W., de Valk, H., Sadkowska-Todys, M., Zota, L., Kuusi, M., Jernberg, C., Rota, M. C., van Duynhoven, Y. T., Teunis, P. F., Krogfelt, K. A., Molbak, K.
#' "Serological cross-sectional studies on salmonella incidence in eight European countries: no correlation with incidence of reported cases"
#' BMC Public Health 12, no. 1 (July 15, 2012): 523--23. doi:10.1186/1471-2458-12-523.
#'
#' * Teunis, P. F., Falkenhorst, G., Ang, C. W., Strid, M. A., De Valk, H., Sadkowska-Todys, M., Zota, L., Kuusi, M., Rota, M. C., Simonsen, J. B., Molbak, K., Van Duynhoven, Y. T., van Pelt, W.
#' "Campylobacter seroconversion rates in selected countries in the European Union"
#' Epidemiology and Infection 141, no. 10 (2013): 2051--57. doi:10.1017/S0950268812002774.
#'
#' * de Melker, H. E., Versteegh, F. G., Schellekens, J. F., Teunis, P. F., Kretzschmar, M.
#' "The incidence of Bordetella pertussis infections estimated in the population from a combination of serological surveys"
#' The Journal of Infection 53, no. 2 (August 1, 2006): 106--13. doi:10.1016/j.jinf.2005.10.020
#'
#'
#' ***Quantification of seroresponse***
#'
#' * de Graaf, W. F., Kretzschmar, M. E., Teunis, P. F., Diekmann, O.
#' "A two-phase within-host model for immune response and its application to serological profiles of pertussis"
#' Epidemics 9 (2014):1--7. doi:10.1016/j.epidem.2014.08.002.
#'
#' * Berbers, G. A., van de Wetering, M. S., van Gageldonk, P. G., Schellekens, J. F., Versteegh, F. G., Teunis, P.F.
#' "A novel method for evaluating natural and vaccine induced serological responses to Bordetella pertussis antigens"
#' Vaccine 31, no. 36 (August 12, 2013): 3732--38. doi:10.1016/j.vaccine.2013.05.073.
#'
#' * Versteegh, F. G., Mertens, P. L., de Melker, H. E., Roord, J. J., Schellekens, J. F., Teunis, P. F.
#' "Age-specific long-term course of IgG antibodies to pertussis toxin after symptomatic infection with Bordetella pertussis"
#' Epidemiology and Infection 133, no. 4 (August 1, 2005): 737--48.
#'
#' * Teunis, P. F., van der Heijden, O. G., de Melker, H. E., Schellekens, J. F., Versteegh, F. G., Kretzschmar, M. E.
#' "Kinetics of the IgG antibody response to pertussis toxin after infection with B. pertussis"
#' Epidemiology and Infection 129, no. 3 (December 10, 2002):479. doi:10.1017/S0950268802007896.
#'
#'

## usethis namespace: start
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr anti_join
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_all
#' @importFrom dplyr if_any
#' @importFrom dplyr inner_join
#' @importFrom dplyr is.grouped_df
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr pull
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr semi_join
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom foreach %:%
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 autoplot
#' @importFrom ggplot2 ggplot
#' @importFrom lifecycle deprecated
#' @importFrom magrittr %>%
#' @importFrom mixtools normalmixEM
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rngtools RNGseq
#' @importFrom rngtools setRNG
#' @importFrom stats dlnorm optim pgamma plnorm
#' @importFrom stats formula
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats nlm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats runif
#' @importFrom stats xtabs
#' @importFrom tibble as_tibble
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect contains
#' @importFrom tidyselect ends_with
#' @importFrom utils download.file unzip
#' @importFrom utils tail
#' @useDynLib serocalculator, .registration = TRUE
## usethis namespace: end
NULL
