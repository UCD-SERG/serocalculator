#' @docType package
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
#' @details
#' For detailed documentation type the following in the R console:
#'
#' `vignette("installation", package = "serocalculator")`
#' `vignette("tutorial", package = "serocalculator")`
#' `vignette("methodology", package = "serocalculator")`
#'
#' @author
#' Author: Peter Teunis \email{p.teunis@@emory.edu}
#' Author: Doug Ezra Morrison \email{demorrison@@ucdavis.edu}
#' Author: Kristen Aiemjoy \email{kaiemjoy@@ucdavis.edu}
#' Author: Kristina Lai \email{kaiemjoy@@ucdavis.edu}
#'
#' @references
#'
#' ***Methods for estimating seroincidence***
#'
#' \itemize{
#' \item Teunis, P. F., van Eijkeren, J. C., Ang, C. W., van Duynhoven, Y. T., Simonsen, J. B., Strid, M. A., van Pelt, W.
#' "Biomarker dynamics: estimating infection rates from serological data"
#' Statistics in Medicine 31, no. 20 (September 9, 2012): 2240--48. doi:10.1002/sim.5322.
#'
#' \item Simonsen, J., Molbak, K., Falkenhorst, G., Krogfelt, K. A., Linneberg, A., Teunis, P. F.
#' "Estimation of incidences of infectious diseases based on antibody measurements"
#' Statistics in Medicine 28, no. 14 (June 30, 2009): 1882--95. doi:10.1002/sim.3592.
#' }
#'
#' ***Applications***
#'
#' \itemize{
#'
#' \item Monge, S., Teunis, P. F., Friesema, I., Franz, E., Ang, W., van Pelt, W., Mughini-Gras, L.
#' "Immune response-eliciting exposure to Campylobacter vastly exceeds the incidence of clinically
#' overt campylobacteriosis but is associated with similar risk factors: A nationwide serosurvey in the Netherlands"
#' Journal of Infection, 2018, 1--7, doi:10.1016/j.jinf.2018.04.016
#'
#' \item Kretzschmar, M., Teunis, P. F., Pebody, R. G.
#' "Incidence and reproduction numbers of pertussis: estimates from serological and social contact data in five European countries"
#' PLoS Medicine 7, no. 6 (June 1, 2010):e1000291. doi:10.1371/journal.pmed.1000291.
#'
#' \item Simonsen, J., Strid, M. A., Molbak, K., Krogfelt, K. A., Linneberg, A., Teunis, P.
#' "Sero-epidemiology as a tool to study the incidence of Salmonella infections in humans"
#' Epidemiology and Infection 136, no. 7 (July 1, 2008): 895--902. doi:10.1017/S0950268807009314
#'
#' \item Simonsen, J., Teunis, P. F., van Pelt, W., van Duynhoven, Y., Krogfelt, K. A., Sadkowska-Todys, M., Molbak K.
#' "Usefulness of seroconversion rates for comparing infection pressures between countries"
#' Epidemiology and Infection, April 12, 2010, 1--8. doi:10.1017/S0950268810000750.
#'
#' \item Falkenhorst, G., Simonsen, J., Ceper, T. H., van Pelt, W., de Valk, H., Sadkowska-Todys, M., Zota, L., Kuusi, M., Jernberg, C., Rota, M. C., van Duynhoven, Y. T., Teunis, P. F., Krogfelt, K. A., Molbak, K.
#' "Serological cross-sectional studies on salmonella incidence in eight European countries: no correlation with incidence of reported cases"
#' BMC Public Health 12, no. 1 (July 15, 2012): 523--23. doi:10.1186/1471-2458-12-523.
#'
#' \item Teunis, P. F., Falkenhorst, G., Ang, C. W., Strid, M. A., De Valk, H., Sadkowska-Todys, M., Zota, L., Kuusi, M., Rota, M. C., Simonsen, J. B., Molbak, K., Van Duynhoven, Y. T., van Pelt, W.
#' "Campylobacter seroconversion rates in selected countries in the European Union"
#' Epidemiology and Infection 141, no. 10 (2013): 2051--57. doi:10.1017/S0950268812002774.
#'
#' \item de Melker, H. E., Versteegh, F. G., Schellekens, J. F., Teunis, P. F., Kretzschmar, M.
#' "The incidence of Bordetella pertussis infections estimated in the population from a combination of serological surveys"
#' The Journal of Infection 53, no. 2 (August 1, 2006): 106--13. doi:10.1016/j.jinf.2005.10.020
#' }
#'
#' ***Quantification of seroresponse***
#'
#' \itemize{
#' \item de Graaf, W. F., Kretzschmar, M. E., Teunis, P. F., Diekmann, O.
#' "A two-phase within-host model for immune response and its application to serological profiles of pertussis"
#' Epidemics 9 (2014):1--7. doi:10.1016/j.epidem.2014.08.002.
#'
#' \item Berbers, G. A., van de Wetering, M. S., van Gageldonk, P. G., Schellekens, J. F., Versteegh, F. G., Teunis, P.F.
#' "A novel method for evaluating natural and vaccine induced serological responses to Bordetella pertussis antigens"
#' Vaccine 31, no. 36 (August 12, 2013): 3732--38. doi:10.1016/j.vaccine.2013.05.073.
#'
#' \item Versteegh, F. G., Mertens, P. L., de Melker, H. E., Roord, J. J., Schellekens, J. F., Teunis, P. F.
#' "Age-specific long-term course of IgG antibodies to pertussis toxin after symptomatic infection with Bordetella pertussis"
#' Epidemiology and Infection 133, no. 4 (August 1, 2005): 737--48.
#'
#' \item Teunis, P. F., van der Heijden, O. G., de Melker, H. E., Schellekens, J. F., Versteegh, F. G., Kretzschmar, M. E.
#' "Kinetics of the IgG antibody response to pertussis toxin after infection with B. pertussis"
#' Epidemiology and Infection 129, no. 3 (December 10, 2002):479. doi:10.1017/S0950268802007896.
#' }
#'
#'

## usethis namespace: start
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr anti_join
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr semi_join
#' @importFrom dplyr tibble
#' @importFrom magrittr %>%
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel parLapplyLB
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom stats dlnorm optim pgamma plnorm
#' @importFrom stats nlm
#' @importFrom stats qnorm
#' @importFrom tibble as_tibble
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom utils download.file unzip
#' @useDynLib serocalculator, .registration = TRUE
## usethis namespace: end
NULL
