#' @docType data
#'
#' @name campylobacterSimLowData
#'
#' @aliases campylobacterSimLowData campylobacterSimMediumData campylobacterSimHighData
#' pertussisSimLowData pertussisSimMediumData pertussisSimHighData
#'
#' @title
#' Simulated Cross-sectional Data
#'
#' @description
#' Simulated cross-sectional population sample of antibody levels data for Campylobacter and
#' Pertussis for lambda 0.036/yr (low), 0.021/yr (medium) and 1.15/yr (high).
#'
#' @usage
#' campylobacterSimLowData
#'
#' @format
#' A data frame with 500 observations on the following 2 to 4 variables:
#' \describe{
#' \item{Age}{}
#' \item{IgG}{}
#' \item{IgM}{}
#' \item{IgA}{}
#' }
#'
#' @examples
#'
#' # Show first rows of the data
#' head(campylobacterSimLowData)
#'
#' # Summarize the data
#' summary(campylobacterSimLowData)
#'
NULL

#' @docType data
#'
#' @name campylobacterDelftParams1
#'
#' @title
#' Campylobacter Delft Response Parameters Data for Model 1
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' campylobacterDelftParams1
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 4000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 4000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 4000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterDelftParams1
#' lapply(campylobacterDelftParams1, head)
#'
NULL

#' @docType data
#'
#' @name campylobacterDelftParams3
#'
#' @title
#' Campylobacter Delft Response Parameters Data for Model 3
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' campylobacterDelftParams3
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 4000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 4000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 4000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterDelftParams3
#' lapply(campylobacterDelftParams3, head)
#'
NULL

#' @docType data
#'
#' @name campylobacterDelftParams4
#'
#' @title
#' Campylobacter Delft Response Parameters Data for Model 4
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' campylobacterDelftParams4
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterDelftParams4
#' lapply(campylobacterDelftParams4, head)
#'
NULL

#' @docType data
#'
#' @name campylobacterSSIParams1
#'
#' @title
#' Campylobacter SSI Response Parameters Data for Model 1
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type
#'
#' @usage
#' campylobacterSSIParams1
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 4000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 4000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 4000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterSSIParams1
#' lapply(campylobacterSSIParams1, head)
#'
NULL

#' @docType data
#'
#' @name campylobacterSSIParams2
#'
#' @title
#' Campylobacter SSI Response Parameters Data for Model 2
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type
#'
#' @usage
#' campylobacterSSIParams2
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterSSIParams2
#' lapply(campylobacterSSIParams2, head)
#'
NULL

#' @docType data
#'
#' @name campylobacterSSIParams4
#'
#' @title
#' Campylobacter SSI Response Parameters Data for Model 4
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' campylobacterSSIParams4
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in campylobacterSSIParams4
#' lapply(campylobacterSSIParams4, head)
#'
NULL

#' @docType data
#'
#' @name pertussisIgGPTParams1
#'
#' @title
#' Pertussis IgG-PT Response Parameters Data for Model 1
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' pertussisIgGPTParams1
#'
#' @format
#' A dataframe `IgG` containing 3000 rows with 7 parameters for IgG antibody.
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in pertussisIgGPTParams1
#' lapply(pertussisIgGPTParams1, head)
#'
NULL

#' @docType data
#'
#' @name pertussisIgGPTParams2
#'
#' @title
#' Pertussis IgG-PT Response Parameters Data for Model 2
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' pertussisIgGPTParams2
#'
#' @format
#' A dataframe `IgG` containing 3000 rows with 7 parameters for IgG antibody.
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in pertussisIgGPTParams2
#' lapply(pertussisIgGPTParams2, head)
#'
NULL

#' @docType data
#'
#' @name pertussisIgGPTParams3
#'
#' @title
#' Pertussis IgG-PT Response Parameters Data for Model 3
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' pertussisIgGPTParams3
#'
#' @format
#' A dataframe `IgG` containing 3000 rows with 7 parameters for IgG antibody.
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in pertussisIgGPTParams3
#' lapply(pertussisIgGPTParams3, head)
#'
NULL

#' @docType data
#'
#' @name pertussisIgGPTParams4
#'
#' @title
#' Pertussis IgG-PT Response Parameters Data for Model 4
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' pertussisIgGPTParams4
#'
#' @format
#' A dataframe `IgG` containing 3000 rows with 7 parameters for IgG antibody.
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in pertussisIgGPTParams4
#' lapply(pertussisIgGPTParams4, head)
#'
NULL

#' @docType data
#'
#' @name salmonellaSSIParams1
#'
#' @title
#' Salmonella SSI Response Parameters Data for Model 1
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' salmonellaSSIParams1
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in salmonellaSSIParams1
#' lapply(salmonellaSSIParams1, head)
#'
NULL

#' @docType data
#'
#' @name salmonellaSSIParams2
#'
#' @title
#' Salmonella SSI Response Parameters Data for Model 2
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' salmonellaSSIParams2
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in salmonellaSSIParams2
#' lapply(salmonellaSSIParams2, head)
#'
NULL

#' @docType data
#'
#' @name salmonellaSSIParams4
#'
#' @title
#' Salmonella SSI Response Parameters Data for Model 4
#'
#' @description
#' List of data frames of all longitudinal parameters. Each data frame contains
#' Monte Carlo samples for each antibody type.
#'
#' @usage
#' salmonellaSSIParams4
#'
#' @format
#' A list of three dataframes:
#' \describe{
#' \item{`IgA`}{A dataframe containing 3000 rows with 7 parameters for IgA antibody.}
#' \item{`IgM`}{A dataframe containing 3000 rows with 7 parameters for IgM antibody.}
#' \item{`IgG`}{A dataframe containing 3000 rows with 7 parameters for IgG antibody.}
#' }
#'
#' @examples
#'
#' # Show first rows of every dataframe contained in salmonellaSSIParams4
#' lapply(salmonellaSSIParams4, head)
#'
NULL
