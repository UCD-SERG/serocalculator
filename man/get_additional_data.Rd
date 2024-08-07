% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_additional_data.R
\name{get_additional_data}
\alias{get_additional_data}
\title{Get Additional Data}
\usage{
get_additional_data(fileURL, savePath = NULL)
}
\arguments{
\item{fileURL}{URL of the file to be downloaded.}

\item{savePath}{Folder directory and filename to save the downloaded and unzipped (if needed) file. File is saved only
if this argument is not \code{NULL}. Optional. Default = \code{NULL}.}
}
\value{
the R object stored in the file indicated by the \code{fileURL} input
Data object
}
\description{
Retrieves additional data from internet. The data format must be .RDS or a zipped .RDS. The purpose of this
function is to download data such as longitudinal response parameters from an online repository or cross-sectional population data.

Data for this package is available at: \url{https://osf.io/ne8pc/files/osfstorage}

You can save the data into your chosen directory using the optional savePath argument. Specify the file path and the file name.

Large datasets may timeout. If so, you can increase the download time by updating the maximum timeout time in the code below. (Ex: increase from 300 to 1000)
\code{options(timeout = max(300, getOption("timeout")))}
}
\examples{
\dontrun{
curve_param_samples <-
  get_additional_data(
    fileURL = "https://osf.io/download/bhfvx"
  )

# optionally, save the data to disk
curve_param_samples <-
  get_additional_data(
    fileURL = "https://osf.io/download/bhfvx",
    savePath = "~/Downloads/curv_params.rds"
  )
}

}
