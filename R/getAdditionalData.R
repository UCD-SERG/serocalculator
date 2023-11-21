#' Get Additional Data
#'
#' Retrieves additional data from internet. The data format must be .RDS or a zipped .RDS. The purpose of this
#' function is to download data such as longitudinal response parameters from an online repository or population data.
#'
#' Data for this package is available at: <https://osf.io/ne8pc/files/osfstorage>
#'
#' you can save the data into your chosen directory using the optional savePath argument. specify the file path and the file name
#'
#' ***explain this option to increase time to download:
#' options(timeout = max(300, getOption("timeout")))
#'
#' @param fileURL URL of the file to be downloaded.
#' @param savePath Folder directory and filename to save the downloaded and unzipped (if needed) file. File is saved only
#'   if this argument is not `NULL`. Optional. Default = `NULL`.
#'
#' @return the R object stored in the file indicated by the `fileURL` input
#' Data object
#'
#' @examples
#' \dontrun{
#' curve_param_samples =
#'   getAdditionalData(
#'     fileURL = "https://osf.io/download/bhfvx")
#'
#' # optionally, save the data to disk
#' curve_param_samples =
#'   getAdditionalData(
#'     fileURL = "https://osf.io/download/bhfvx",
#'     savePath = "~/Downloads/curv_params.rds"))
#' }
#'
#' @export
getAdditionalData <- function(
    fileURL,
    savePath = NULL)
{
  fileName <- basename(fileURL)
  tmpFileName <- file.path(tempdir(), fileName)
  on.exit({
    unlink(tmpFileName)
  })
  #Increase timeout for big files
  options(timeout = max(300, getOption("timeout")))
  # Download
  tryCatch(
    {
      download.file(fileURL,
                    tmpFileName,
                    mode = "wb",
                    quiet = TRUE)
    },
    error = function(e) {
      print("There is problem with downloading the requested file. Please, check input arguments or the internet connection.")
    })

  # Unzip
  if (tolower(tools::file_ext(tmpFileName)) == "zip") {
    fileList <- unzip(tmpFileName, list = TRUE)
    if (nrow(fileList) == 1) {
      zipedFileName <- fileList[1, "Name"]
      if (tolower(tools::file_ext(zipedFileName)) == "rds") {
        unzip(tmpFileName, exdir = tempdir())
        unlink(tmpFileName)
        tmpFileName <- file.path(tempdir(), zipedFileName)
      } else {
        stop("Downloaded zip file does not contain an Rds file.")
      }
    } else {
      stop("Downloaded zip file contains multiple files. It should contain a single Rds file.")
    }
  }

  # Store
  if (!is.null(savePath)) {
    pathName <- dirname(savePath)
    dir.create(pathName, showWarnings = FALSE, recursive = TRUE)
    file.copy(tmpFileName, pathName, overwrite = TRUE, recursive = TRUE)
  }

  # Read
  data <- readRDS(tmpFileName)

  return(data)
}
