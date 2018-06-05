#' Get Additional Data
#'
#' Retrieves additional data from internet. This can be any file type, but the purpose of this
#' function is to download data such as longitudinal response parameters from an online repository.
#'
#' @param fileName Name of the file to download. Required.
#' @param repoURL Web address of the remote repository of files to download from. Required.
#'   Default = \code{"http://ecdc.europa.eu/sites/portal/files/documents"}
#' @param savePath Folder to save the downloaded and unzipped (if needed) file. File is saved only
#'   if this argument is not \code{NULL}. Optional. Default = \code{NULL}.
#'
#' @return
#' Data object
#'
#' @examples
#'
#' \dontrun{
#' getAdditionalData(fileName = "coxiellaIFAParams4.zip")
#' getAdditionalData(fileName = "yersiniaSSIParams4.zip")
#' getAdditionalData(fileName = "coxiellaIFAParams4.zip", savePath = getwd())
#' getAdditionalData(fileName = "yersiniaSSIParams4.zip", savePath = getwd())
#' }
#'
#' @export
getAdditionalData <- function(
  fileName,
  repoURL = "http://ecdc.europa.eu/sites/portal/files/documents",
  savePath = NULL)
{
  tmpFileName <- file.path(tempdir(), fileName)
  on.exit({
    unlink(tmpFileName)
  })

  # Download
  tryCatch({
    download.file(file.path(repoURL, fileName),
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
    dir.create(savePath, showWarnings = FALSE, recursive = TRUE)
    file.copy(tmpFileName, savePath, overwrite = TRUE, recursive = TRUE)
  }

  # Read
  data <- readRDS(tmpFileName)

  return(data)
}
