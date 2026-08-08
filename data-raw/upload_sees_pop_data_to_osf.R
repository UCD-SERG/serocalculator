# Upload the corrected SEES cross-sectional data to OSF.

osfr::osf_auth()

if (!requireNamespace("osfr", quietly = TRUE)) {
  stop("Install osfr before running this script: install.packages('osfr')")
}

project_id <- "ne8pc"
file_guid <- "n6cp3"
parent_names <- c("Enteric Fever", "Cross-sectional Population data")
remote_name <- "sees_crossSectionalPopulation_baseline_allCountries.rds"
local_path <- "vignettes/precomputed/osf/n6cp3.rds"

if (!file.exists(local_path)) {
  stop("Run this script from the serocalculator repository root.")
}

project <- osfr::osf_retrieve_node(project_id)
enteric_folder <- osfr::osf_ls_files(project)
enteric_folder <- enteric_folder[enteric_folder$name == parent_names[[1]], ]

if (nrow(enteric_folder) != 1L) {
  stop("Could not resolve the expected Enteric Fever folder on OSF.")
}

target_folder <- osfr::osf_ls_files(enteric_folder)
target_folder <- target_folder[target_folder$name == parent_names[[2]], ]

if (nrow(target_folder) != 1L) {
  stop("Could not resolve the expected cross-sectional data folder on OSF.")
}

existing_file <- osfr::osf_ls_files(target_folder)
existing_file <- existing_file[existing_file$name == remote_name, ]

if (
  nrow(existing_file) != 1L ||
    existing_file$meta[[1]]$attributes$guid != file_guid
) {
  stop("The existing OSF file does not match the expected filename and GUID.")
}

upload_dir <- tempfile("serocalculator-osf-upload-")
dir.create(upload_dir)
upload_path <- file.path(upload_dir, remote_name)

if (!file.copy(local_path, upload_path)) {
  stop("Could not prepare the corrected RDS for upload.")
}

version_before <- existing_file$meta[[1]]$attributes$current_version
osfr::osf_upload(
  target_folder,
  upload_path,
  conflicts = "overwrite",
  progress = TRUE
)

uploaded_file <- osfr::osf_retrieve_file(file_guid)
download_dir <- tempfile("serocalculator-osf-verify-")
dir.create(download_dir)
downloaded_file <- osfr::osf_download(
  uploaded_file,
  path = download_dir,
  conflicts = "overwrite",
  progress = TRUE
)

local_raw <- readBin(local_path, what = "raw", n = file.info(local_path)$size)
remote_path <- downloaded_file$local_path[[1]]
remote_raw <- readBin(remote_path, what = "raw", n = file.info(remote_path)$size)

if (!identical(local_raw, remote_raw)) {
  stop("OSF verification failed: the downloaded file differs from the upload.")
}

cli::cli_inform(
  c(
    "v" = "Uploaded and verified OSF file {.code {file_guid}}.",
    "i" = "Version: {version_before} -> {uploaded_file$meta[[1]]$attributes$current_version}"
  )
)
