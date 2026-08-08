# ELISA data
#
# The input contains sensitive identifiers and must not be committed. Point
# SEES_REDACTED_DATA at the redacted source extract before running this script.
sees_source_path <- Sys.getenv("SEES_REDACTED_DATA")
if (!nzchar(sees_source_path)) {
  stop("Set SEES_REDACTED_DATA to the redacted SEES source extract.")
}

d0 <- readr::read_csv(
  sees_source_path,
  col_types = readr::cols(
    .default = readr::col_skip(),
    studyarm = readr::col_character(),
    elisa_antigen = readr::col_character(),
    elisa_antbdy_iso = readr::col_character(),
    Age = readr::col_double(),
    TimePeriod = readr::col_character(),
    Arm = readr::col_character(),
    catchment = readr::col_character(),
    areaunit3 = readr::col_character(),
    sid = readr::col_character(),
    result = readr::col_double(),
    Country = readr::col_character()
  ),
  show_col_types = FALSE
) |>
  dplyr::filter(
    studyarm != "highE_hh",
    studyarm != "lowE_hh",
    studyarm != "ae control"
  ) |>
  dplyr::mutate(
    antigen_iso =
      paste(elisa_antigen, "_", elisa_antbdy_iso, sep = "") |>
        factor(),
    ageCat = cut(
      Age,
      breaks = c(0, 4.99, 15.99, 99),
      right = FALSE, labels = c("<5", "5-15", "16+")
    ),
    cluster = areaunit3
  ) |>
  dplyr::filter(Age <= 25) |>
  dplyr::filter(catchment != "matiari") |>
  dplyr::filter(catchment != "mirzapur") |>
  dplyr::filter(Arm == "Population-based" & TimePeriod == "Baseline") |>
  dplyr::select(
    sid,
    Country,
    cluster,
    catchment,
    Age,
    ageCat,
    antigen_iso,
    result
  ) |>
  dplyr::mutate(cluster = factor(cluster)) |>
  dplyr::filter(antigen_iso %in% c("HlyE_IgG", "HlyE_IgA")) |>
  droplevels() |>
  tibble::as_tibble()

if (anyNA(d0$sid)) {
  stop("SEES subject identifiers must not be missing.")
}

country_prefixes <- substr(unique(d0$Country), 1L, 1L)
if (anyDuplicated(country_prefixes)) {
  stop("Country initials must be unique before creating public subject IDs.")
}

# Preserve within-subject biomarker linkage without exposing the source key.
d0 <-
  d0 |>
  dplyr::group_by(Country) |>
  dplyr::mutate(
    index_id = paste0(substr(Country, 1L, 1L), match(sid, unique(sid)))
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    index_id,
    Country,
    cluster,
    catchment,
    Age,
    ageCat,
    antigen_iso,
    result
  )

readr::write_rds(d0, "vignettes/precomputed/osf/n6cp3.rds")
