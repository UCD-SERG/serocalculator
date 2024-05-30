.makeStrata <- function(data, strata = "") {
  dataStrata <- data

  if (all(strata != "")) {
    if (all(strata %in% names(data))) {
      dataStrata$Stratum <- interaction(dataStrata[, strata])
    } else {
      return(dataStrata) # no stratum variable
    }
  } else {
    dataStrata$Stratum <- factor("all data")
  }
  return(dataStrata)
}
