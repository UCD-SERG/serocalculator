#Elisa data
library(dplyr)
library(forcats)
library(readr)
d0 <- read.csv(
  "inst/extdata/SEES_2022-10-24_redacted_2023-10-12.csv",
  header=T) %>%
  #filter(antigen != "CdtB" | antigen != "YncE") %>%
  dplyr::filter(studyarm!= "highE_hh",
         studyarm!= "lowE_hh",
         studyarm!="ae control") %>%
  mutate(
    antigen_iso =
      paste(elisa_antigen, "_", elisa_antbdy_iso, sep="") %>%
      factor(),
    ageCat = cut(
      Age,
      breaks= c(0, 4.99, 15.99, 99),
      right=FALSE, labels = c("<5", "5-15", "16+")),
    TimePeriod =
      TimePeriod %>%
      factor(levels = c("Baseline","28 days","3 months","6 months", "12 months", "18 months", "First visit")),
    Arm2 =
      Arm %>%
      fct_collapse(Cases = c("Prospective Cases", "Retrospective Cases")) %>%
      factor(
        levels = c("Cases", "Population-based"),
        labels = c("Cases", "Population sample")),
    sex = sex %>% as.factor() %>% fct_collapse(NULL = c("97"))) %>%
  mutate(Gender = factor(sex, labels = c("Male", "Female"))) %>%
  dplyr::filter(Age<=25) %>%
  dplyr::filter(catchment!="matiari") %>%
  dplyr::filter(catchment!="mirzapur") %>%
  mutate(cluster = areaunit3) %>%
  droplevels() %>%
  dplyr::filter(Arm2 == "Population sample" & TimePeriod == "Baseline") %>%
  select(Country, cluster, catchment, Age, ageCat, antigen_iso, result) %>%
  mutate(cluster = factor(cluster)) %>%
  dplyr::filter(antigen_iso %in% c("HlyE_IgG", "HlyE_IgA")) %>%
  as_tibble() %>%
  select(-cluster)

  sees_crossSectional_baseline_allCountries = d0
use_data(sees_crossSectional_baseline_allCountries, overwrite =  TRUE)
write_csv(
  d0,
  fs::path(
          "inst/extdata",
          paste0(
            "sees_crossSectional_baseline_allCountries",
            ".102523.csv")))

