

#serocalculator: scrub typhus demo



#load packages
library(Hmisc)
library(tidyverse)
library(doParallel)
registerDoParallel(detectCores()-1)

library(serocalculator)

#load longitudinal params
dmcmc <- readRDS("~/Dropbox/DataAnalysis/ScrubTyphus/longitudinal/v5/output/mcmc.rds") %>%
  mutate(alpha = alpha/365.25)


##load population data
#created in file "Scrub_dataPrep.R

dpop <-  readRDS("~/Dropbox/DataAnalysis/ScrubTyphus/Source Data/scrub_dpop_05.02.22.AllAge.rds")  %>%
  mutate(areaunt2 = factor(areaunt2),
         country = as.factor(country)) %>%
  select(index_id, country, Age, IgG_result, IgM_result) %>%
  pivot_longer(cols = c("IgG_result", "IgM_result"), names_to = "antigen_iso", values_to = "value") %>%
  mutate(antigen_iso = factor(antigen_iso, levels = c("IgM_result", "IgG_result"), labels = c("IgM", "IgG")))


##NOISE PARAMETERS
#table(dpopL$country)
#proxy for biologic noise
# b <- dpop %>% filter(value < kitcut) %>%
#   filter(Age<30) %>%
#   #group_by(antigen_iso, country) %>%
#   group_by(antigen_iso) %>%
#   summarise(nu = quantile(value, .90, na.rm = T))

# define global parameters
cond <- data.frame(cbind(c("IgG","IgM"),
                         #c(0.357,0.37),  # Biologic noise (nu)
                         # c(1.14,0.48),  # Biologic noise (nu)
                         #c(as.numeric(b[2,2]),as.numeric(b[1,2])),  # Biologic noise (nu)
                         c(0.3,0.3),  # Biologic noise (nu)
                         c(0.2,0.2),  # M noise (eps)
                         c(0.1,0.1),  # low cutoff (llod)
                         c(200,200))) # high cutoff (y.high)


names(cond) <- c("antigen_iso", "nu", "eps", "llod", "y.high")

cond <- cond %>% mutate(nu = as.numeric(nu), eps = as.numeric(eps), llod = as.numeric(llod), y.high = as.numeric(y.high))



### Estimate incidence
est.incidence(dpop = dpop %>%
                      rename(y = value,
                       a = Age) %>%
                 filter(country == "India"),
                 dmcmc = dmcmc,
               noise_params = cond)


est.incidence(dpop = dpop %>%
                rename(y = value,
                       a = Age) %>%
                filter(country == "India"),
              dmcmc = dmcmc,
              noise_params = cond,
              antigen_isos = c("IgG"))

est.incidence.by(dpop %>%
                   rename(y = value,
                          a = Age),
                 dmcmc,
                 cond,
                 antigen_isos = c("IgM", "IgG"),
                 strata = "country")





