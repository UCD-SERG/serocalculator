#' Create bar plot for stratified seroincidence results
#'
#'
#' @return a [ggplot2::ggplot()] object

autoplot.seroincidence.by.bar <- function(est_country_agedf, ageCat, incidence.rate, Country, CI.lwr, CI.upr, country_pal) {
  ggplot(est_country_agedf,
         aes(
           y = fct_rev(ageCat),
           x = incidence.rate * 1000, #rescale incidence
           fill = Country
         )) +
    geom_bar(stat = "identity",
             position = position_dodge2(reverse = TRUE),
             show.legend = TRUE) +
    geom_errorbar(
      aes(xmin = CI.lwr * 1000, xmax = CI.upr * 1000), #rescale CIs
      position = position_dodge2(reverse = TRUE)
    ) +
    labs(title = "Enteric Fever Seroincidence by Country and Age",
         x = "Seroincidence rate per 1000 person-years",
         y = "Age Category",
         fill = "Country") +
    theme_linedraw() +
    theme(axis.text.y = element_text(size = 11),
          axis.text.x = element_text(size = 11)) +
    scale_x_continuous(expand = c(0, 10)) +
    scale_fill_manual(values = country_pal)
}
