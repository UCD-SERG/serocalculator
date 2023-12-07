
#Import longitudinal antibody parameters from OSF
curve_param = getAdditionalData(fileURL ="https://osf.io/download/rtw5k/")


day2yr <- 365.25

tx2 <- 10^seq(-1,3.1,0.025)

bt <- function(y0,y1,t1) log(y1/y0)/t1

ab <- function(t,y0,y1,t1,alpha,shape) {
  beta <- bt(y0,y1,t1);
  yt <- 0;
  if(t <= t1) yt <- y0*exp(beta*t);
  if(t > t1) yt <- (y1^(1-shape)-(1-shape)*alpha*(t-t1))^(1/(1-shape));
  return(yt);
}


d <- curve_param %>%
  mutate(alpha = alpha/day2yr)



dT <- data.frame(t=tx2) %>%
  mutate(ID = 1:n()) %>%
  pivot_wider(names_from = ID, values_from = t, names_prefix = "time") %>%
  slice(rep(1:n(), each = nrow(d)))


serocourse.all <- cbind(d, dT)  %>%
  pivot_longer(
    cols = starts_with("time"),
    values_to = "t") %>%
  select(-name)  %>%
  rowwise() %>%
  mutate(res = ab(t,y0,y1,t1,alpha,r))


serocourse.sum <- serocourse.all %>%
  group_by(antigen_iso, t) %>%
  summarise(res.med  = quantile(res, 0.5),
            res.low  = quantile(res, 0.025),
            res.high = quantile(res, 0.975),
            res.p75 = quantile(res, 0.75),
            res.p25 = quantile(res, 0.25),
            res.p10 = quantile(res, 0.10),
            res.p90 = quantile(res, 0.90)) %>%
  pivot_longer(names_to = "quantile", cols = c("res.med","res.low","res.high", "res.p25", "res.p75", "res.p10", "res.p90"),
               names_prefix = "res.", values_to = "res")



ggplot() +
  geom_line(data=serocourse.sum %>% filter(quantile == "med") ,
            aes(x=t, y=res), linewidth=1) +
  geom_line(data=serocourse.sum %>% filter(quantile == "p10") ,
            aes(x=t, y=res), linewidth=.5) +
  geom_line(data=serocourse.sum %>% filter(quantile == "p90") ,
            aes(x=t, y=res), linewidth=.5) +
  facet_wrap(~antigen_iso, ncol=2) +
  scale_y_log10(limits = c(0.9, 2000), breaks = c(1, 10, 100, 1000), minor_breaks = NULL) +
  theme_minimal()  +
  theme(axis.line=element_line()) +
  labs(x="Days since fever onset", y="ELISA units")



# ggplot() +
#   geom_line(data = serocourse.all, aes(x= t, y = res, group = iter)) +
#   facet_wrap(~antigen_iso, ncol=2) +
#   scale_y_log10(limits = c(0.9, 2000), breaks = c(1, 10, 100, 1000), minor_breaks = NULL) +
#   theme_minimal()  +
#   theme(axis.line=element_line()) +
#   labs(x="Days since fever onset", y="ELISA units")








