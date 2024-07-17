dmcmc <- load_curve_params("https://osf.io/download/rtw5k") %>%
  slice_head(n = 1000, by = antigen_iso)

xs_data <- "https://osf.io/download//n6cp3/" %>%
  load_pop_data() %>%
  slice_head(n = 1000, by = antigen_iso)
# Load noise params
cond <- tibble(
  antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
  nu = c(0.5, 0.5), # Biologic noise (nu)
  eps = c(0, 0), # M noise (eps)
  y.low = c(1, 1), # low cutoff (llod)
  y.high = c(5e6, 5e6)
) # high cutoff (y.high)

{
est1 = est.incidence(
  pop_data = xs_data,
  curve_params = dmcmc,
  noise_params = cond,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  lambda = 0.1
)
} |> system.time()

library(numDeriv)
{
  temp =
  grad(
  .nll,
  x = est1$est,
  pop_data = xs_data,
  curve_params = dmcmc,
  noise_params = cond,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"))
} |> system.time()

{
hess =
  hessian(
    .nll,
    x = est1$est,
    pop_data = xs_data,
    curve_params = dmcmc,
    noise_params = cond,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"))
} |> system.time()

{
est2 =
  optimize(
   .nll,
   interval = c(-3,3),
     pop_data = xs_data,
   curve_params = dmcmc,
   noise_params = cond,
   antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)} |> system.time()
