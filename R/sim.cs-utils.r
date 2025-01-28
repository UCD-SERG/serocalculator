
# calculate a few additional variables needed for the simulation
# parameter vector: c(y0,b0,mu0,mu1,c1,alpha,shape)
afunc <- function(par) (par[4, ] - par[3, ]) / (par[5, ] * par[1, ])
bfunc <- function(par) par[4, ] / (par[4, ] - par[3, ])
t1func <- function(par) log(1 + afunc(par) * par[2, ]) / (par[4, ] - par[3, ])
y1func <- function(par) par[1, ] * (1 + afunc(par) * par[2, ])^(bfunc(par))

# symptomatic (1) or asymptomatic (0) seroconversion?
symp <- function(par) {
  y0 <- par["y0", ]
  b0 <- par["b0", ]
  mu0 <- par["mu0", ]
  c1 <- par["c1", ]
  ymin <- mu0 * b0 / c1
  return(as.numeric(y0 <= ymin))
}

# the function f() linking pre- and post- antibody levels
transf <- function(y0, par) {
  b0 <- par["b0", ]
  mu0 <- par["mu0", ]
  mu1 <- par["mu1", ]
  c1 <- par["c1", ]
  cc1 <- mu1 / (mu1 - mu0)
  cc2 <- (mu1 - mu0) * b0 / c1
  return(y0 * (1 + cc2 / y0)^cc1)
}
