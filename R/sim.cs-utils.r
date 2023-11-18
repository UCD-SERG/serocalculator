# take a random sample from longitudinal parameter set
# given age at infection, for a  list of antibodies (ablist in 1:7) and
# nmc in 1:4000
ldpar <- function(age,ablist,nmc,npar,...){
  spar <- array(NA,dim=c(2+npar,length(ablist))); # 2 additional parameters
  for(k.test in 1:length(ablist))
    spar[,k.test] <- simpar(age,ablist[k.test],nmc, ...);
  return(spar); # parameter vector: c(y0,b0,mu0,mu1,c1,alpha,shape)
}

# calculate a few additional variables needed for the simulation
# parameter vector: c(y0,b0,mu0,mu1,c1,alpha,shape)
afunc  <- function(par) (par[4,]-par[3,])/(par[5,]*par[1,])
bfunc  <- function(par) par[4,]/(par[4,]-par[3,])
t1func <- function(par) log(1+afunc(par)*par[2,])/(par[4,]-par[3,])
y1func <- function(par) par[1,]*(1+afunc(par)*par[2,])^(bfunc(par))

# kinetics of the bacteria (ag: antigen) response
ag <- function(t,par) {
  t1 <- t1func(par);
  y0 <- par[1,]; b0 <- par[2,]; mu0 <- par[3,]; mu1 <- par[4,]; c1 <- par[5,];
  bt <- array(0,dim=c(length(t),ncol(par)));
  for(k in 1:ncol(par)){
    u <- (t<=t1[k]);
    bt[u,k] <- b0[k] * exp(mu0[k]*t[u]) -
      c1[k] * y0[k] *(exp(mu0[k] * t[u]) - exp(mu1[k] * t[u]))/
      (mu0[k] - mu1[k]);
  }
  return(bt);
}

# kinetics of the antibody (ab) response (power function decay)
ab <- function(t,par,...) {
  t1 <- t1func(par); y1 <- y1func(par);
  y0 <- par[1,]; mu1 <- par[4,];
  alpha <- par[6,]; shape <- par[7,];
  yt <- array(0,dim=c(length(t),ncol(par)));
  for(k in 1:ncol(par)){
    u <- (t <= t1[k]); d <- (t > t1[k]);
    yt[u,k] <- y0[k] * exp(mu1[k] * t[u]);
    if(shape[k]!=1)
      yt[d,k] <- (y1[k]^(1 - shape[k]) -
                    (1-shape[k]) * alpha[k] * (t[d] - t1[k]))^(1/(1 - shape[k]));
    if(shape[k]==1) yt[d,k] <- y1[k] * exp(-alpha[k] * t[d]);
    yt[,k] <- baseline(k,yt[,k], ...);
  }
  return(yt);
}

# symptomatic (1) or asymptomatic (0) seroconversion?
symp <- function(par){
  y0 <- par[1,]; b0 <- par[2,]; mu0 <- par[3,]; c1 <- par[5,];
  ymin <- mu0*b0/c1;
  return(as.numeric(y0 <= ymin));
}

# the function f() linking pre- and post- antibody levels
transf <- function(y0,par){
  b0 <- par[2,]; mu0 <- par[3,]; mu1 <- par[4,]; c1 <- par[5,];
  cc1 <- mu1/(mu1-mu0);
  cc2 <- (mu1-mu0)*b0/c1;
  return(y0*(1+cc2/y0)^cc1);
}

# whenever y is below a cutoff (blims[kab,2]) substitute a random sample
# from a baseline distribution
baseline <- function(kab,yvec, blims, ...){
  subst <- which(yvec < blims[kab,2]); k <- 1;
  while(k <= length(subst)){
    yvec[subst[k]] <- mkbaseline(kab,1, blims);
    k <- k + 1;
  }
  return(yvec);
}

# generate random sample from baseline distribution
mkbaseline <- function(kab,n=1, blims, ...){
  # yset <- rlnorm(n=1,meanlog=negpar[1],sdlog=negpar[2]);
  if(blims[kab,2]==0){
    yset <- rep(0,n);
  }else{
    yset <- runif(n=n,min=blims[kab,1],max=blims[kab,2]);
  }
  return(yset);
}

# simulate kinetics of y over interval t = (0,t.end) given
# seroconversion rate lambda (1/days),
# parameter estimates for fixed age (age.fx in years) or not
# when age.fx = NA then age at infection is used.
# responses calculated  for list of antibodies ablist (1:7, may be any subset)
# a posterior sample may be selected n.mc (1:4000), or not
# when n.mc = 0 a posterior sample is chosen at random.
# At infection, a new parameter sample may be generated (renew.params = TRUE)
# when renew.params = FALSE, a sample is generated at birth and kept,
# but baseline y0 are carried over from prior infections.
# This function returns a list with:
# t = times (in days, birth at day 0),
# b = bacteria level, for each antibody signal (not used; probably meaningless),
# y = antibody level, for each antibody signal
# smp = whether an infection involves a big jump or a small jump
# t.inf = times when infections have occurred.
simresp.tinf = function(
    lambda,
    t.end,
    age.fx,
    ablist,
    n.mc = 0,
    renew.params,
    predpar,
    ...)
{
  mcsize <- dim(predpar)[3]
  nmc <- n.mc

  day2yr <- 365.25;

  if (n.mc == 0)
    nmc <- sample.int(n = mcsize, size = 1)

  n.ab <- length(ablist)

  t0 <- 0
  t <- c()
  b <- c()
  y <- c()
  t.step <- 1
  smp <- c()
  t.inf <- c()

  #set.seed(975313579)
  t.next <- -log(runif(1, 0, 1)) / lambda # time to first infection...
  if (!is.na(age.fx))
    mcpar <- ldpar(age.fx, ablist, nmc, predpar = predpar, ...)

  if (is.na(age.fx))
    mcpar <- ldpar(t.next / day2yr, ablist, nmc, predpar = predpar, ...)

  par.now <- mcpar

  b.inf <- mcpar[2, ] # b0
  b.end <- rep(0, n.ab)

  if (t.next > t.end)
    t.next <- t.end - t0

  if (t.next < t.end)
    t.inf <- c(t.inf, t.next)

  t.now <- seq(from = 0, to = t.next, by = t.step)
  b.now <- array(0, dim = c(length(t.now), n.ab))

  y.now <- array(0, dim = c(length(t.now), n.ab))

  for (k.ab in 1:n.ab)
    y.now[, k.ab] <- mkbaseline(k.ab, length(t.now), ...)

  t <- c(t, t0 + t.now)

  b <- rbind(b, b.now)
  y <- rbind(y, y.now)

  y.end <- as.matrix(y.now)[nrow(y.now), ]

  if (n.ab == 1 && y.end == 0)
    y.end <- par.now[1]

  if (n.ab > 1)
    y.end[y.end == 0] <- par.now[1, y.end == 0]

  while (t0 < t.end - t.next) {
    t0 <- t0 + t.next

    if (!renew.params) {
      if (!is.na(age.fx))
        par.now <- ldpar(age.fx, ablist, nmc, predpar = predpar, ...)

      if (is.na(age.fx))
        par.now <- ldpar(t0 / day2yr, ablist, nmc, predpar = predpar, ...)

      b0 <- b.inf
      # b0 <- runif(n=1,min=1,max=200); not implemented
      par.now[1, ] <- y.end
      # y0 = y at end of prior episode
    }
    t.next <- -log(runif(1, 0, 1)) / lambda
    if (t0 <= t.end & t0 + t.next > t.end)
      t.next <- t.end - t0

    if (t0 + t.next < t.end)
      t.inf <- c(t.inf, t0 + t.next)

    smp <- rbind(smp, as.vector(symp(par.now)))

    t.now <- seq(from = 0, to = t.next, by = t.step)

    b.now <- ag(t.now, par.now)

    y.now <- ab(t.now, par.now, ...)

    t <- c(t, t0 + t.now)

    b <- rbind(b, b.now)
    y <- rbind(y, y.now)

    b.end <-
      b[nrow(as.matrix(b)), ]
    y.end <- y[nrow(as.matrix(y)), ]

    if (renew.params)
      if (n.mc == 0)
        nmc <- sample.int(n = mcsize, size = 1)

    if (!is.na(age.fx))
      par.now <- ldpar(age.fx, ablist, nmc, predpar = predpar, ...)

    if (is.na(age.fx))
      par.now <- ldpar((t0 + t.next) / day2yr, ablist, nmc, predpar = predpar, ...)

  }
  return(list(
    t = t,
    b = b,
    y = y,
    smp = smp,
    t.inf = t.inf
  ))

}

# collect cross-sectional data (age, y(t) set)
# seroconversion rate lambda (1/days)
# number of samples n.smpl (= nr of simulated records)
# age range for output age.rng = c(lowest, highest)
# age.fx for parameter sample (age.fx = NA for age at infection)
# antibody set ablist in 1:7
# when n.mc is in 1:4000 a fixed posterior sample is used,
# when n.mc = 0 a random smaple is chosen
# renew.params = TRUE generates a new parameter set for each infection
# renew.params = FALSE keeps the one selected at birth, but updates baseline y0
simcs.tinf <- function(
    lambda,
    n.smpl,
    age.rng,
    age.fx = NA,
    ablist,
    n.mc = 0,
    renew.params = FALSE,
    ...)
{
  st.days <- round(age.rng[1])
  # from min=age.rng[1] days...
  en.days <- round(age.rng[2])
  # to   max=age.rng[2] days...
  if (st.days == 0)
    st.days <- 1

  # if(en.days>30000) en.days <- 30000;
  y.smpl <- array(NA, dim = c(n.smpl, length(ablist) + 1))
  # y and age
  for (k.smpl in 1:n.smpl) {
    resp <-
      simresp.tinf(
        lambda,
        t.end = en.days,
        age.fx = age.fx,
        ablist = ablist,
        n.mc = n.mc,
        renew.params = renew.params,
        ...
      )

    tinf.smp <-
      sample((st.days:en.days), size = 1)
    # sample at random age
    y.smpl[k.smpl, ] <-
      c(resp$t[tinf.smp], as.matrix(resp$y)[tinf.smp, ])

  }
  return(y.smpl)

}




### these below are copied from Pater's par-extract.r script
par.pred <- function(parnum, k.test, predpar)
{
  if (parnum == 2)
  {
    par1 <- exp(predpar[k.test, 1, ])

    par2 <- exp(predpar[k.test, parnum, ])

    return(par1 + par2)

  } else
  {

    par <- exp(predpar[k.test, parnum, ])

    if (parnum == 5)
    {
      return(par + 1)
    } else
    {
      return(par)
    }

  }
}

# not used?
ltpar <- function(k.test, ...) {
  y0.mc       <- par.pred(1, k.test, ...)

  y1.mc       <- par.pred(2, k.test, ...)

  t1.mc       <- par.pred(3, k.test, ...)

  alpha.mc    <- par.pred(4,k.test, ...);
  shape.mc    <- par.pred(5,k.test, ...);
  return(data.frame("y1"=y1.mc,"alpha"=alpha.mc,"r"=shape.mc,
                    "y0"=y0.mc,"t1"=t1.mc));
}

# Generate longitudinal parameter sample for the simulation model
# parnum: use y0=par[1]; b0=par[2]; mu0=par[3]; mu1=par[4]; c1=par[5];
# alpha=par[6]; shape=par[7] (dmu = mu1 - mu0; b0 = 1)
mu1fn   <- function(y0,y1,t1) log(y1/y0)/t1;
mu0fn   <- function(beta,y0,y1,t1) mu1fn(y0,y1,t1)*exp(beta)/(1+exp(beta));
dmufn   <- function(beta,y0,y1,t1) mu1fn(y0,y1,t1)/(1+exp(beta));
t1fn    <- function(beta,y0,y1,t1,c1)
  log(1+dmufn(beta,y0,y1,t1)/(c1*y0))/dmufn(beta,y0,y1,t1);
y1fn    <- function(beta,y0,y1,t1,c1)
  y0*(1+dmufn(beta,y0,y1,t1)/(c1*y0))^(1+exp(beta))
objfnt1 <- function(par,y0,y1,t1) (t1-t1fn(par[1],y0,y1,t1,exp(par[2])))^2;
# the longitudinal model estimates observables: y0, y1, t1, alpha, shape
# for the simulation model we need mu0, mu1 and c1 (=c/b0)
# mu1 can be calculated (see above) but mu0 and c1 are not so easy
# the equation for t1 (above) can be used to calculate dmu (= mu1 - mu0)
# and c1. We need a pair (dmu, c1) that minimizes the difference
# (t1 - t1fn(dmu, c1, y0))^2. This is done in the following function
estpart1 <- function(y0,y1,t1){
  # est <- nlm(objfn,c(-1,-12),y0=y0,t1=t1); # print(est$estimate);
  # if(sqrt(est$minimum/t1)>1e0) print(sqrt(est$minimum));# return(NA);
  # return(exp(est$estimate));
  est <- optim(c(-1,-12),objfnt1,y0=y0,y1=y1,t1=t1,method="Nelder-Mead");
  if(sqrt(est$value/t1)>1e-1) print(sqrt(est$value));
  return(c(exp(est$par[1])/(1+exp(est$par[1])),exp(est$par[2])));
}

par.pred.n <- function(parnum,k.test,nmc, predpar, ...){
  if(parnum==2){
    par1 <- exp(predpar[k.test,1,nmc]);
    par2 <- exp(predpar[k.test,parnum,nmc]);
    return(par1+par2);
  }
  par <- exp(predpar[k.test,parnum,nmc]);
  if(parnum==5) return(par+1);
  return(par);
}

simpar <- function(age,k.test,nmc, ...){
  y0.mc       <- par.pred.n(1,k.test,nmc, ...);
  b0.mc       <- 1;
  y1.mc       <- par.pred.n(2,k.test,nmc, ...);
  t1.mc       <- par.pred.n(3,k.test,nmc, ...);
  alpha.mc    <- par.pred.n(4,k.test,nmc, ...);
  shape.mc    <- par.pred.n(5,k.test,nmc, ...);
  sp <- estpart1(y0.mc,y1.mc,t1.mc);
  mu1.mc <- mu1fn(y0.mc,y1.mc,t1.mc);
  mu0.mc <- mu0fn(sp[1],y0.mc,y1.mc,t1.mc);
  c1.mc  <- sp[2];
  return(c(y0.mc,b0.mc,mu0.mc,mu1.mc,c1.mc,alpha.mc,shape.mc));
}

