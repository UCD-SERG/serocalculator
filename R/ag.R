# kinetics of the bacteria (ag: antigen) response
ag <- function(t,par)
{
  t1 <- t1func(par)
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
