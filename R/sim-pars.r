ver <-  "v9na";
basepath <- paste("~/stat/salmonella/typhoid/sero/long/",ver,"/",sep="");
source(paste(basepath,ver,".data",".r",sep=""));
filemc <- paste(basepath,"output/",ver,".pred",".rda",sep="");
filescc <- paste("output/",ver,".scalc",".rda",sep="");
filesim <- paste("output/",ver,".simul",".rda",sep="");
filepar <- function(k.test,age)
  return(paste("age-",age,"-longpar-",
               ab.nm[k.test,1],"-",ab.nm[k.test,2],".rda",sep=""));

# Generate longitudinal parameter sample for the serocalculator
# parnum: use y0=par[1]; y1=par[2]; t1=par[3]; alpha=par[4]; shape=par[5]
par.pred <- function(k.test,parnum){
  if(parnum==2){
    par1 <- exp(predpar[k.test,1,]);
    par2 <- exp(predpar[k.test,parnum,]);
    return(par1+par2);
  }
  par <- exp(predpar[k.test,parnum,]);
  if(parnum==5) return(par+1);
  return(par);
}

# Generate longitudinal parameter sample for the simulation model
# parnum: use y0=par[1]; b0=par[2]; mu0=par[3]; mu1=par[4]; c1=par[5];
# alpha=par[6]; shape=par[7] (dmu = mu1 - mu0; b0 = 1)
f.mu1 <- function(y0,y1,t1) log(y1/y0)/t1;
# f.logy1 <- function(y0,x,c.star) log(y0)+x*log(1+1/(x*c.star));
# exp(c.star) = c1*y0/mu1
# when mu0 = exp(x.star)(1+exp(x.star))*mu1 then
# mu1/(mu1-mu0) = 1+exp(x.star)
f.logy1 <- function(y0,x.star,c.star)
  log(y0)+(1+exp(x.star))*log(1+1/((1+exp(x.star))*exp(c.star)));
f.dlogy1 <- function(par,y0,y1) (log(y1)-f.logy1(y0,par[1],par[2]))^2;
f.y1 <- function(y0,mu0,mu1,c1) y0*(1+(mu1-mu0)/(c1*y0))^(mu1/(mu1-mu0));
f.t1 <- function(y0,mu0,mu1,c1) log(1+(mu1-mu0)/(c1*y0))/(mu1-mu0)
f.mu0 <- function(par,mu1) mu1*exp(par[1])/(1+exp(par[1]));
f.c1 <- function(par,y0,mu1) mu1*exp(par[2])/y0;
# the longitudinal model estimates observables: y0, y1, t1, alpha, shape
# for the simulation model we need mu0, mu1 and c1 (=c/b0)
# mu1 can be calculated (see above) but mu0 and c1 are not so easy.
# The equation for y1 (above) can be used to calculate a pair x.star
# and c.star that minimizes the difference (log(y1) - f.logy1(y0,x,c.star))^2.
# This is done in the following function
f.est.y1 <- function(y0,y1){
  est <- optim(c(-6,-6),f.dlogy1,y0=y0,y1=y1,method="Nelder-Mead");
  # est <- optim(c(-6,-6),f.dlogy1,y0=y0,y1=y1,method="L-BFGS-B");
  #if(exp(sqrt(est$value))/y1 > 1e-2) print(exp(sqrt(est$value))/y1);
  return(est$par); # c(x.star,c.star)
}

load(filemc); # load posterior predictive parameter sample

scalcpar <- function(){
  nmc <- length(par.pred(1,1));
  par.mc <- array(NA,dim=c(5,ntest,nmc));
  for(k.par in 1:5){  
    for(k.test in 1:ntest){
      par.mc[k.par,k.test,] <- par.pred(k.test,k.par);
    }
  }
  return(list("y0"=par.mc[1,,],"alpha"=par.mc[4,,],"r"=par.mc[5,,],
              "y1"=par.mc[2,,],"t1"=par.mc[3,,]));
}

simpar <- function(){
  ltpar <- scalcpar();
  dmc <- dim(ltpar$y0);
  b0.pred  <- array(1.0,dim=dmc);
  mu0.pred <- array( NA,dim=dmc);
  mu1.pred <- array( NA,dim=dmc);
  c1.pred  <- array( NA,dim=dmc);
  for(k.test  in 1:dmc[1]){
    for(k.mc in 1:dmc[2]){
      sp <- f.est.y1(ltpar$y0[k.test,k.mc],ltpar$y1[k.test,k.mc]);
      mu1.pred[k.test,k.mc] <- f.mu1(ltpar$y0[k.test,k.mc],
                                     ltpar$y1[k.test,k.mc],
                                     ltpar$t1[k.test,k.mc]);
      mu0.pred[k.test,k.mc] <- f.mu0(sp,mu1.pred[k.test,k.mc]);
      c1.pred[k.test,k.mc]  <- f.c1(sp,ltpar$y0[k.test,k.mc],
                                    mu1.pred[k.test,k.mc]);
    }
  }
  y0.pred    <- ltpar$y0;
  alpha.pred <- ltpar$alpha;
  r.pred     <- ltpar$r;
  return(list("y0"=y0.pred,"b0"=b0.pred,
              "mu0"=mu0.pred,"mu1"=mu1.pred,"c1"=c1.pred,
              "alpha"=alpha.pred,"r"=r.pred));
}
