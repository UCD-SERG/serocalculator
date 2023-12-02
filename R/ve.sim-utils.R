day2yr <- 365.25;

ver <-  "v9na";
#basepath <- paste("~/stat/salmonella/typhoid/sero/long/",ver,"/",sep="");
#source(paste(basepath,ver,".data",".r",sep=""));
#filemc <- paste(basepath,"output/",ver,".pred",".rda",sep="");
#filescc <- paste("output/",ver,".scalc",".rda",sep="");
#filesim <- paste("output/",ver,".simul",".rda",sep="");
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

#load(filemc); # load posterior predictive parameter sample

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




simul.pred <- simpar();


initvec <- function(ktest)
  return(rbind(simul.pred$y0[ktest,],simul.pred$b0[ktest,]));


parmvec <- function(ktest)
  return(rbind(simul.pred$mu0[ktest,],simul.pred$mu1[ktest,],
               simul.pred$c1[ktest,],
               simul.pred$alpha[ktest,],simul.pred$r[ktest,]));

scalc.pred <- scalcpar();
longpars <- function(ktest)
  return(data.frame(y1=scalc.pred$y1[ktest,],              # peak levels
                    alpha=day2yr*scalc.pred$alpha[ktest,], # decay (1/yr)
                    d=scalc.pred$r[ktest,] - 1));          # shape (r-1)

dmc <- dim(scalc.pred$y1);
nmc <- dmc[2];



scalcv3 <- function(lam.init,asmp,ysmp,bv){
  llam.init <- log(lam.init);
  max.step <- (log(lam.init*10)-log(lam.init/10))/4;
  cs.smpl <- data.frame(y=ysmp,a=asmp);

  objfunc <- function(llam){
    return(fdev(llam,cs.smpl,ln.pars,bv));
  }
  fit <- nlm(objfunc,llam.init,hessian=TRUE,print.level=0,stepmax=max.step);
  llam.est = c(fit$estimate,
               fit$estimate + qnorm(c(0.05))*sqrt(1/fit$hessian),
               fit$estimate + qnorm(c(0.95))*sqrt(1/fit$hessian));
  return(exp(llam.est));
}

# functions for simulation of cross-sectional serum antibody sample
#source(paste(basepath,"C/simulv.r",sep="")); # also loads simulv.so

mkagsmpl <- function(nsmp,minim,maxim){
  return(runif(n=nsmp,min=minim,max=maxim));
}
mkabsmpl <- function(nsmp,lam,asmp,nmc,iv,pv,bv){
  ysmp <- rep(NA,nsmp);
  for(ksmp in 1:nsmp){
    kmc <- sample.int(nmc,size=1);
    ysmp[ksmp] <- srespmemry(day2yr*asmp[ksmp],lam,iv[,kmc],pv[,kmc],bv);
  }
  return(ysmp);
}

mkabsmplrenew <- function(nsmp,lam,asmp,nmc,iv,pv,bv){
  ysmp <- rep(NA,nsmp);
  for(ksmp in 1:nsmp){
    kmc <- sample.int(nmc,size=1);
    ysmp[ksmp] <- sresprenew(day2yr*asmp[ksmp],lam,iv[,kmc],pv[,kmc],bv);
  }
  return(ysmp);
}

# simulation-based fit functions

ksdata <- function(ysmp,asmp,nfac){
  srtd <- order(ysmp);
  nsmp <- length(ysmp);
  ny <- nsmp*nfac; # nr simulated y = nr cs sample (think of ages!)
  if(nfac==1) ay <- asmp[srtd];
  if(nfac!=1) ay <- sample(asmp,size=ny,replace=TRUE);
  return(list(y=ysmp[srtd],a=ay)); # can be unequal length!
}

devksa <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimksa(log(ysmp),ny,aks,lam,
                         y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                         alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devksarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimksarenew(log(ysmp),ny,aks,lam,
                              y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                              alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devada <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimada(log(ysmp),ny,aks,lam,
                         y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                         alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devadarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimadarenew(log(ysmp),ny,aks,lam,
                              y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                              alpha.pred,shape.pred,bv);
  return(kd/navr);
}

probksa <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kp <- 0;
  for(k in 1:navr)
    kp <- kp + probsimksa(log(ysmp),ny,aks,lam,
                          y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                          alpha.pred,shape.pred,bv);
  return(kp/navr);
}

probksarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kp <- 0;
  for(k in 1:navr)
    kp <- kp + probsimksarenew(log(ysmp),ny,aks,lam,
                               y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                               alpha.pred,shape.pred,bv);
  return(kp/navr);
}

ksdev <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devksa(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

ksdevrenew <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devksarenew(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

addev <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devada(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

addevrenew <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devadarenew(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

ksprb <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; ksprb <- ksp;
  while(ksp >= 1e-10 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam2,llam);
    ksprb <- c(ksprb,ksp);
  }
  llam <- llam.st; ksp <- ksprb[1];
  while(ksp >= 1e-10 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam,llam2);
    ksprb <- c(ksp,ksprb);
  }
  return(data.frame(llam=llam2,p=ksprb));
}

ksprbrenew <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; ksprb <- ksp;
  while(ksp >= 1e-10 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam2,llam);
    ksprb <- c(ksprb,ksp);
  }
  llam <- llam.st; ksp <- ksprb[1];
  while(ksp >= 1e-10 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam,llam2);
    ksprb <- c(ksp,ksprb);
  }
  return(data.frame(llam=llam2,p=ksprb));
}

kslogprb <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; kslprb <- log(ksp); kslp <- kslprb;
  while(kslprb[1]-kslp < 8 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    kslp <- log(ksp);
    llam2 <- c(llam2,llam);
    kslprb <- c(kslprb,kslp);
  }
  llam <- llam.st; kslp <- kslprb[1];
  while(kslprb[1]-kslp < 8 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    kslp <- log(ksp);
    llam2 <- c(llam,llam2);
    kslprb <- c(kslp,kslprb);
  }
  return(data.frame(llam=llam2,p=exp(kslprb)));
}

findinterv <- function(y,yref){
  n.ref <- length(yref);
  if(y <= yref[1]) return(0.5/(n.ref+2));
  if(y >= yref[n.ref]) return((n.ref+0.5)/(n.ref+2))
  kl <- rev(which(yref < y))[1]; # lower
  kh <- which(yref > y)[1];      # upper
  r <- (y - yref[kl])/(yref[kh]-yref[kl]);
  return((kl+r)/(n.ref+2));
}

qq <- function(y,yref){
  ym.dist <- rep(NA,length(y));
  for(k in 1:length(y)) ym.dist[k] <- findinterv(y[k],yref);
  return(ym.dist);
}

findmxdev <- function(y,yref){
  n.y <- length(y);
  ym.dist <- rep(NA,length(y));
  mxdev <- 0;
  for(k in 1:length(y)){
    ym.dist[k] <- findinterv(y[k],yref);
    dev <- abs(ym.dist[k]-(k/n.y));
    if(mxdev < dev) mxdev <- dev;
  }
  return(mxdev);
}

findpmdev <- function(y,yref){
  n.y <- length(y);
  ym.dist <- rep(NA,length(y));
  mxdevpls <- 0; mxdevmin <- 0;
  for(k in 1:length(y)){
    ym.dist[k] <- findinterv(y[k],yref);
    devpls <- k/n.y - ym.dist[k];
    #devmin <- ym.dist[k]-((k-1)/n.y);
    devmin <- ym.dist[k]-((k)/n.y);
    if(mxdevpls < devpls) mxdevpls <- devpls;
    if(mxdevmin < devmin) mxdevmin <- devmin;
  }
  return(mxdevpls + mxdevmin);
}

ksdev.r <- function(llam,ysmp,aks,bv){
  mx1dev <- 1000;
  yqq <- array(NA,dim=c(length(llam),length(aks)));
  kmdev1 <- rep(NA,length(llam));
  for(k.lam in 1:length(llam)){
    ym <- mkabsmpl(length(aks),exp(llam[k.lam]),aks,nmc,iv.mc,pv.mc,bv)
    ym.c <- log(sort(ym));
    yref.c <- log(sort(ysmp));
    yqq[k.lam,] <- qq(ym.c,yref.c);
    kmdev1[k.lam] <- findmxdev(ym.c,yref.c);
    if(kmdev1[k.lam] < mx1dev){
      mx1dev <- kmdev1[k.lam];
      ymx1.c <- ym.c;
    }
  }
  return(list(kmdev1=kmdev1,ymx1=ymx1.c,yqq=yqq));
}

ksdevrenew.r <- function(llam,ysmp,aks,bv){
  mx1dev <- 1000;
  yqq <- array(NA,dim=c(length(llam),length(aks)));
  kmdev1 <- rep(NA,length(llam));
  for(k.lam in 1:length(llam)){
    ym <- mkabsmplrenew(length(aks),exp(llam[k.lam]),aks,nmc,iv.mc,pv.mc,bv)
    ym.c <- log(sort(ym));
    yref.c <- log(sort(ysmp));
    yqq[k.lam,] <- qq(ym.c,yref.c);
    kmdev1[k.lam] <- findmxdev(ym.c,yref.c);
    if(kmdev1[k.lam] < mx1dev){
      mx1dev <- kmdev1[k.lam];
      ymx1.c <- ym.c;
    }
  }
  return(list(kmdev1=kmdev1,ymx1=ymx1.c,yqq=yqq));
}

estimdev <- function(lam.init,bv.init,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  ny <- length(ksd$y); rpt1 <- n.aver;
  kmdev <- function(par){
    kd <- 0;
    for(rp in 1:rpt1)
      kd <- kd + devsimksa(log(ksd$y),ny,ksd$a*day2yr,exp(par[1]),
                           y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                           alpha.pred,shape.pred,c(par[2],exp(par[3])));
    return(kd/rpt1);
  }
  # res1 <- nlm(f=kmdev,p=c(log(lam.init),bv.init[1],log(bv.init[2])));
  # lam.est <- exp(res1$estimate[1])*day2yr;
  # bv.est <- c(res1$estimate[2],exp(res1$estimate[3]));
  # cat("lambda=",llam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  # return(c(lam.est,bv.est));
  res1 <- optim(par=c(log(lam.init),bv.init[1],log(bv.init[2])),kmdev,
                method="Nelder-Mead",
                control=list(trace=0,fnscale=1,maxit=1000,reltol=1e-3));
  lam.est <- exp(res1$par[1])*day2yr;
  bv.est <- c(res1$par[2],exp(res1$par[3]));
  # cat("lambda=",lam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  return(c(lam.est,bv.est));
}

estimprb <- function(lam.init,bv.init,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  ny <- length(ksd$y); rpt2 <- 2*n.aver;
  kmprb <- function(par){
    kp <- 0;
    for(rp in 1:(rpt2))
      kp <- kp + probsimksa(log(ksd$y),ny,ksd$a*day2yr,exp(par[1]),
                            y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                            alpha.pred,shape.pred,c(par[2],exp(par[3])));
    return(kp/(rpt2));
  }
  # res2 <- nlm(f=kmprb,p=c(log(lam.init),bv.init[1],log(bv.init[2])));
  # lam.est <- exp(res2$estimate[1])*day2yr;
  # bv.est <- c(res2$estimate[2],exp(res2$estimate[3]));
  # cat("lambda=",llam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  # return(c(lam.est,bv.est));
  res2 <- optim(par=c(log(lam.init),bv.init[1],log(bv.init[2])),kmprb,
                method="Nelder-Mead",
                control=list(trace=0,fnscale=-1,maxit=1000,reltol=1e-3));
  lam.est <- exp(res2$par[1])*day2yr;
  bv.est <- c(res2$par[2],exp(res2$par[3]));
  # cat("lambda=",lam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  return(c(lam.est,bv.est));
}

gridprb <- function(lam.init,bvec,step,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  prb <- ksprb(log(lam.init),llam.stp=step,ysmp=ksd$y,aks=ksd$a*day2yr,
               bv=bvec,navr=n.aver);
  return(prb);
}

findmxprb <- function(prblst){
  # mx <- floor(median(which(prblst$p==max(prblst$p)))); # may be > 1
  mx <- which(prblst$p==max(prblst$p)); # may be > 1
  ci <- which(prblst$p >= 0.025); ci <- c(min(ci),max(ci));
  return(c(mean(prblst$llam[mx]),prblst$llam[ci[1]],prblst$llam[ci[2]],
           prblst$p[mx]));
}



mkagsmpl <- function(nsmp,minim,maxim){
  return(runif(n=nsmp,min=minim,max=maxim));
}
mkabsmpl <- function(nsmp,lam,asmp,nmc,iv,pv,bv){
  ysmp <- rep(NA,nsmp);
  for(ksmp in 1:nsmp){
    kmc <- sample.int(nmc,size=1);
    ysmp[ksmp] <- srespmemry(day2yr*asmp[ksmp],lam,iv[,kmc],pv[,kmc],bv);
  }
  return(ysmp);
}

mkabsmplinterv <- function(nsmp,lam1,tau_interv,lam2,asmp,nmc,iv,pv,bv){
  ysmp <- rep(NA,nsmp);
  for(ksmp in 1:nsmp){
    kmc <- sample.int(nmc,size=1);
    ysmp[ksmp] <- srespinterv(day2yr*asmp[ksmp],lam1,tau_interv,lam2,
                              iv[,kmc],pv[,kmc],bv);
  }
  return(ysmp);
}

mkabsmplrenew <- function(nsmp,lam,asmp,nmc,iv,pv,bv){
  ysmp <- rep(NA,nsmp);
  for(ksmp in 1:nsmp){
    kmc <- sample.int(nmc,size=1);
    ysmp[ksmp] <- sresprenew(day2yr*asmp[ksmp],lam,iv[,kmc],pv[,kmc],bv);
  }
  return(ysmp);
}

# simulation-based fit functions

ksdata <- function(ysmp,asmp,nfac){
  srtd <- order(ysmp);
  nsmp <- length(ysmp);
  ny <- nsmp*nfac; # nr simulated y = nr cs sample (think of ages!)
  if(nfac==1) ay <- asmp[srtd];
  if(nfac!=1) ay <- sample(asmp,size=ny,replace=TRUE);
  return(list(y=ysmp[srtd],a=ay)); # can be unequal length!
}

devksa <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimksa(log(ysmp),ny,aks,lam,
                         y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                         alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devksarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimksarenew(log(ysmp),ny,aks,lam,
                              y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                              alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devada <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimada(log(ysmp),ny,aks,lam,
                         y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                         alpha.pred,shape.pred,bv);
  return(kd/navr);
}

devadarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kd <- 0;
  for(k in 1:navr)
    kd <- kd + devsimadarenew(log(ysmp),ny,aks,lam,
                              y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                              alpha.pred,shape.pred,bv);
  return(kd/navr);
}

probksa <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kp <- 0;
  for(k in 1:navr)
    kp <- kp + probsimksa(log(ysmp),ny,aks,lam,
                          y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                          alpha.pred,shape.pred,bv);
  return(kp/navr);
}

probksainterv <- function(lam1,tau,lam2,ysmp,aks,bv,navr){
  ny <- length(aks); kp <- 0;
  for(k in 1:navr)
    kp <- kp + probsimksainterv(log(ysmp),ny,aks,lam1,tau,lam2,
                                y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                                alpha.pred,shape.pred,bv);
  return(kp/navr);
}

probksarenew <- function(lam,ysmp,aks,bv,navr){
  ny <- length(aks); kp <- 0;
  for(k in 1:navr)
    kp <- kp + probsimksarenew(log(ysmp),ny,aks,lam,
                               y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                               alpha.pred,shape.pred,bv);
  return(kp/navr);
}

ksdev <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devksa(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

ksdevrenew <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devksarenew(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

addev <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devada(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

addevrenew <- function(llam,ysmp,aks,bv,navr){
  kmdev <- rep(NA,length(llam));
  for(klam in 1:length(llam))
    kmdev[klam] <- devadarenew(exp(llam[klam]),ysmp,aks,bv,navr);
  return(kmdev);
}

ksprb <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; ksprb <- ksp;
  while(ksp >= 1e-10 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam2,llam);
    ksprb <- c(ksprb,ksp);
  }
  llam <- llam.st; ksp <- ksprb[1];
  while(ksp >= 1e-10 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam,llam2);
    ksprb <- c(ksp,ksprb);
  }
  return(data.frame(llam=llam2,p=ksprb));
}

ksprbrenew <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; ksprb <- ksp;
  while(ksp >= 1e-10 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam2,llam);
    ksprb <- c(ksprb,ksp);
  }
  llam <- llam.st; ksp <- ksprb[1];
  while(ksp >= 1e-10 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksarenew(exp(llam),ysmp,aks,bv,navr);
    llam2 <- c(llam,llam2);
    ksprb <- c(ksp,ksprb);
  }
  return(data.frame(llam=llam2,p=ksprb));
}

kslogprb <- function(llam.st,llam.stp,ysmp,aks,bv,navr){
  llam <- llam.st;
  ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
  llam2 <- llam.st; kslprb <- log(ksp); kslp <- kslprb;
  while(kslprb[1]-kslp < 8 & llam - llam.st < 2.5){
    llam <- llam + llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    kslp <- log(ksp);
    llam2 <- c(llam2,llam);
    kslprb <- c(kslprb,kslp);
  }
  llam <- llam.st; kslp <- kslprb[1];
  while(kslprb[1]-kslp < 8 & llam.st-llam < 2.5){
    llam <- llam - llam.stp;
    ksp <- probksa(exp(llam),ysmp,aks,bv,navr);
    kslp <- log(ksp);
    llam2 <- c(llam,llam2);
    kslprb <- c(kslp,kslprb);
  }
  return(data.frame(llam=llam2,p=exp(kslprb)));
}

findinterv <- function(y,yref){
  n.ref <- length(yref);
  if(y <= yref[1]) return(0.5/(n.ref+2));
  if(y >= yref[n.ref]) return((n.ref+0.5)/(n.ref+2))
  kl <- rev(which(yref < y))[1]; # lower
  kh <- which(yref > y)[1];      # upper
  r <- (y - yref[kl])/(yref[kh]-yref[kl]);
  return((kl+r)/(n.ref+2));
}

qq <- function(y,yref){
  ym.dist <- rep(NA,length(y));
  for(k in 1:length(y)) ym.dist[k] <- findinterv(y[k],yref);
  return(ym.dist);
}

findmxdev <- function(y,yref){
  n.y <- length(y);
  ym.dist <- rep(NA,length(y));
  mxdev <- 0;
  for(k in 1:length(y)){
    ym.dist[k] <- findinterv(y[k],yref);
    dev <- abs(ym.dist[k]-(k/n.y));
    if(mxdev < dev) mxdev <- dev;
  }
  return(mxdev);
}

findpmdev <- function(y,yref){
  n.y <- length(y);
  ym.dist <- rep(NA,length(y));
  mxdevpls <- 0; mxdevmin <- 0;
  for(k in 1:length(y)){
    ym.dist[k] <- findinterv(y[k],yref);
    devpls <- k/n.y - ym.dist[k];
    #devmin <- ym.dist[k]-((k-1)/n.y);
    devmin <- ym.dist[k]-((k)/n.y);
    if(mxdevpls < devpls) mxdevpls <- devpls;
    if(mxdevmin < devmin) mxdevmin <- devmin;
  }
  return(mxdevpls + mxdevmin);
}

ksdev.r <- function(llam,ysmp,aks,bv){
  mx1dev <- 1000;
  yqq <- array(NA,dim=c(length(llam),length(aks)));
  kmdev1 <- rep(NA,length(llam));
  for(k.lam in 1:length(llam)){
    ym <- mkabsmpl(length(aks),exp(llam[k.lam]),aks,nmc,iv.mc,pv.mc,bv)
    ym.c <- log(sort(ym));
    yref.c <- log(sort(ysmp));
    yqq[k.lam,] <- qq(ym.c,yref.c);
    kmdev1[k.lam] <- findmxdev(ym.c,yref.c);
    if(kmdev1[k.lam] < mx1dev){
      mx1dev <- kmdev1[k.lam];
      ymx1.c <- ym.c;
    }
  }
  return(list(kmdev1=kmdev1,ymx1=ymx1.c,yqq=yqq));
}

ksdevrenew.r <- function(llam,ysmp,aks,bv){
  mx1dev <- 1000;
  yqq <- array(NA,dim=c(length(llam),length(aks)));
  kmdev1 <- rep(NA,length(llam));
  for(k.lam in 1:length(llam)){
    ym <- mkabsmplrenew(length(aks),exp(llam[k.lam]),aks,nmc,iv.mc,pv.mc,bv)
    ym.c <- log(sort(ym));
    yref.c <- log(sort(ysmp));
    yqq[k.lam,] <- qq(ym.c,yref.c);
    kmdev1[k.lam] <- findmxdev(ym.c,yref.c);
    if(kmdev1[k.lam] < mx1dev){
      mx1dev <- kmdev1[k.lam];
      ymx1.c <- ym.c;
    }
  }
  return(list(kmdev1=kmdev1,ymx1=ymx1.c,yqq=yqq));
}

estimdev <- function(lam.init,bv.init,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  ny <- length(ksd$y); rpt1 <- n.aver;
  kmdev <- function(par){
    kd <- 0;
    for(rp in 1:rpt1)
      kd <- kd + devsimksa(log(ksd$y),ny,ksd$a*day2yr,exp(par[1]),
                           y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                           alpha.pred,shape.pred,c(par[2],exp(par[3])));
    return(kd/rpt1);
  }
  # res1 <- nlm(f=kmdev,p=c(log(lam.init),bv.init[1],log(bv.init[2])));
  # lam.est <- exp(res1$estimate[1])*day2yr;
  # bv.est <- c(res1$estimate[2],exp(res1$estimate[3]));
  # cat("lambda=",llam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  # return(c(lam.est,bv.est));
  res1 <- optim(par=c(log(lam.init),bv.init[1],log(bv.init[2])),kmdev,
                method="Nelder-Mead",
                control=list(trace=0,fnscale=1,maxit=1000,reltol=1e-3));
  lam.est <- exp(res1$par[1])*day2yr;
  bv.est <- c(res1$par[2],exp(res1$par[3]));
  # cat("lambda=",lam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  return(c(lam.est,bv.est));
}

estimprb <- function(lam.init,bv.init,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  ny <- length(ksd$y); rpt2 <- 2*n.aver;
  kmprb <- function(par){
    kp <- 0;
    for(rp in 1:(rpt2))
      kp <- kp + probsimksa(log(ksd$y),ny,ksd$a*day2yr,exp(par[1]),
                            y0.pred,b0.pred,mu0.pred,mu1.pred,c1.pred,
                            alpha.pred,shape.pred,c(par[2],exp(par[3])));
    return(kp/(rpt2));
  }
  # res2 <- nlm(f=kmprb,p=c(log(lam.init),bv.init[1],log(bv.init[2])));
  # lam.est <- exp(res2$estimate[1])*day2yr;
  # bv.est <- c(res2$estimate[2],exp(res2$estimate[3]));
  # cat("lambda=",llam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  # return(c(lam.est,bv.est));
  res2 <- optim(par=c(log(lam.init),bv.init[1],log(bv.init[2])),kmprb,
                method="Nelder-Mead",
                control=list(trace=0,fnscale=-1,maxit=1000,reltol=1e-3));
  lam.est <- exp(res2$par[1])*day2yr;
  bv.est <- c(res2$par[2],exp(res2$par[3]));
  # cat("lambda=",lam.est," bv=(",bv.est[1],",",bv.est[2],")\n");
  return(c(lam.est,bv.est));
}

gridprb <- function(lam.init,bvec,step,ydata,adata,n.fac,n.aver){
  ksd <- ksdata(ysmp=ydata,asmp=adata,nfac=n.fac); # prepare data
  prb <- ksprb(log(lam.init),llam.stp=step,ysmp=ksd$y,aks=ksd$a*day2yr,
               bv=bvec,navr=n.aver);
  return(prb);
}

findmxprb <- function(prblst){
  # mx <- floor(median(which(prblst$p==max(prblst$p)))); # may be > 1
  mx <- which(prblst$p==max(prblst$p)); # may be > 1
  ci <- which(prblst$p >= 0.025); ci <- c(min(ci),max(ci));
  return(c(mean(prblst$llam[mx]),prblst$llam[ci[1]],prblst$llam[ci[2]],
           prblst$p[mx]));
}



#  Utility functions: interface with C lib simul.so

#dyn.load(paste(basepath,"C/simulv.so",sep=""));

a <- function(initvec,parvec){
  res <- 0;
  iv <- as.double(initvec); pv <- as.double(parvec);
  ax <- .C("ar",res=as.double(res),iv=iv,pv=pv);
  return(ax$res);
}

b <- function(parvec){
  res <- 0;
  pv <- as.double(parvec);
  bx <- .C("br",res=as.double(res),pv=pv);
  return(bx$res);
}

t1 <- function(initvec,parvec){
  res <- 0;
  iv <- as.double(initvec); pv <- as.double(parvec);
  t1x <- .C("t1r",res=as.double(res),iv=iv,pv=pv);
  return(t1x$res);
}

y1 <- function(initvec,parvec){
  res <- 0;
  iv <- as.double(initvec); pv <- as.double(parvec);
  y1x <- .C("y1r",res=as.double(res),iv=iv,pv=pv);
  return(y1x$res);
}

ag <- function(t,initvec,parvec){
  res <- 0; t <- as.double(t);
  iv <- as.double(initvec); pv <- as.double(parvec);
  agx <- .C("agr",res=as.double(res),t=t,iv=iv,pv=pv);
  return(agx$res);
}

ab <- function(t,initvec,parvec){
  res <- 0; t <- as.double(t);
  iv <- as.double(initvec); pv <- as.double(parvec);
  abx <- .C("abr",res=as.double(res),t=t,iv=iv,pv=pv);
  return(abx$res);
}

symp <- function(initvec,parvec){
  res <- 0;
  iv <- as.double(initvec); pv <- as.double(parvec);
  sympx <- .C("sympr",res=as.integer(res),iv=iv,pv=pv);
  return(sympx$res);
}

transf <- function(y0,initvec,parvec){
  res <- 0; y0 <- as.double(y0);
  iv <- as.double(initvec); pv <- as.double(parvec);
  transfx <- .C("transfr",res=as.double(res),y0=y0,iv=iv,pv=pv);
  return(transfx$res);
}

tinterv <- function(lambda){
  res <-  0; lambda <- as.double(lambda);
  tintervx <- .C("tintervr",res=as.double(res),lambda=lambda);
  return(tintervx$res);
}

unifbasel <- function(bslvec){
  res <-  0; bslvec <- as.double(bslvec);
  baselx <- .C("rndunifr",res=as.double(res),basel=bslvec);
  return(baselx$res);
}

lnormbasel <- function(bslvec){
  res <-  0; bslvec <- as.double(bslvec);
  baselx <- .C("rndlnormr",res=as.double(res),basel=bslvec);
  return(baselx$res);
}

srespmemry  <- function(age,lambda,initvec,parvec,basevec){
  res <- 0; age <- as.double(age); lambda <- as.double(lambda);
  iv <- as.double(initvec); pv <- as.double(parvec); bsl <- as.double(basevec);
  srespx <- .C("srespmemryr",res=as.double(res),age=age,lambda=lambda,
               iv=iv,pv=pv,bsl=bsl);
  return(srespx$res);
}

sresprenew  <- function(age,lambda,initvec,parvec,basevec){
  res <- 0; age <- as.double(age); lambda <- as.double(lambda);
  iv <- as.double(initvec); pv <- as.double(parvec);bsl <- as.double(basevec);
  srespx <- .C("sresprenewr",res=as.double(res),age=age,lambda=lambda,
               iv=iv,pv=pv,bsl=bsl);
  return(srespx$res);
}

rndind <- function(sz){
  res <-  0; sz <- as.integer(sz);
  rndindx <- .C("rndindr",res=as.integer(res),size=sz);
  return(rndindx$res)
}

devsimks <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimksrenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                          bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksrenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimkp <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndkpr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimkprenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                          bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndkprenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

probsimks <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndkspr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

probsimksrenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                           bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksprenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimad <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndadr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimadrenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                          bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndadrenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimksa <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksar",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimksarenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                           bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksarenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimkpa <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndkpar",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimkparenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                           bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndkparenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

probsimksa <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksapr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

probsimksarenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                            bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndksaprenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimada <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndadar",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

devsimadarenew <- function(logyobs,nsim,age,lambda,y0,b0,mu0,mu1,c,alpha,shape,
                           bsvec){
  logyobs <- as.double(logyobs); nobs <- as.integer(length(logyobs));
  nsim <- as.integer(nsim); age <- as.double(age);
  lambda <- as.double(lambda);
  nmc <- as.integer(length(y0)); y0 <- as.double(y0); b0 <- as.double(b0);
  mu0 <- as.double(mu0);  mu1 <- as.double(mu1);  c <- as.double(c);
  alpha <- as.double(alpha);  shape <- as.double(shape);
  bsl <- as.double(bsvec);
  res <- 0.0;
  devs <- .C("drndadarenewr",res=as.double(res),logyobs,nobs,nsim,age,lambda,
             nmc,y0,b0,mu0,mu1,c,alpha,shape,bsl=bsl);
  return(devs$res);
}

srespinterv  <- function(age,lambda1,tau_interv,lambda2,
                         initvec,parvec,basevec){
  res <- 0; age <- as.double(age); tau_interv <- as.double(tau_interv);
  lambda1 <- as.double(lambda1); lambda2 <- as.double(lambda2);
  iv <- as.double(initvec); pv <- as.double(parvec); bsl <- as.double(basevec);
  srespx <- .C("srespintervr",res=as.double(res),age=age,
               lambda1=lambda1,tau_interv=tau_interv,lambda2=lambda2,
               iv=iv,pv=pv,bsl=bsl);
  return(srespx$res);
}

ksdevtwo <- function(dat1,dat2){
  d1 <- as.double(dat1); n1 <- length(dat1);
  d2 <- as.double(dat2); n2 <- length(dat2);
  res <- 0.0;
  ks2 <- .C("ksdevtwor",res=as.double(res),d1,n1,d2,n2);
  return(ks2$res);
}
ksprobtwo <- function(dat1,dat2){
  d1 <- as.double(dat1); n1 <- length(dat1);
  d2 <- as.double(dat2); n2 <- length(dat2);
  res <- 0.0;
  ks2 <- .C("ksprobtwor",res=as.double(res),d1,n1,d2,n2);
  return(ks2$res);
}
kpdevtwo <- function(dat1,dat2){
  d1 <- as.double(dat1); n1 <- length(dat1);
  d2 <- as.double(dat2); n2 <- length(dat2);
  res <- 0.0;
  ks2 <- .C("kpdevtwor",res=as.double(res),d1,n1,d2,n2);
  return(ks2$res);
}

adstattwo <- function(dat1,dat2){
  d1 <- as.double(dat1); n1 <- length(dat1);
  d2 <- as.double(dat2); n2 <- length(dat2);
  res <- 0.0;
  ad2 <- .C("adstattwor",res=as.double(res),d1,n1,d2,n2);
  return(ad2$res);
}



