#  Utility functions: interface with C lib simul.so

dyn.load(paste(basepath,"C/simulv.so",sep=""));

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
