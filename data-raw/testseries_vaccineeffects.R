# compare scalcv3 and the new simulation method
# for a range of lambdas

if(!any(objects()=="predpar")) # check if parameters loaded
  source("initpar.r"); # if not: do it now (takes a little time)
source("simfunc.r"); # load internal and external functions

# define global parameters for simulation
ktest <- 1; # there are 7
ab.nm <- c("HlyE IgA", "HlyE IgG","LPS IgA","LPS IgG", "MP IgA","MP IgG",
           "Vi IgG");
iv.mc <- initvec(ktest);
pv.mc <- parmvec(ktest);
y0.pred <- iv.mc[1,]; b0.pred <- iv.mc[2,];
mu0.pred <- pv.mc[1,]; mu1.pred <- pv.mc[2,]; c1.pred <- pv.mc[3,];
alpha.pred <- pv.mc[4,]; shape.pred <- pv.mc[5,];
ln.pars <- longpars(ktest);
max.age <- 20; # maximum nr years of circulation

n.smpl <- 500; # sample size
day2yr <- 365.25;
lam.ref <- exp(seq(from=log(1e-2),to=log(1e1),
                   by=(log(5e0)-log(1e-3))/20)-log(day2yr)); # log lambda (1/day)
basevec <- c(-4,0.5); # lognormal noise parameters
# define global parameters for serocalculator
lnn <- qlnorm(p=0.98,meanlog=basevec[1],sdlog=basevec[2]);
cond0 <- data.frame(nu=lnn,             # B noise
                    eps=0.3,            # M noise
                    y.low=0.0,          # low cutoff
                    y.high=5e4);        # high cutoff
lnn <- qlnorm(p=0.9999,meanlog=basevec[1],sdlog=basevec[2]);
cond1 <- data.frame(nu=lnn,             # B noise
                    eps=0.3,            # M noise
                    y.low=0.0,          # low cutoff
                    y.high=5e4);        # high cutoff
# define global parameters for KS simulations
n.fac <- 10; # n simulated = n.fac * n observed sample
n.avr <- 1; # nr to average over (decrease noise in KS dev etc.)
lcstep <- 0.05; # step for coarse grid log lambda
llstep <- 0.01; # step for fine grid log lambda
lam.std <- c();
for(lr in lam.ref){
  # simulate ages and antibody concentrations
  a.smpl <- mkagsmpl(nsmp=n.smpl,minim=0,maxim=max.age);
  y.smpl <- mkabsmpl(nsmp=n.smpl,lam=lr,asmp=a.smpl,
                     nmc=nmc,iv=iv.mc,pv=pv.mc,bv=basevec);
  cat("lambda = ",lr*day2yr,"(1/yr)\n");
  lam.init = lr*day2yr # initial estimate: starting value
  # run serocalculator
  lam.est0 <- scalcv3(lam.init=lam.init,asmp=a.smpl,ysmp=y.smpl,bv=cond0);
  cat("lambda = ",lam.est0[1],"(",lam.est0[2],"-",lam.est0[3],")\n");
  lam.est1 <- scalcv3(lam.init=lam.init,asmp=a.smpl,ysmp=y.smpl,bv=cond1);
  cat("lambda = ",lam.est1[1],"(",lam.est1[2],"-",lam.est1[3],")\n");
  # estimate lambda by simulation
  ksd <- ksdata(ysmp=y.smpl,asmp=a.smpl,nfac=n.fac); # prepare data
  log.lam <- seq(from=-2.5,to=2.5,by=lcstep)+log(lr); # log lambda grid
  # run estimation
  kmdev3 <- ksdev(llam=log.lam,ysmp=ksd$y,aks=day2yr*ksd$a,bv=basevec,navr=n.avr);
  mx3 <- which(kmdev3==min(kmdev3))[1];
  cat("lambda = ",exp(log.lam[mx3])*day2yr,"(mx3) \n");
  llam.start <- log.lam[mx3]; # start at minimum KS deviance
  # estimate KS prob near min KS dev
  prb.rng <- ksprb(llam.st=llam.start,llam.stp=llstep,
                   ysmp=ksd$y,aks=day2yr*ksd$a,bv=basevec,navr=n.avr*5);
  # find maximum KS prob and range
  kmdev5 <- prb.rng$p;
  log.lam2 <- prb.rng$llam;
  mx5 <- which(kmdev5==max(kmdev5))[1]; # may be > 1
  ci5 <- which(kmdev5>=0.005); ci5 <- c(min(ci5),max(ci5));
  cat("lambda = ",exp(log.lam2[mx5])*day2yr,
      "(",exp(log.lam2[ci5[1]])*day2yr,"-",exp(log.lam2[ci5[2]])*day2yr,")\n");
  cat("prob = ",kmdev5[mx5],"(",kmdev5[ci5[1]],"-",kmdev5[ci5[2]],")","\n");
  lam.std <- rbind(lam.std,
                   c(lr*day2yr,lam.est0,lam.est1,exp(log.lam[mx3])*day2yr,
                     exp(c(log.lam2[mx5],log.lam2[ci5[1]],log.lam2[ci5[2]]))*day2yr,
                     c(kmdev5[mx5],kmdev5[ci5[1]],kmdev5[ci5[2]])));
}
