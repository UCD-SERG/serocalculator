if(!any(objects()=="predpar")) # check if parameters loaded
  source("initpar.r"); # if not: do it now (takes a little time)
source("simfunc.r"); # load internal and external functions

ktest <- 1; # antigen/isotype (there are 7)
ab.nm <- c("HlyE IgA", "HlyE IgG","LPS IgA","LPS IgG", "MP IgA","MP IgG",
           "Vi IgG");
iv.mc <- initvec(ktest); # (y0, b0)
pv.mc <- parmvec(ktest); # (mu0, mu1, c1, alpha, shape)
y0.pred <- iv.mc[1,]; b0.pred <- iv.mc[2,];
mu0.pred <- pv.mc[1,]; mu1.pred <- pv.mc[2,]; c1.pred <- pv.mc[3,];
alpha.pred <- pv.mc[4,]; shape.pred <- pv.mc[5,];

max.age <- 20; # maximum nr years of circulation ("age" of subjects)
basevec <- c(-4,0.5); # lognormal B-noise parameters
lam.ref <- 5e-1; # simulated lambda 1/yr

n.ref <- 1000; # cross-sectional sample size
day2yr <- 365.25;

# define global parameters for serocalculator
ln.pars <- longpars(ktest); # longitudinal  parameters for scalcv3
lnn <- qlnorm(p=0.98,meanlog=basevec[1],sdlog=basevec[2]);
cond0 <- data.frame(nu=lnn,             # B noise
                    eps=0.3,            # M noise
                    y.low=0.0,          # low cutoff
                    y.high=5e4);        # high cutof
lnn <- qlnorm(p=0.99999,meanlog=basevec[1],sdlog=basevec[2]);
cond1 <- data.frame(nu=lnn,             # B noise
                    eps=0.3,            # M noise
                    y.low=0.0,          # low cutoff
                    y.high=5e4);        # high cutof

cat("lambda (ref)=",lam.ref,"\n");
lam.init <- lam.ref/day2yr; # incidence 1/day
a.smpl <- mkagsmpl(nsmp=n.ref,minim=0,maxim=max.age);     # make age sample
y.smpl <- mkabsmpl(nsmp=n.ref,lam=lam.init,asmp=a.smpl,   # and corresponding
                   nmc=nmc,iv=iv.mc,pv=pv.mc,bv=basevec); # antibody sample

# kmdev <- estimdev(lam.init=lam.init,bv.init=basevec+1, # min Kolmogorov D
#                   ydata=y.smpl,adata=a.smpl,n.fac=5,n.aver=10);
# nr of samples for KS D = n.fac * n.ref (allows smoother EDF)
# n.aver = nr replicate calculations to average (allows smoother D)
# print(kmdev);
# lam.in <- kmdev[1]/day2yr; # use this as crude estimate for lambda
# bvec <- kmdev[2:3]; # and use these for  B-noise
# cat("lambda = ",lam.in*day2yr,"\n");
# cat("B-noise basevec = (",kmdev[2],",",kmdev[3],")\n");

cat("Simulation done. Starting serocalculator...\n");
# run serocalculator
lam.est0 <- scalcv3(lam.init=lam.ref,asmp=a.smpl,ysmp=y.smpl,bv=cond0);
cat("lambda = ",lam.est0[1],"(",lam.est0[2],"-",lam.est0[3],")\n");
# run serocalculator
lam.est1 <- scalcv3(lam.init=lam.ref,asmp=a.smpl,ysmp=y.smpl,bv=cond1);
cat("lambda = ",lam.est1[1],"(",lam.est1[2],"-",lam.est1[3],")\n");
