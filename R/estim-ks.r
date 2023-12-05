if(!any(objects()=="predpar")) # check if parameters loaded
  source("initpar.r"); # if not: do it now (takes a little time)
source("simfunc.r");  # sets path to C directory

ktest <- 1; # antigen/isotype (there are 7)
ab.nm <- c("HlyE IgA", "HlyE IgG","LPS IgA","LPS IgG", "MP IgA","MP IgG",
           "Vi IgG");
iv.mc <- initvec(ktest); # (y0, b0)
pv.mc <- parmvec(ktest); # (mu0, mu1, c1, alpha, shape)
y0.pred <- iv.mc[1,]; b0.pred <- iv.mc[2,];
mu0.pred <- pv.mc[1,]; mu1.pred <- pv.mc[2,]; c1.pred <- pv.mc[3,];
alpha.pred <- pv.mc[4,]; shape.pred <- pv.mc[5,];
ln.pars <- longpars(ktest); # longitudinal  parameters for scalcv3
max.age <- 20; # maximum nr years of circulation ("age" of subjects)

basevec <- c(-4,0.5); # lognormal B-noise parameters
lam.ref <- 5e-1; # simulated lambda 1/yr
n.ref <- 500; # cross-sectional sample size
day2yr <- 365.25;

lam.init <- lam.ref/day2yr; # incidence 1/day
a.smpl <- mkagsmpl(nsmp=n.ref,minim=0,maxim=max.age);     # make age sample
y.smpl <- mkabsmpl(nsmp=n.ref,lam=lam.init,asmp=a.smpl,   # and corresponding
                   nmc=nmc,iv=iv.mc,pv=pv.mc,bv=basevec); # antibody sample

kmdev <- estimdev(lam.init=lam.init,bv.init=basevec+1, # min Kolmogorov D
                  ydata=y.smpl,adata=a.smpl,n.fac=5,n.aver=10);
# nr of samples for KS D = n.fac * n.ref (allows smoother EDF)
# n.aver = nr replicate calculations to average (allows smoother D)
cat("lambda (ref)=",lam.ref,"\n");
cat("lambda  (crude)=",kmdev[1]," B-noise = LN(",kmdev[2],",",kmdev[3],")\n");
lam.in <- kmdev[1]/day2yr; # use this as crude estimate for lambda
bvec <- kmdev[2:3]; # and use these for  B-noise

# kmprb1 <- estimprb(lam.init=lam.in,bv.init=bvec,
#                    ydata=y.smpl,adata=a.smpl,n.fac=5,n.aver=10);
# print(kmprb);
# lam.in <- kmprb1[1]/day2yr;
# bvec <- kmprb1[2:3];

# now calculate the Kolmogorov probability over a grid of lambda (around lam.in)
kmprb2 <- gridprb(lam.init=lam.in,bvec=bvec,step=0.02,
                  ydata=y.smpl,adata=a.smpl,n.fac=5,n.aver=10);
kmest <- findmxprb(prblst=kmprb2); # and find lambda with max Kolmogorov prob
cat("lambda (KS)=",exp(kmest[1])*day2yr,
    "(",exp(kmest[2])*day2yr,"-",exp(kmest[3])*day2yr,")\n");
cat("KS prob=",kmest[4],"\n");
