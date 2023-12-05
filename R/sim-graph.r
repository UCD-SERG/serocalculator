source("minticks.r");
file.memry <- paste("./output/","simmemry-n",".pdf",sep="");
file.renew <- paste("./output/","simrenew-n",".pdf",sep="");
# load typhoid longitudinal predictive posterior parameter sample
if(!any(objects()=="predpar")) # check if parameters loaded
  source("initpar.r"); # if not: do it now (takes a little time)

init.mem <- array(NA,dim=c(dmc[1],2));
init.rnw <- array(NA,dim=c(dmc[1],2));
parm.mem <- array(NA,dim=c(dmc[1],5));
parm.rnw <- array(NA,dim=c(dmc[1],5));
ab.nm <- c("HlyE IgA", "HlyE IgG","LPS IgA","LPS IgG", "MP IgA","MP IgG",
           "Vi IgG");
ab.un <- rep("(U/ml)",length(ab.nm));
# load simulation model functions
basepath <- "~/stat/seroincidence/simulation/typhoid/"
source(paste(basepath,"C/simulv.r",sep=""));
day2yr <- 365.25;
lambda <- 0.5/day2yr; # incidence (1/yr)
lifetime <- round(20*day2yr); # total length of simulation (in days)

dlims <- c(1e-2,5e-2); # noise distribution
blims <- dlims; # baseline distribution

# n.mc = 0 randomly selected posterior sample (any  out  of 1:3000
# selects fixed sample)
# renew.params = FALSE fixes parameter sample at birth, but updates y0
# sresp <- simresp.tinf(lambda/day2yr,lifetime,age.fx,
#                       n.abs,n.mc=0,renew.params=FALSE);
k.fx <- 0;
t.next <- round(-log(runif(n=1,min=0,max=1))/lambda);
t.start <- 0;
t.inf <- t.next;
smp <-  c();  smp.t <- rep(NA,ntest);
smpr <-  c(); smpr.t <- rep(NA,ntest);
y  <- array(NA,dim=c(lifetime,ntest));
yr <- array(NA,dim=c(lifetime,ntest));
k.mc <- k.fx;
if(k.fx==0) k.mc <- sample.int(nmc,size=1);
for(ktst in 1:ntest){
  init.mem[ktst,] <- initvec(ktst)[,k.mc];
  parm.mem[ktst,] <- parmvec(ktst)[,k.mc];
  init.rnw[ktst,] <- initvec(ktst)[,k.mc];
  parm.rnw[ktst,] <- parmvec(ktst)[,k.mc];
}
tau.end <- min(t.next,lifetime);
for(tau in 1:tau.end){
  y[tau,]  <- init.mem[,1];
  yr[tau,] <- init.rnw[,1];
}
t.start <- t.next;
t.next <- t.next + round(-log(runif(n=1,min=0,max=1))/lambda);
while(t.next <= lifetime){
  t.inf <- c(t.inf,t.next);
  tau.end <- t.next - t.start;
  for(ktst in 1:ntest){
    smp.t[ktst]  <- symp(init.mem[ktst,],parm.mem[ktst,]);
    smpr.t[ktst] <- symp(init.rnw[ktst,],parm.rnw[ktst,]);
  }
  for(tau in 0:tau.end){
    tx <- tau + t.start;
    for(ktst in 1:ntest){
      y[tx,ktst]  <- ab(tau,init.mem[ktst,],parm.mem[ktst,]);
      yr[tx,ktst] <- ab(tau,init.rnw[ktst,],parm.rnw[ktst,]);
    }
  }
  if(k.fx==0) k.mc <- sample.int(nmc,size=1);
  for(ktst in 1:ntest){
    init.rnw[ktst,] <- initvec(ktst)[,k.mc];
    parm.rnw[ktst,] <- parmvec(ktst)[,k.mc];
    init.mem[ktst,] <- c(y[tx,ktst],1);
  }
  t.start <- t.next;
  t.next <- t.next + round(-log(runif(n=1,min=0,max=1))/lambda);
  for(ktst in 1:ntest){
    smp <- rbind(smp,smp.t);
    smpr <- rbind(smpr,smpr.t);
  }
}
tau.end <- lifetime - t.start;
if(tau.end > 0){
  for(ktst in 1:ntest){
    smp.t[ktst]  <- symp(init.mem[ktst,],parm.mem[ktst,]);
    smpr.t[ktst] <- symp(init.rnw[ktst,],parm.rnw[ktst,]);
  }
  for(tau in 0:tau.end){
    tx <- tau + t.start;
    for(ktst in 1:ntest){
      y[tx,ktst]  <- ab(tau,init.mem[ktst,],parm.mem[ktst,]);
      yr[tx,ktst] <- ab(tau,init.rnw[ktst,],parm.rnw[ktst,]);
    }
  }
  for(ktst in 1:ntest){
    smp <- rbind(smp,smp.t);
    smpr <- rbind(smpr,smpr.t);
  }
}
sresp <- list(y=y,t=1:lifetime,t.inf=t.inf,smp=smp);
srespr <- list(y=yr,t=1:lifetime,t.inf=t.inf,smp=smpr);

pdf(file.memry,width=9,height=18);
par(mar=c(4,4,0.2,0.2),mfrow=c(ntest,1));
for(ktst in 1:ntest){
  plot(0,0,col="white",main="",
       xlab="t (yr)",ylab=paste(ab.nm[ktst],"(",ab.un[ktst],")",sep=""),yaxt="n",
       xlim=c(0,max(sresp$t)/day2yr),
       ylim=c(-4,1.05*log10(max(sresp$y[,ktst]))));
  lines(sresp$t/day2yr,log10(sresp$y[,ktst]));
  if(length(sresp$smp[,ktst]) > 0){
    for(k.inf in 1:length(sresp$smp[,ktst])){
      if(sresp$smp[k.inf,ktst]==0)
        points(sresp$t.inf[k.inf]/day2yr,-4,#-0.05*max(sresp$y),
               pch=24,cex=1.2,col="blue",bg="blue");
      if(sresp$smp[k.inf,ktst]==1)
        points(sresp$t.inf[k.inf]/day2yr,-4,#-0.05*max(sresp$y),
               pch=24,cex=1.2,col="red",bg="red");
    }
  }
  ticks.log(2);
}
dev.off();
pdf(file.renew,width=9,height=18);
par(mar=c(4,4,0.2,0.2),mfrow=c(ntest,1));
for(ktst in 1:ntest){
  plot(0,0,col="white",main="",
       xlab="t (yr)",ylab=paste(ab.nm[ktst],"(",ab.un[ktst],")",sep=""),yaxt="n",
       xlim=c(0,max(srespr$t)/day2yr),
       ylim=c(-4,1.05*log10(max(srespr$y[,ktst]))));
  lines(srespr$t/day2yr,log10(srespr$y[,ktst]));
  if(length(srespr$smp[,ktst]) > 0){
    for(k.inf in 1:length(srespr$smp[,ktst])){
      if(srespr$smp[k.inf,ktst]==0)
        points(srespr$t.inf[k.inf]/day2yr,-4,#-0.05*max(srespr$y),
               pch=24,cex=1.2,col="blue",bg="blue");
      if(srespr$smp[k.inf,ktst]==1)
        points(srespr$t.inf[k.inf]/day2yr,-4,#-0.05*max(srespr$y),
               pch=24,cex=1.2,col="red",bg="red");
    }
  }
  ticks.log(2);
}
dev.off();
