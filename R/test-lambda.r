if(!any(objects()=="predpar")) # check if parameters loaded
  source("initpar.r"); # if not: do it now (takes a little time)
source("simfunc.r"); # sets path to C directory
source("minticks.r");

ktest <- 1; #  antigen/isotype (there are 7)
ab.nm <- c("HlyE IgA", "HlyE IgG","LPS IgA","LPS IgG", "MP IgA","MP IgG",
           "Vi IgG");
iv.mc <- initvec(ktest);
pv.mc <- parmvec(ktest);
y0.pred <- iv.mc[1,]; b0.pred <- iv.mc[2,];
mu0.pred <- pv.mc[1,]; mu1.pred <- pv.mc[2,]; c1.pred <- pv.mc[3,];
alpha.pred <- pv.mc[4,]; shape.pred <- pv.mc[5,];
max.age <- 20; # maximum nr years of circulation ("age" of subjects)

basevec <- c(-4,0.5); # lognormal B-noise parameters
lam.char <- "5e-2"; # character string (needed later) 
lam.ref <- as.numeric(lam.char); # simulated lambda 1/yr
n.y <- 2000; # size of sample for simulated EDF
n.avr <- 5; # to average D
n.ref <- 2000; # size of  cross-sectional sample
day2yr <- 365.25;

age.smpl <- runif(n=n.ref,min=0,max=max.age)*day2yr; # uniform(0,max.age)
yref <- rep(NA,n.ref);
log.lam.ref <- log(lam.ref/day2yr);
for(k.ref in 1:n.ref){
  k.mc <- sample.int(nmc,size=1);
  # yref[k.ref] <- sresprenew(age.smpl[k.ref],exp(log.lam.ref), # don't use
  yref[k.ref] <- srespmemry(age.smpl[k.ref],exp(log.lam.ref), # use this
                        iv.mc[,k.mc],pv.mc[,k.mc],basevec);
}
yref.c <- log(sort(yref));
#  grid for lambda
log.lam <- seq(from=-2.5,to=2.5,by=0.05)+log.lam.ref;
ym <- rep(NA,n.y);
mx1dev <- 1000;
mx2dev <- 1000;
yqq <- array(NA,dim=c(length(log.lam),n.y));
kmdev1 <- rep(NA,length(log.lam)); # Kolmogorov D in R
kmdev2 <- rep(NA,length(log.lam)); # Kuiper V in R
kmdev3 <- rep(NA,length(log.lam)); # Kolmogorov D in C
kmdev4 <- rep(NA,length(log.lam)); # Kuiper V in C
kmdev5 <- rep(NA,length(log.lam)); # Kolmogorov probability in C
kmdev6 <- rep(NA,length(log.lam)); # Anderson-Darling A^2 in C
for(k.lam in 1:length(log.lam)){
  kd1 <- 0; kd2 <- 0;
  for(k.avr in 1:n.avr){
    for(k.y in 1:n.y){
      k.mc <- sample.int(nmc,size=1);
      # ym[k.y] <- sresprenew(age.smpl[k.y],exp(log.lam[k.lam]),
      ym[k.y] <- srespmemry(age.smpl[k.y],exp(log.lam[k.lam]),
                            iv.mc[,k.mc],pv.mc[,k.mc],basevec);
    }
    ym.c <- log(sort(ym));
    yqq[k.lam,] <- qq(ym.c,yref.c);
    kd1 <- kd1 + findmxdev(ym.c,yref.c);
    kd2 <- kd2 + findpmdev(ym.c,yref.c);
  }
  kmdev1[k.lam] <- kd1/n.avr;
  if(kmdev1[k.lam] < mx1dev){
    mx1dev <- kmdev1[k.lam];
    ymx1.c <- ym.c;
  }
  kmdev2[k.lam] <- kd2/n.avr;
  if(kmdev2[k.lam] < mx2dev){
    mx2dev <- kmdev2[k.lam];
    ymx2.c <- ym.c;
  }
  kd <- 0;
  for(k.avr in 1:n.avr)
    kd <- kd + devsimksa(yref.c,n.y,age.smpl,exp(log.lam[k.lam]),
                    y0.pred,b0.pred,
                    mu0.pred,mu1.pred,c1.pred,alpha.pred,shape.pred,
                    basevec);
  kmdev3[k.lam] <- kd/n.avr;
  kd <- 0;
  for(k.avr in 1:n.avr)
    kd <- kd + devsimkpa(yref.c,n.y,age.smpl,exp(log.lam[k.lam]),
                    y0.pred,b0.pred,
                    mu0.pred,mu1.pred,c1.pred,alpha.pred,shape.pred,
                    basevec);
  kmdev4[k.lam] <- kd/n.avr;
  kp <- 0;
  for(k.avr in 1:(5*n.avr))
    kp <- kp + probsimksa(yref.c,n.y,age.smpl,exp(log.lam[k.lam]),
                     y0.pred,b0.pred,
                     mu0.pred,mu1.pred,c1.pred,alpha.pred,shape.pred,
                     basevec);
  kmdev5[k.lam] <- kp/(5*n.avr);
  kd <- 0;
  for(k in 1:n.avr)
    kd <- kd + devsimada(yref.c,n.y,age.smpl,exp(log.lam[k.lam]),
                         y0.pred,b0.pred,
                         mu0.pred,mu1.pred,c1.pred,alpha.pred,shape.pred,
                         basevec);
  kmdev6[k.lam] <- kd/n.avr;
  cat(k.lam," ");
}
cat("\n");
mx1 <- which(kmdev1==min(kmdev1));
mx2 <- which(kmdev2==min(kmdev2));
mx3 <- which(kmdev3==min(kmdev3));
mx4 <- which(kmdev4==min(kmdev4));
mx5 <- which(kmdev5==max(kmdev5));
mx6 <- which(kmdev6==min(kmdev6));
cat("log(lambda) =", exp(log.lam.ref)*day2yr,"(ref) \n");
cat("log(lambda) =", exp(log.lam[mx1])*day2yr,"(mx1) ",
    exp(log.lam[mx2])*day2yr,"(mx2) \n");
cat("log(lambda) =", exp(log.lam[mx3])*day2yr,"(mx3) ",
    exp(log.lam[mx4])*day2yr,"(mx4) \n");
cat("log(lambda) =",exp(log.lam[mx5])*day2yr,"(mx5) ",
    exp(log.lam[mx6])*day2yr,"(mx6) \n");

yrng <- c(min(log10(exp(yref.c)))-1,max(log10(exp(yref.c)))+1);
pdf(paste("output/lambda/yfit",lam.char,".pdf",sep=""),width=5,height=5);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(density(log10(exp(ymx1.c))),lty=3,xaxt="n",
     xlim=yrng,col="black",xlab="yfit",ylab="freq",main="");
lines(density(log10(exp(ymx2.c))),col="blue",lty=3);
lines(density(log10(exp(yref.c))),col="black");
legend("topright",c("Observed","K-S","Kuiper"),cex=1.0,bty="n",
       lty=c(1,3,3),col=c("black","black","blue"));
ticks.log(1);
dev.off();
pdf(paste("output/lambda/lambda",lam.char,".pdf",sep=""),width=4,height=4);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(0,0,col="white",xlim=log10(exp(c(-2.5,2.5)+log.lam.ref)*day2yr),
     ylim=c(0,1),xlab="",ylab="dev",xaxt="n");
lines(log10(exp(log.lam)*day2yr),(kmdev3-min(kmdev3))/(max(kmdev3)-min(kmdev3)),
      col="black",lty=1);
lines(log10(exp(log.lam)*day2yr),(kmdev4-min(kmdev4))/(max(kmdev4)-min(kmdev4)),
      col="blue",lty=1);
lines(log10(exp(log.lam)*day2yr),kmdev5,col="red",lty=1);
lines(log10(exp(log.lam)*day2yr),(kmdev6-min(kmdev6))/(max(kmdev6)-min(kmdev6)),
      col="green",lty=1);
legend("topleft",c("KS dev","KP dev","KS prob","AD dev"),cex=0.8,bty="n",
       lty=c(1,1,1,1),
       col=c("black","blue","red","green"));
ticks.log(1);
mtext(expression(paste(lambda["est"],"(1/yr)")),
      line=2.5,side=1,cex=1.3,las=1);
dev.off();

# pdf(paste("output/lambda/cumul",lam.char,".pdf",sep=""),width=5,height=5);
# par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
# plot(sort(log10(exp(yref.c))),(0:(n.ref-1))/n.ref,"l",lty=1,
#      xlim=yrng,xaxt="n",col="black",xlab="yref",ylab="freq",main="");
# lines(sort(log10(exp(ymx1.c))),(0:(n.y-1))/n.y,lty=3,col="black");
# lines(sort(log10(exp(ymx2.c))),(0:(n.y-1))/n.y,lty=3,col="blue");
# legend("bottomright",c("Observed","K-S","Kuiper"),cex=1.0,bty="n",
#        lty=c(1,3,3),col=c("black","black","blue"));
# ticks.log(1);
# dev.off();
# pdf(paste("output/lambda/qq",lam.char,".pdf",sep=""),width=5,height=5);
# par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
# plot(0,0,xlim=c(0,1),ylim=c(0,1),col="white",xlab="yref",ylab="yfit");
# for(k.lam in 1:length(log.lam))
#   lines((1:n.y)/n.y,yqq[k.lam,],col=gray(0.8));
# lines((1:n.y)/n.y,yqq[mx1,],col="black");
# lines((1:n.y)/n.y,yqq[mx2,],col="blue");
# lines(rbind(c(0,0),c(1,1)),col="red");
# legend("topleft",c("K-S","Kuiper"),cex=1.0,bty="n",lty=1,col=c("black","blue"));
# dev.off();
# pdf(paste("output/lambda/lambda",lam.char,"r.pdf",sep=""),width=4,height=4);
# par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
# # kmdev5 <- -log(kmdev5);
# # kmdev5 <- (kmdev5-min(kmdev5))/(max(kmdev5)-min(kmdev5));
# plot(0,0,col="white",xlim=log10(exp(c(-2.5,2.5)+log.lam.ref)*day2yr),
#      ylim=c(0,1),xlab="",ylab="dev",xaxt="n");
# lines(log10(exp(log.lam)*day2yr),(kmdev1-min(kmdev1))/(max(kmdev1)-min(kmdev1)),
#      col="black");
# lines(log10(exp(log.lam)*day2yr),(kmdev2-min(kmdev2))/(max(kmdev2)-min(kmdev2)),
#       col="blue");
# # lines(log10(exp(log.lam)*day2yr),(kmdev3-min(kmdev3))/(max(kmdev3)-min(kmdev3)),
# #       col="black",lty=1);
# # lines(log10(exp(log.lam)*day2yr),(kmdev4-min(kmdev4))/(max(kmdev4)-min(kmdev4)),
# #       col="blue",lty=1);
# # lines(log10(exp(log.lam)*day2yr),kmdev5,col="red",lty=1);
# # lines(log10(exp(log.lam)*day2yr),(kmdev6-min(kmdev6))/(max(kmdev6)-min(kmdev6)),
# #       col="green",lty=1);
# legend("topleft",c("K-S","Kuiper"),cex=1.0,bty="n",
#        lty=c(1,1),col=c("black","blue"));
# # legend("topleft",c("KS dev","KP dev","KS prob","AD dev"),cex=0.8,bty="n",
# #        lty=c(1,1,1,1),
# #        col=c("black","blue","red","green"));
# ticks.log(1);
# mtext(expression(paste(lambda["est"],"(1/yr)")),
#       line=2.5,side=1,cex=1.3,las=1);
# dev.off();
