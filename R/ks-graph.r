library(Hmisc);
source("minticks.r");

yrng <- c(min(log10(y.smpl))-1,max(log10(y.smpl))+1);

pdf("output/cumul.pdf",width=5,height=5);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(sort(log10(y.smpl)),(0:(n.smpl-1))/n.smpl,"l",lty=1,
     xlim=yrng,xaxt="n",col="black",xlab="y",ylab="freq",main="");
lines(sort(log10(exp(ymx1))),(0:(length(ymx1)-1))/length(ymx1),lty=3,col="black");
legend("bottomright",c("Observed","K-S"),cex=1.0,bty="n",
       lty=c(1,3),col=c("black","black"));
ticks.log(1);
dev.off();

pdf("output/qq.pdf",width=5,height=5);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(0,0,xlim=c(0,1),ylim=c(0,1),col="white",xlab="",ylab="");
for(k.lam in 1:length(log.lam3))
  lines((1:length(ymx1))/length(ymx1),yqq[k.lam,],col=gray(0.8));
lines((1:length(ymx1))/length(ymx1),yqq[mx1,],col="black");
lines(rbind(c(0,0),c(1,1)),col="red");
legend("topleft",c("K-S"),cex=1.0,bty="n",lty=1,col=c("black"));
mtext(expression("y"["smpl"]),line=1,side=2,cex=1.5,las=2);
mtext(expression("y"["sim"]), line=3,side=1,cex=1.5,las=1);
dev.off();

pdf("output/yfit.pdf",width=5,height=5);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(density(log10(exp(ymx1))),lty=3,xaxt="n",
     xlim=yrng,col="black",xlab="y",ylab="freq",main="");
lines(density(log10(y.smpl)),col="black");
legend("topright",c("Observed","K-S"),cex=1.0,bty="n",
       lty=c(1,3,3),col=c("black","black"));
ticks.log(1);
dev.off();

pdf("output/lambda.pdf",width=5,height=5);
par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
plot(log10(exp(log.lam3)*day2yr),(kmdev1-min(kmdev1))/(max(kmdev1)-min(kmdev1)),
     "l",col="black",xlab="",ylab="",xaxt="n");
lines(log10(exp(log.lam)*day2yr),(kmdev3-min(kmdev3))/(max(kmdev3)-min(kmdev3)),
      col="black",lty=2);
# lines(log10(exp(log.lam2)*day2yr),kmdev5,col="red",lty=1);
lines(lowess(log10(exp(log.lam2)*day2yr),kmdev5,f=0.05),col="red",lty=1);
legend("topleft",c("K-S","K-S (C)","K-S prob (C)"),cex=1.0,bty="n",
       lty=c(1,2,1),
       col=c("black","black","red"));
mtext("dev",line=2,side=2,cex=1,las=2);
mtext(expression(paste(lambda["sim"]," (1/yr)")),
      line=3,side=1,cex=1.5,las=1);
ticks.log(1);
dev.off();

# pdf("output/lambda.pdf",width=5,height=5);
# par(mfrow=c(1,1),mar=c(4,4,0.4,0.4));
# plot(log10(exp(log.lam)*day2yr),(kmdev3-min(kmdev3))/(max(kmdev3)-min(kmdev3)),
#      "l",col="black",
#      xlab="lambda (1/yr)",ylab="dev",xaxt="n",ylim=c(-0.05,1));
# lines(lowess(log10(exp(log.lam2)*day2yr),kmdev5,f=0.05),col="red",lty=1);
# # lines(log10(exp(log.lam2)*day2yr),kmdev5,col="red",lty=1);
# # lines(log10(exp(log.lam)*day2yr),(lldev-min(lldev))/(max(lldev)-min(lldev)),
# #       col="blue",lty=1);
# legend("topleft",c("K-S","K-S prob","-loglik"),cex=1.0,bty="n",
#        lty=c(1,1),
#        col=c("black","red","blue"));
# points(log10(lam.est[1]),-0.05,pch=9,col="blue");
# lines(c(log10(lam.est[2]),log10(lam.est[3])),c(-0.05,-0.05),lty=1,col="blue");
# lines(c(log10(lam.est[2]),log10(lam.est[2])),c(-0.04,-0.06),lty=1,col="blue");
# lines(c(log10(lam.est[3]),log10(lam.est[3])),c(-0.04,-0.06),lty=1,col="blue");
# points(log10(lam.ref*day2yr),-0.02,pch=2);
# ticks.log(1);
# dev.off();
