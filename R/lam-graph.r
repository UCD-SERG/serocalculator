library(Hmisc);
source("minticks.r");

col0 <- rgb(0.5,0.5,1);
col1 <- rgb(0.8,0.8,1);
colk <- rgb(0,0,0);

lam.scalc0 <- lam.std[,2:4]/lam.std[,1];
lam.scalc1 <- lam.std[,5:7]/lam.std[,1];
lam.simul <- lam.std[,9:11]/lam.std[,1];

pdf("output/lam-est.pdf",width=4.5,height=3.5);
par(mfrow=c(1,1),mar=c(4,4.3,0.4,0.4));
errbar(x=log10(lam.std[,1])-0.04,y=lam.scalc1[,1],
       yminus=lam.scalc1[,2],yplus=lam.scalc1[,3],col=col0,errbar.col=col0,
       add=FALSE,xaxt="n",pch=16,
       xlim=c(-2,1),ylim=c(0,2),xlab="",ylab="");
errbar(x=log10(lam.std[,1])+0.04,y=lam.scalc0[,1],
       yminus=lam.scalc0[,2],yplus=lam.scalc0[,3],col=col1,errbar.col=col1,
       add=TRUE);
errbar(x=log10(lam.std[,1]),y=lam.simul[,1],
       yminus=lam.simul[,2],yplus=lam.simul[,3],col=colk,errbar.col=colk,
       add=TRUE);
lines(c(-3,1),c(1,1),col="black",lty=2);
mtext(expression(paste(frac(lambda["est"],lambda["sim"]))),
      line=2.5,side=2,cex=1.3,las=2);
mtext(expression(paste(lambda["sim"])),
      line=3,side=1,cex=1.3,las=1);
ticks.log(1);
legend("topright",c("ML (B 98%)","ML (B 99.99%)","KS"),
       cex=0.8,bty="n",lty=c(1,1,1),lwd=c(1.5,1.5,1.5),col=c(col0,col1,colk));
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
