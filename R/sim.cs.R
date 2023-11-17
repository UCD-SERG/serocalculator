# make a cross-sectional data set (age, y(t) set)
# and add noise, if desired
makecs <- function(lambda,n.smpl,age.rng,age.fx=NA,ablist,n.mc=0,
                   renew.params=FALSE,add.noise=FALSE){
  ysim <- simcs.tinf(lambda=lambda,n.smpl=n.smpl,
                     age.rng=age.rng,age.fx=age.fx,ablist=ablist,n.mc=n.mc,
                     renew.params=renew.params);
  if(add.noise){
    for(k.ab in 1:(ncol(ysim)-1)){
      ysim[,1+k.ab] <- ysim[,1+k.ab] +
        runif(n=nrow(ysim),min=dlims[k.ab,1],max=dlims[k.ab,2]);
    }
  }
  colnames(ysim) <- c("age",antibodies);
  return(ysim);
}
