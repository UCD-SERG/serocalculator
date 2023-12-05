source("sim-pars.r");
day2yr <- 365.25;

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
