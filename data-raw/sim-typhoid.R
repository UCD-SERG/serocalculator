# Generate a simulated cross-sectional sample

# This script generates simulated seroresponses for incident infections
# as a Poisson process with frequency `lambda'. Responses are generated for
# antibodies given in `abs' (any subset of 1:7, names in `antibodies').
# Age range of the simulated cross-sectional record is `lifespan'.
# The size of the sample is `nrep'.
# Each individual is simulated separately, but different antibodies are
# modelled jointly (see "sim-graph.r").

# Longitudinal parameters are calculated for an age: `age.fx' (fixed age).
# However, when `age.fx' is set to NA then the age at infection is used.

# The boolean `renew.params' determines whether each infection uses a
# new set of longitudinal parameters, sampled at random from the
# posterior predictive output of  the longitudinal model. If set to FALSE
# a parameter set is chosen at birth and kept, but:
# 1. the baseline antibody levels (y0) are updated with the simulated level
#    (just) prior to infection, and
# 2. when is.na(age.fx) then the selected parameter sample is updated for the
#    age when infection occurs.

# There is also a variable `n.mc': when n.mc==0 then a random MC
# sample is chosen out of the posterior set (1:4000). When `n.mc' is
# given a value in 1:4000 then the chosen number is fixed and reused
# in any subsequent infection. This is for diagnostic purposes.

#file.simcs <- paste("./output/","sim",".rda",sep="");

#mcbasepath <- "~/stat/salmonella/typhoid/sero/long/v9na/";
ab.nm <- rbind(c("HlyE","IgA"),c("HlyE","IgG"));

antibodies <- c();
for(k.test in 1:nrow(ab.nm))
  antibodies <- c(antibodies,paste(ab.nm[k.test,1],ab.nm[k.test,2]));



abs <-  c(1,2);
age.fx <- 6; # age used for longitudinal model (fixed for now)
day2yr <- 365.25;
lambda <- 0.2; # incidence (1/yr)
npar <- 5; # y0, y1, t1, alpha, shape

lifespan <- c(0,20); # range covered in simulations
nrep <- 100; # cross-sectional sample size

dlims <- rbind(c(1e-2,5e-1),c(1e-2,5e-1)); # noise distribution
blims <- dlims; # baseline distribution

csdata <- makecs(lambda/day2yr,n.smpl=nrep,age.rng=lifespan*day2yr,
                 age.fx=age.fx,ablist=abs,n.mc=0,
                 renew.params=TRUE,add.noise=TRUE);
csdata[,1] <-  csdata[,1]/day2yr;

save(csdata,file=file.simcs,ascii=TRUE);
