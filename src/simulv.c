/* compile with R CMD SHLIB simulv.c */
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "simulv.h"
#include "ks.h"
#include "ad.h"
// #include "ks.c"
// #include "ad.c"

#define MAXINF 25 /* maximum nr past infections accounted for */

/* parameter vector pv: (mu0,mu1,c1,alpha,shape) */
/* initials vector iv: (y0,b0) */
double afn (double *iv, double *pv){
  return (pv[1]-pv[0])/(pv[2]*iv[0]);
}
double bfn (double *pv){
  return pv[1]/(pv[1]-pv[0]);
}
double t1fn (double *iv, double *pv){
  return log(1+afn(iv,pv)*iv[1])/(pv[1]-pv[0]);
}
double y1fn (double *iv, double *pv){
  return iv[0]*pow((1+afn(iv,pv)*iv[1]),bfn(pv));
}
/* pathogen  kinetics: a(nti)g(en) response */
double ag (double t, double *iv, double *pv){
  double bt = 0;
  if(t > t1fn(iv,pv)) return bt;
  bt = iv[1]*exp(pv[0]*t)-pv[1]*iv[0]*(exp(pv[0]*t)-exp(pv[1]*t))/(pv[0]-pv[1]);
  return bt;
}
/* a(nti)b(ody) kinetics: power function decay */
double ab (double t, double *iv, double *pv){
  double yt;
  double t1 = t1fn(iv,pv);
  double y1 = y1fn(iv,pv);
  if(t <= t1) yt = iv[0]*exp(pv[1]*t);
  if(t >  t1) yt = pow(pow(y1,1-pv[4])-(1-pv[4])*pv[3]*(t - t1),1/(1-pv[4]));
  return yt;
}
/* symptomatic or asymptomatic seroconversion? */
int symp (double *iv, double *pv){
  int smp = 0;
  double ymin = pv[0]*iv[1]/pv[2];
  if(ymin >= iv[0]) smp = 1;
  return smp;
}
/* generate Poisson random interval */
double tinterv (double lambda){
  double u;
  GetRNGstate();
  u = -log(runif(0.0,1.0))/lambda;
  PutRNGstate();
  return u;
}

/* generate random index */
int rndind (int veclen){
  int k;
  double u;
  GetRNGstate();
  u = runif(0.0,1.0);
  PutRNGstate();
  u = u*(veclen-1);
  k = (int) u;
  return k;
}

/* generate random baseline  antibody level */
double rndunif (double *basel){
  double u;
  GetRNGstate();
  u = runif(basel[0],basel[1]);
  PutRNGstate();
  return u;
}

double rndlnorm (double *basel){
  double fac, rsq, v1, v2;
  do{
    GetRNGstate();
    v1 = 2.0 * runif(0.0,1.0) - 1.0; /* Sample two random numbers in the  */
    PutRNGstate();
    GetRNGstate();
    v2 = 2.0 * runif(0.0,1.0) - 1.0; /* square (-1,1) in either direction */
    PutRNGstate();
    rsq = v1 * v1 + v2 * v2;         /* and check if they are in the unit */
  } while (rsq >= 1.0 || rsq == 0.0); /* circle. If not, sample again.    */
  fac = sqrt(-2.0 * log(rsq)/rsq);    /* Box-Muller transformation.       */
  return exp(basel[0] + basel[1] * v1 * fac);
}

double srespmemry_old (double age, double lambda,
		   double *iv, double *pv, double *bsl){
  int k;
  double tau, arst, basl[2];
  k = 0; basl[0] = iv[0]; basl[1] = iv[1];
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return rndlnorm(bsl); /* no infection */
  k = k + 1;
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* next infection */
  if(arst < tau) return ab(arst,basl,pv); /* 1 inf */
  k =  k + 1;
  arst = arst - tau; /* remaining time */
  while(k < 100){ /* maximum number seroconversions */
    basl[0] = ab(tau,basl,pv);
    tau = tinterv(lambda); /* next infection */
    if(arst < tau) return ab(arst,basl,pv);
    k = k + 1;
    arst = arst - tau;
  }
}

double sresprenew_old (double age, double lambda,
		   double *iv, double *pv, double *bsl){
  int k;
  double tau, arst,basl[2];
  k = 0; basl[0] = iv[0]; basl[1] = iv[1];
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return rndlnorm(bsl); /* no infection */
  k = k + 1;
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* next infection */
  if(arst < tau) return ab(arst,iv,pv); /* 1 inf */
  k =  k + 1;
  arst = arst - tau; /* remaining time */
  while(k < 100){ /* maximum number seroconversions */
    tau = tinterv(lambda); /* next infection */
    if(arst < tau) return ab(arst,iv,pv);
    k = k + 1;
    arst = arst - tau;
  }
}

double srespmemry (double age, double lambda,
		   double *iv, double *pv, double *bsl){
  double tau, arst, basl[2];
  basl[0] = iv[0]; basl[1] = iv[1];
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return rndlnorm(bsl); /* no infection */
  if(age*lambda > MAXINF) tau = age - MAXINF/lambda; /* < MAXINF infections */
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* time to next infection */
  if(arst < tau) return ab(arst,basl,pv); /* 1 inf */
  while(arst > tau){ /* is there time for another infection?*/
    arst = arst - tau; /* remaining time after next infection */
    basl[0] = ab(tau,basl,pv); /* baseline for current infection */
    tau = tinterv(lambda); /* time to next infection */
  }
  return ab(arst,basl,pv);
}

double sresprenew (double age, double lambda,
		   double *iv, double *pv, double *bsl){
  double tau, arst,basl[2];
  basl[0] = iv[0]; basl[1] = iv[1];
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return rndlnorm(bsl); /* no infection */
  if(age*lambda > MAXINF) tau = age - MAXINF/lambda; /* < MAXINF infections */
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* time to next infection */
  if(arst < tau) return ab(arst,iv,pv); /* 1 inf */
  while(arst > tau){ /* is there time for another infection? */
    arst = arst - tau;
    tau = tinterv(lambda); /* time to next infection */
  }
  return ab(arst,iv,pv);
}

double srespmemry_noise (double age, double lambda,
                         double *iv, double *pv, double *bsl){
  double tau, arst, basl[2], ynoise;
  basl[0] = iv[0]; basl[1] = iv[1];
  ynoise = rndlnorm(bsl); /* need this only once */
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return ynoise; /* no infection */
  if(age*lambda > MAXINF) tau = age - MAXINF/lambda; /* < MAXINF infections */
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* time to next infection */
  if(arst < tau) return ab(arst,basl,pv)+ynoise; /* 1 inf */
  while(arst > tau){ /* is there time for another infection?*/
    arst = arst - tau; /* remaining time after next infection */
    basl[0] = ab(tau,basl,pv); /* baseline for current infection */
    tau = tinterv(lambda); /* time to next infection */
  }
  return ab(arst,basl,pv)+ynoise;
}

double sresprenew_noise (double age, double lambda,
                         double *iv, double *pv, double *bsl){
  double tau, arst,basl[2], ynoise;
  basl[0] = iv[0]; basl[1] = iv[1];
  ynoise = rndlnorm(bsl); /* need this only once */
  tau = tinterv(lambda); /* age at first infection */
  if(age < tau) return ynoise; /* no infection */
  if(age*lambda > MAXINF) tau = age - MAXINF/lambda; /* < MAXINF infections */
  arst = age - tau; /* remaining time */
  tau = tinterv(lambda); /* time to next infection */
  if(arst < tau) return ab(arst,iv,pv)+ynoise; /* 1 inf */
  while(arst > tau){ /* is there time for another infection? */
    arst = arst - tau;
    tau = tinterv(lambda); /* time to next infection */
  }
  return ab(arst,iv,pv)+ynoise;
}

/* From here Kolmogorov-Smirnov deviate, Kuiper deviate           */
/* and Kolmogorov-Smirnov probability                             */
/* for infections ignoring y0 (renew) and carried over y0 (memry) */
/* First assume all samples taken from subjects of same age       */

/* yobs  must be sorted!! */
double drndks (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = ksdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! */
double drndksrenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda,int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = ksdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! */
double drndkp (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = kpdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! */
double drndkprenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda,int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = kpdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! */
double drndksp (double *logyobs, int nobs, int nsim, double age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl){
  int kmc, ksim;
  double prb, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  prb = ksprobtwo(logyobs,nobs,logysim,nsim);
  return prb;
}

/* yobs  must be sorted!! */
double drndksprenew (double *logyobs, int nobs, int nsim, double age,
                     double lambda,int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl){
  int kmc, ksim;
  double prb, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  prb = ksprobtwo(logyobs,nobs,logysim,nsim);
  return prb;
}

/* yobs  must be sorted!! */
double drndad (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = adstattwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! */
double drndadrenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda,int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age,lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = adstattwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* From here Kolmogorov-Smirnov deviate, Kuiper deviate           */
/* and Kolmogorov-Smirnov probability                             */
/* for infections ignoring y0 (renew) and carried over y0 (memry) */
/* Second assume samples taken from subjects of given ages        */

/* yobs  must be sorted!! (and ages accordingly) */
double drndksa (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = ksdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndksarenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda,int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = ksdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndkpa (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = kpdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndkparenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda,int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = kpdevtwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndksap (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl){
  int kmc, ksim;
  double prb, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  prb = ksprobtwo(logyobs,nobs,logysim,nsim);
  return prb;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndksaprenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda,int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl){
  int kmc, ksim;
  double prb, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  prb = ksprobtwo(logyobs,nobs,logysim,nsim);
  return prb;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndada (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(srespmemry_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = adstattwo(logyobs,nobs,logysim,nsim);
  return dev;
}

/* yobs  must be sorted!! (and ages accordingly) */
double drndadarenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda,int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl){
  int kmc, ksim;
  double dev, ivmc[2], pvmc[5], logysim[nsim];
  for(ksim=0; ksim < nsim; ksim++){
    kmc = rndind(nmc);
    ivmc[0] = y0[kmc]; ivmc[1] = b0[kmc];
    pvmc[0] = mu0[kmc]; pvmc[1] = mu1[kmc]; pvmc[2] = c[kmc];
    pvmc[3] = alpha[kmc]; pvmc[4] = shape[kmc];
    logysim[ksim] = log(sresprenew_noise(age[ksim],lambda,ivmc,pvmc,bsl));
  }
  sort(logysim,nsim);
  dev = adstattwo(logyobs,nobs,logysim,nsim);
  return dev;
}
