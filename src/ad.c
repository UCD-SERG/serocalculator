#include <math.h>

/* Given an array obs[1..n1], containing observed data and an array       */
/* sim[1..n2] with simulated data this function returns the A-D           */
/* statistic a. The arrays obs and sim must be sorted into ascending order*/
double adstattwo(double *obs, int nobs, double *sim, int nsim)
{
  int jobs=1, jsim=1;
  double enobs, ensim, *u, sm=0;
  enobs = (double) nobs;
  ensim = (double) nsim;
  u = dvector(1,nobs);
  while(jobs<=nobs){                             /* while we are not done */
    if(obs[jobs-1] >  sim[jsim-1] && jsim < nsim) jsim++;  /* step in sim */
    if(obs[jobs-1] <= sim[jsim-1]){
      u[jobs]=(jsim-1)/ensim;
      if(jsim==1) u[jobs]=1/(2*ensim);
      jobs++;                                              /* step in obs */
    }
    if(jsim==nsim && jobs <= nobs){
      u[jobs]=(ensim-0.5)/ensim;
      jobs++;                                              /* step in obs */
    }
  }
  for(jobs=1;jobs<=nobs;jobs++){
    sm = sm+(2*jobs-1)*(log(u[jobs])+log(1-u[nobs-jobs+1]));
  }
  free_dvector(u,1,nobs);
  return -enobs-sm/enobs;
}
