/* compile with: R CMD SHLIB serocalc.c */
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include "serocalc.h"

/* SIM equation 20: negative log likelihood function  (incl B and  M noise) */
double NLLFf (double lambda, double *yy, double *aa, int nsubj,
              double nu, double eps, double step, double yLo, double yHi,
              double *A, double *k, double *d, int nmc){
  double llf,y,age,Pa,Qa,EXPla,rho;
  int subj,samp;

  /* Calculate LLF */
  llf = 0;
  if(0 < nu){                            /* B noise */
    for(subj=1; subj <= nsubj; subj++){ /* over all subjects */
      y = yy[subj-1];
      age = aa[subj-1];
      Pa = 1-exp(-lambda*age);           /* 1 or more seroconv before age a */
      Qa = 1 - Pa;                       /* No seroconv before age a */
      EXPla = exp(-lambda*age)/age;
      if((yLo < y) && (y < yHi)){        /* Uncensored: yLo < y < yHi */
        rho = 0;
        if(0 < eps){                     /* M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + dnsBM(lambda,y,age,nu,eps,step,Pa,Qa,EXPla,
                              A[samp-1],k[samp-1],d[samp-1]);
          }
        }else{                           /* no M noise */
          for(samp=1; samp<=nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + dnsB(lambda,y,age,nu,Pa,Qa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        }
        rho = rho/nmc;
      }
      if(y <= yLo){                      /* Left censored: y <= yLo */
        rho = 0;
        if(0 < eps){                     /* M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + prbBM(lambda,yLo,age,nu,eps,step,Pa,Qa,EXPla,
                              A[samp-1],k[samp-1],d[samp-1]);
          }
        } else{                          /* no M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + prbB(lambda,yLo,age,nu,step,Pa,Qa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        }
        rho = rho/nmc;
      }
      if(yHi <= y){                      /* Right censored: y >= yHi */
        rho = 0;
        for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
          rho = rho + prbB(lambda,yHi,age,nu,step,Pa,Qa,EXPla,
                           A[samp-1],k[samp-1],d[samp-1]);
        }
        rho = 1 - rho/nmc;
      }
      if(0 < rho) llf = llf - log(rho);  /* over all subjects */
    }
  }else {                                /* no B noise */
    for(subj=1; subj <= nsubj; subj++){ /* over all subjects */
      y = yy[subj-1];
      age = aa[subj-1];
      Pa = 1 - exp(-lambda*age);         /* 1 or more seroconv before age a */
      Qa = 1 - Pa;                       /* No seroconv before age a */
      EXPla = exp(-lambda*age)/age;
      if((yLo < y) && (y < yHi)){        /* Uncensored: yLo < y < yHi */
        rho = 0;
        if(0 < eps){                     /* M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + dnsM(lambda,y,age,eps,step,Pa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        } else {                         /* no M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + dnsF(lambda,y,age,Pa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        }
        rho = rho/nmc;
      }
      if(y <= yLo){                      /* Left censored: y <= yLo */
        rho = 0;
        if(0 < eps){                     /* M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + prbM(lambda,yLo,age,eps,step,Pa,Qa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        } else {                         /* no M noise */
          for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
            rho = rho + prbF(lambda, yLo,age,Pa,Qa,EXPla,
                             A[samp-1],k[samp-1],d[samp-1]);
          }
        }
        rho = rho/nmc;
      }
      if(yHi <= y){                      /* Right censored: y >= yHi */
        rho = 0;
        for(samp=1; samp <= nmc; samp++){ /* Integrate over MC param smpl */
          rho = rho + prbF(lambda,yHi,age,Pa,Qa,EXPla,
                           A[samp-1],k[samp-1],d[samp-1]);
        }
        rho = 1 - rho/nmc;
      }
      if(0 < rho) llf = llf - log(rho);   /* over all subjects */
    }
  }
  return(llf);
}

/* SIM equation 8: fundamental density for y: no noise */
double dnsF (double lambda, double y, double age, double Pa,
             double EXPla, double A, double k, double d){
  double ya,tau,dns_f;

  ya = A/pow((1+d*(pow(A,d))*k*age),(1/d));
  dns_f = 0;
  if((ya <= y) && (y <= A)){
    tau = (pow(y,-d) - pow(A,-d))/(k*d);
    dns_f = Pa*(lambda*exp(-lambda*tau)+EXPla)/(k*(pow(y,1+d)));
  }
  /*printf("%f\n",A);*/
  return(dns_f);
}

/* SIM equation 8: fundamental probability function for y: no noise */
double prbF (double lambda, double y, double age, double Pa, double Qa,
             double EXPla, double A, double k, double d){
  double ya,tau,prb_f;

  ya = A/pow((1+d*(pow(A,d))*k*age),(1/d));
  prb_f = 0;

  if(0 <= y) prb_f = Qa;
  if((ya <= y) && (y <= A)){
    tau = (pow(y,-d) - pow(A,-d))/(k*d);
    prb_f = prb_f + Pa*(exp(-lambda*tau) - tau*EXPla);
  }
  if(A < y) prb_f = 1;
  return(prb_f);
}

/* SIM equation 11: density of y with B noise present */
double dnsB (double lambda, double y, double age, double nu,
             double Pa, double Qa, double EXPla, double A, double k, double d){
  double dns_b;

  dns_b = (prbF(lambda,y,age,Pa,Qa,EXPla,A,k,d) -
           prbF(lambda,y-nu,age,Pa,Qa,EXPla,A,k,d))/nu;
  return(dns_b);
}

/* SIM equation 12: probability function for y with B noise present */
double prbB (double lambda, double y, double age, double nu, double step,
             double Pa, double Qa, double EXPla, double A, double k, double d){
  double ya,y1,zmin,zmax,z,dz,integ,prb_b;
  int Nstep, kstep;

  ya = A/pow((1+d*(pow(A,d))*k*age),(1/d));
  prb_b = 0;

  zmin = dmax(0,y-nu);
  zmax = dmin(y,ya);
  if(zmin < zmax) prb_b = (zmax - zmin)*Qa/nu;

  zmin = dmax(ya,y-nu);
  zmax = dmin(y,A);
  if(zmin < zmax){                       /* Numerical  integration */
    Nstep = dint((zmax - zmin)/step) + 1;
    dz = (zmax - zmin)/Nstep;
    integ = 0;
    for(kstep=1; kstep <= Nstep; kstep++){
      z = zmin +(kstep - 0.5)*dz;
      integ = integ + dz*prbF(lambda,z,age,Pa,Qa,EXPla,A,k,d);
    }
    prb_b = prb_b + integ/nu;
  }
  zmin = dmax(A,y-nu);
  zmax = y;
  if(zmin < zmax){
    prb_b = prb_b + (zmax - zmin)/nu;
  }
  return(prb_b);
}

/* SIM equation 14: density of y with M noise present */
double dnsM (double lambda, double y, double age, double eps, double step,
             double Pa, double EXPla, double A, double k, double d){
  double ya,zmin,zmax,dz,integ,z,dns_m;
  int Nstep,kstep;

  ya = A/pow((1 + d*(pow(A,d))*k*age),(1/d));
  dns_m = 0;

  zmin = dmax(y/(1+eps),ya);
  zmax = dmin(y/(1-eps),A);
  if(zmin < zmax){                       /* Numerical integration */
    Nstep = dint((zmax - zmin)/step) + 1;
    dz = (zmax - zmin)/Nstep;
    integ = 0;
    /* riemann integral over possible true underlying values: */
    for(kstep = 1; kstep <= Nstep; kstep++){
      z = zmin + (kstep - 0.5)*dz;
      integ = integ + dz*dnsF(lambda,z,age,Pa,EXPla,A,k,d)/z;
    }
    dns_m = integ/(2*eps);
  }
  return(dns_m);
}

/* SIM equation 14: probability function of y with M noise present */
double prbM (double lambda, double y, double age, double eps, double step,
             double Pa, double Qa, double EXPla,
             double A, double k, double d){
  double ymin,ymax,prb_m;

  prb_m = 0;
  if(0 < y){
    ymin = y/(1 + eps);
    ymax = y/(1 - eps);
    prb_m = ((1+eps)*prbF(lambda,ymin,age,Pa,Qa,EXPla,A,k,d) -
             (1-eps)*prbF(lambda,ymax,age,Pa,Qa,EXPla,A,k,d))/(2*eps) +
            y*dnsM(lambda,y,age,eps,step,Pa,EXPla,A,k,d);
  } else {
    if(y == 0) prb_m = Qa;
  }
  return(prb_m);
}

/* SIM equation 15: density of y with both B and M  noise present */
double dnsBM (double lambda, double y, double age, double nu, double eps,
              double step, double Pa, double Qa, double EXPla,
              double A, double k, double d){
  double ya,ymin,ymax,zmin,zmax,dns_bm;

  ya = A/pow((1 + d*(pow(A,d))*k*age),(1/d));
  ymin = y/(1 + eps);
  ymax = y/(1 - eps);
  dns_bm = 0;

  if(nu <= ya){
    zmin = dmax(0,ymin);
    zmax = dmin(ymax,nu);
    if(zmin < zmax){
      dns_bm = (Qa/nu)*log(zmax/zmin)/(2*eps);
    }
    zmin = dmax(nu,ymin);
    zmax = dmin(ymax,ya);
    if(zmin < zmax){
      dns_bm = dns_bm +
        IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
    }
    zmin = dmax(ya,ymin);
    zmax = dmin(ymax,dmin(ya+nu,A));
    if(zmin < zmax){
      dns_bm = dns_bm +
        IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
    }
    zmin = dmax(dmin(ya+nu,A),ymin);
    zmax = dmin(ymax,A+nu);
    if(zmin < zmax){
      dns_bm = dns_bm +
        IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
    }
  } else {
    if(nu < A){
      zmin = dmax(0,ymin);
      zmax = dmin(ymax,ya);
      if(zmin < zmax){
        dns_bm = (Qa/nu)*log(zmax/zmin)/(2*eps);
      }
      zmin = dmax(ya,ymin);
      zmax = dmin(ymax,nu);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(nu,ymin);
      zmax = dmin(ymax,dmin(ya+nu,A));
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(dmin(ya+nu,A),ymin);
      zmax = dmin(ymax,dmax(ya+nu,A));
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(dmax(ya+nu,A),ymin);
      zmax = dmin(ymax,A+nu);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
    } else {
      zmin = dmax(0,ymin);
      zmax = dmin(ymax,ya);
      if(zmin < zmax){
        dns_bm = (Qa/nu)*log(zmax/zmin)/(2*eps);
      }
      zmin = dmax(ya,ymin);
      zmax = dmin(ymax,A);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(A,ymin);
      zmax = dmin(ymax,nu);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(A,ymin);
      zmax = dmin(ymax,ya+nu);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
      zmin = dmax(ya+nu,ymin);
      zmax = dmin(ymax,A+nu);
      if(zmin < zmax){
        dns_bm = dns_bm +
          IdnsB(zmin,zmax,lambda,y,age,nu,step,Pa,Qa,EXPla,A,k,d)/(2*eps);
      }
    }
  }
  return(dns_bm);
}

/* SIM equation 15: probability function of y with both B and M  noise present */
double prbBM (double lambda, double y, double age, double nu, double eps,
              double step, double Pa, double Qa, double EXPla,
              double A, double k, double d){
  double meps, peps, ymin, ymax, prb_bm;

  meps = 1 - eps;
  peps = 1 + eps;
  ymin = y/peps;
  ymax = y/meps;
  prb_bm = (peps*prbB(lambda,ymin,age,nu,step,Pa,Qa,EXPla,A,k,d) -
            meps*prbB(lambda,ymax,age,nu,step,Pa,Qa,EXPla,A,k,d))/(2*eps) +
    y*dnsBM(lambda,y,age,nu,eps,step,Pa,Qa,EXPla,A,k,d);
  return(prb_bm);
}

/* numerical integration of density of y with B noise */
double IdnsB (double zmin, double zmax, double lambda, double y, double age,
              double nu, double step, double Pa, double Qa, double EXPla,
              double A, double k, double d){
  double dz, z, integ;
  int Nstep, kstep;

  Nstep = dint((zmax - zmin)/step)+1;
  dz = (zmax - zmin)/Nstep;
  integ = 0;
  for(kstep=1; kstep <= Nstep; kstep++){
    z = zmin + (kstep - 0.5) * dz;
    integ = integ + dz*dnsB(lambda,z,age,nu,Pa,Qa,EXPla,A,k,d)/z;
  }
return(integ);
}
