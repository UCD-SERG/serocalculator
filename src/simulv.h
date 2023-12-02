/* Function definitions */
double afn (double *iv, double *pv);
double bfn (double *pv);
double t1fn (double *iv, double *pv);
double y1fn (double *iv, double *pv);
double ag (double  t, double *iv, double *pv);
double ab (double  t, double *iv, double *pv);
int symp (double *iv, double *pv);
double tinterv (double lambda);
int rndind (int veclen);
double rndunif (double *basel);
double rndlnorm (double *basel);
double srespmemry_old (double age, double  lambda,
		       double *iv, double *pv, double *bsl);
double sresprenew_old (double age, double  lambda,
		       double *iv, double *pv, double *bsl);
double srespmemry (double age, double  lambda,
		   double *iv, double *pv, double *bsl);
double sresprenew (double age, double  lambda,
		   double *iv, double *pv, double *bsl);
double srespmemry_noise (double age, double  lambda,
		         double *iv, double *pv, double *bsl);
double sresprenew_noise (double age, double  lambda,
		         double *iv, double *pv, double *bsl);

double drndks (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl);
double drndksrenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda, int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl);
double drndkp (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl);
double drndkprenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda, int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl);
double drndksp (double *logyobs, int nobs, int nsim, double age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl);
double drndksprenew (double *logyobs, int nobs, int nsim, double age,
                     double lambda, int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl);
double drndad (double *logyobs, int nobs, int nsim, double age,
               double lambda, int nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl);
double drndadrenew (double *logyobs, int nobs, int nsim, double age,
                    double lambda, int nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl);

double drndksa (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl);
double drndksarenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda, int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl);
double drndkpa (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl);
double drndkparenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda, int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl);
double drndksap (double *logyobs, int nobs, int nsim, double *age,
                 double lambda, int nmc, double *y0, double *b0,
                 double *mu0, double *mu1, double *c, double *alpha,
                 double *shape, double *bsl);
double drndksaprenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda, int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl);
double drndada (double *logyobs, int nobs, int nsim, double *age,
                double lambda, int nmc, double *y0, double *b0,
                double *mu0, double *mu1, double *c, double *alpha,
                double *shape, double *bsl);
double drndadarenew (double *logyobs, int nobs, int nsim, double *age,
                     double lambda, int nmc, double *y0, double *b0,
                     double *mu0, double *mu1, double *c, double *alpha,
                     double *shape, double *bsl);

int *ivector(long nl, long nh);
void free_ivector(int *v, long nl, long nh);
double *dvector(long nl, long nh);
void free_dvector(double *v, long nl, long nh);
void prerr(char error_text[]);
void sort(double *arr, int n);
double probks(double alam);
double ksdevtwo(double *data1, int n1, double *data2, int n2);
double ksprobtwo(double *data1, int n1, double *data2, int n2);
double kpdevtwo(double *data1, int n1, double *data2, int n2);

double adstattwo(double *obs, int nobs, double *sim, int nsim);

/* Utility functions for calls from R */
void ar (double *res, double *iv, double *pv){
  *res = afn(iv,pv);
}
void br (double *res, double *pv){
  *res = bfn(pv);
}
void t1r (double *res, double *iv, double *pv){
  *res = t1fn(iv,pv);
}
void y1r (double *res, double *iv, double *pv){
  *res = y1fn(iv,pv);
}
void agr (double *res, double *t, double *iv, double *pv){
  *res = ag(*t,iv,pv);
}
void abr (double *res, double *t, double *iv, double *pv){
  *res = ab(*t,iv,pv);
}
void sympr (int *res, double *iv, double *pv){
  *res = symp(iv,pv);
}
void tintervr (double *res, double *lambda){
  *res = tinterv(*lambda);
}
void rndindr (int *res, int *veclen){
  *res = rndind(*veclen);
}
void rndunifr (double *res, double *basel){
  *res = rndunif(basel);
}
void rndlnormr (double *res, double *basel){
  *res = rndlnorm(basel);
}
void srespmemryr (double *res, double *age, double *lambda,
		  double *iv, double *pv, double *bsl){
  *res = srespmemry_noise(*age,*lambda,iv,pv,bsl);
}
void sresprenewr (double *res, double *age, double *lambda,
		  double *iv, double *pv, double *bsl){
  *res = sresprenew_noise(*age,*lambda,iv,pv,bsl);
}

void drndksr (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndks (logyobs,*nobs,*nsim,*age,*lambda,
                 *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndksrenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndksrenew (logyobs,*nobs,*nsim,*age,*lambda,
                      *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndkpr (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndkp (logyobs,*nobs,*nsim,*age,*lambda,
                 *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndkprenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndkprenew (logyobs,*nobs,*nsim,*age,*lambda,
                      *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndkspr (double *res, double *logyobs, int *nobs,
               int *nsim, double *age, double *lambda,
               int *nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl){
  *res = drndksp (logyobs,*nobs,*nsim,*age,*lambda,
                  *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndksprenewr (double *res, double *logyobs, int *nobs,
                    int *nsim, double *age, double *lambda,
                    int *nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl){
  *res = drndksprenew (logyobs,*nobs,*nsim,*age,*lambda,
                       *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndadr (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndad (logyobs,*nobs,*nsim,*age,*lambda,
                 *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndadrenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndadrenew (logyobs,*nobs,*nsim,*age,*lambda,
                      *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}

void drndksar (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndksa (logyobs,*nobs,*nsim,age,*lambda,
                  *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndksarenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndksarenew (logyobs,*nobs,*nsim,age,*lambda,
                       *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndkpar (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndkpa (logyobs,*nobs,*nsim,age,*lambda,
                  *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndkparenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndkparenew (logyobs,*nobs,*nsim,age,*lambda,
                       *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndksapr (double *res, double *logyobs, int *nobs,
               int *nsim, double *age, double *lambda,
               int *nmc, double *y0, double *b0,
               double *mu0, double *mu1, double *c, double *alpha,
               double *shape, double *bsl){
  *res = drndksap (logyobs,*nobs,*nsim,age,*lambda,
                   *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndksaprenewr (double *res, double *logyobs, int *nobs,
                    int *nsim, double *age, double *lambda,
                    int *nmc, double *y0, double *b0,
                    double *mu0, double *mu1, double *c, double *alpha,
                    double *shape, double *bsl){
  *res = drndksaprenew (logyobs,*nobs,*nsim,age,*lambda,
                        *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndadar (double *res, double *logyobs, int *nobs,
              int *nsim, double *age, double *lambda,
              int *nmc, double *y0, double *b0,
              double *mu0, double *mu1, double *c, double *alpha,
              double *shape, double *bsl){
  *res = drndada (logyobs,*nobs,*nsim,age,*lambda,
                  *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}
void drndadarenewr (double *res, double *logyobs, int *nobs,
                   int *nsim, double *age, double *lambda,
                   int *nmc, double *y0, double *b0,
                   double *mu0, double *mu1, double *c, double *alpha,
                   double *shape, double *bsl){
  *res = drndadarenew (logyobs,*nobs,*nsim,age,*lambda,
                       *nmc, y0, b0, mu0, mu1, c, alpha, shape,bsl);
}

void ksdevtwor (double *res, double *data1, int *n1, double *data2, int *n2){
  *res = ksdevtwo(data1,*n1,data2,*n2);
}

void ksprobtwor (double *res, double *data1, int *n1, double *data2, int *n2){
  *res = ksprobtwo(data1,*n1,data2,*n2);
}

void kpdevtwor (double *res, double *data1, int *n1, double *data2, int *n2){
  *res = kpdevtwo(data1,*n1,data2,*n2);
}

void adstattwor (double *res, double *obs, int *nobs, double *sim, int *nsim){
  *res = adstattwo(obs,*nobs,sim,*nsim);
}
