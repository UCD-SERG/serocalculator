/* Auxiliary functions and function definitions for serocalc.c */
int dint(double x)
{
  return((int)x);
}

double dmax(double a, double b)
{
  if(a > b) return(a);
  else return(b);
}

double dmin(double a, double b)
{
  if(a < b) return(a);
  else return(b);
}

/* Function definitions */
double NLLFf (double lambda, double *y, double *a, int nsubj,
             double nu, double eps, double step, double yLo, double yHi,
             double *A, double *k, double *d, int nmc);
double dnsF (double lambda, double y, double age, double Pa,
             double EXPla, double A, double k, double d);
double prbF (double lambda, double y, double age, double Pa, double Qa,
             double EXPla, double A, double k, double d);
double dnsB (double lambda, double y, double age, double nu,
             double Pa, double Qa, double EXPla, double A, double k, double d);
double prbB (double lambda, double y, double age, double nu, double step,
             double Pa, double Qa, double EXPla, double A, double k, double d);
double dnsM (double lambda, double y, double age, double eps, double step,
             double Pa, double EXPla, double A, double k, double d);
double prbM (double lambda, double y, double age, double eps, double step,
             double Pa, double Qa, double EXPla,
             double A, double k, double d);
double dnsBM (double lambda, double y, double age, double nu, double eps,
              double step, double Pa, double Qa, double EXPla,
              double A, double k, double d);
double prbBM (double lambda, double y, double age, double nu, double eps,
              double step, double Pa, double Qa, double EXPla,
              double A, double k, double d);
double IdnsB (double zmin, double zmax, double lambda, double y, double age,
              double nu, double step, double Pa, double Qa, double EXPla,
              double A, double k, double d);

/* Utility functions for calls from R */
void negloglik(double *res, double *lambda, double *y, double *a, int *nsubj,
               double *nu, double *eps, double *step, double *ylo, double *yhi,
               double *y1, double *alpha, double *d, int *nmc)
{
  *res = NLLFf(*lambda,y,a,*nsubj,*nu,*eps,*step,*ylo,*yhi,y1,alpha,d,*nmc);
}

void ydens_fund(double *res, double *lambda, double *y, double *a, double *Pa,
                double *EXPla, double *y1, double *alpha, double *d)
{
  *res = dnsF(*lambda,*y,*a,*Pa,*EXPla,*y1,*alpha,*d);
}

void yprob_fund(double *res, double *lambda, double *y, double *a, double *Pa,
                double *Qa, double *EXPla, double *y1, double *alpha, double *d)
{
  *res = prbF(*lambda,*y,*a,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}

void ydens_b_noise(double *res, double *lambda, double *y, double *a,
                   double *nu, double *Pa, double *Qa, double *EXPla,
                   double *y1, double *alpha, double *d)
{
  *res = dnsB(*lambda,*y,*a,*nu,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}

void yprob_b_noise(double *res, double *lambda, double *y, double *a,
                   double *nu, double *step, double *Pa, double *Qa,
                   double *EXPla, double *y1, double *alpha, double *d)
{
  *res = prbB(*lambda,*y,*a,*nu,*step,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}

void ydens_m_noise(double *res, double *lambda, double *y, double *a,
                   double *eps, double *step, double *Pa, double *EXPla,
                   double *y1, double *alpha, double *d)
{
  *res = dnsM(*lambda,*y,*a,*eps,*step,*Pa,*EXPla,*y1,*alpha,*d);
}

void yprob_m_noise(double *res, double *lambda, double *y, double *a,
                   double *eps, double *step, double *Pa, double *Qa,
                   double *EXPla, double *y1, double *alpha, double *d)
{
  *res = prbM(*lambda,*y,*a,*eps,*step,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}

void ydens_bm_noise(double *res, double *lambda, double *y, double *a,
                    double *nu, double *eps, double *step,
                    double *Pa, double *Qa, double *EXPla,
                    double *y1, double *alpha, double *d)
{
  *res = dnsBM(*lambda,*y,*a,*nu,*eps,*step,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}

void yprob_bm_noise(double *res, double *lambda, double *y, double *a,
                    double *nu, double *eps, double *step,
                    double *Pa, double *Qa, double *EXPla,
                    double *y1, double *alpha, double *d)
{
  *res = prbBM(*lambda,*y,*a,*nu,*eps,*step,*Pa,*Qa,*EXPla,*y1,*alpha,*d);
}
