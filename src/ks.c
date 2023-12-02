/* Modified  from Numerical Recipes in C */
/* Consider license. Rewrite if needed   */
#include <math.h>
#define SWAP(a,b) temp=(a);(a)=(b);(b)=temp;
#define NR_END 1
#define FREE_ARG char*
#define M 7
#define NSTACK 50
#define EPS1 1.0e-3
#define EPS2 1.0e-8

/* Given an array data1[1..n1], and an array data2[1..n2] this function   */
/* returns the K-S statistic d. The arrays data1 and data2 must be sorted */
/* into ascending order.                                                  */
double ksdevtwo(double *data1, int n1, double *data2, int n2)
{
  int j1=1, j2=1;
  double d, d1, d2, dt, en1, en2, fn1=0.0, fn2=0.0;
  en1 = (double) n1;
  en2 = (double) n2;
  d = 0.0;
  while(j1<=n1 && j2<=n2){ /* Wile we are not done */
    if((d1=data1[j1-1]) <= (d2=data2[j2-1])) fn1=j1++/en1; /* step in data1 */
    if(d2<=d1) fn2=j2++/en2;                               /* step in data2 */
    if((dt=fabs(fn2-fn1)) > d) d = dt;
  }
  return d;
}

/* Given an array data1[1..n1], and an array data2[1..n2] this function   */
/* returns the Kuiper statistic d. The arrays data1 and data2 must be     */
/* sorted into ascending order.                                           */
double kpdevtwo(double *data1, int n1, double *data2, int n2)
{
  int j1=1, j2=1;
  double dplus, dmin, d1, d2, dt, en1, en2, fn1=0.0, fn2=0.0;
  en1 = (double) n1;
  en2 = (double) n2;
  dplus = 0.0;
  dmin = 0.0;
  while(j1<=n1 && j2<=n2){ /* Wile we are not done */
    if((d1=data1[j1-1]) <= (d2=data2[j2-1])) fn1=j1++/en1; /* step in data1 */
    if(d2<=d1) fn2=j2++/en2;                               /* step in data2 */
    if((dt=(fn2-fn1)) > dplus) dplus = dt;
    if((dt=(fn1-fn2)) > dmin) dmin = dt;
  }
  return dplus + dmin;
}

/* Given an array data1[1..n1], and an array data2[1..n2] this function   */
/* returns the K-S significance level prob for the null hypothesis that   */
/* the data sets are drawn from the same distribution. Small values of    */
/* prob show that the cumulative distribution function of data1 is        */
/* significantly different from that of data2.                            */
double ksprobtwo(double *data1, int n1, double *data2, int n2)
{
  int j1=1, j2=1;
  double prob, d, d1, d2, dt, en1, en2, en, fn1=0.0, fn2=0.0;
  en1 = (double) n1;
  en2 = (double) n2;
  d = 0.0;
  while(j1<=n1 && j2<=n2){ /* Wile we are not done */
    if((d1=data1[j1-1]) <= (d2=data2[j2-1])) fn1=j1++/en1; /* step in data1 */
    if(d2<=d1) fn2=j2++/en2;                               /* step in data2 */
    if((dt=fabs(fn2-fn1)) > d) d = dt;
  }
  en = sqrt(en1*en2/(en1+en2));
  prob = probks((en+0.12+0.11/en)*d); /* Compute significance. */
  return prob;
}

/* Kolmogorov-Smirnov probability function                               */
double probks(double alam)
{
  int j;
  double a2, fac=2.0, sum=0.0, term, termbf=0.0;
  a2 = -2.0*alam*alam;
  for(j=1; j<100; j++){
    term = fac*exp(a2*j*j);
    sum += term;
    if(fabs(term) <= EPS1*termbf || fabs(term) <= EPS2*sum) return sum;
    fac = -fac; /* Alternating signs in sum. */
    termbf = fabs(term);
  }
  return 1.0; /* Get here only by failing to converge. */
}

/* Sorts an array arr[0..n-1] into ascending numerical order using */
/* the Quicksort algorithm. n input. arr is replaced on output     */
/* by its sorted rearrangement.                                    */
/* Here M is the size of subarrays sorted by straight insertion    */
/* NSTACK is the required auxiliary storage                        */
void sort(double *arr, int n)
{
  int i, ir=n, j, k, l=1;
  int jstack=0, *istack;
  double a, temp;
  istack = ivector(1,NSTACK);
  for(;;){            /* Insertion sort when subarray small enough */
    if(ir-l<M){
      for(j=l+1;j<=ir;j++){
	a=arr[j-1];
	for(i=j-1;i>=1;i--){
	  if(arr[i-1]<=a) break;
	  arr[i]=arr[i-1];
	}
	arr[i]=a;
      }
      if(jstack==0) break;
      ir=istack[jstack--];        /* Pop stack and begin a new round      */
      l=istack[jstack--];         /* of partitioning                      */
    }else{
      k=(l+ir)>>1;                /* Choose median of left, center, and   */
      SWAP(arr[k-1],arr[l]);      /* right elements as partitioning       */
      if(arr[l]>arr[ir-1]){       /* element a. Also rearrange so that    */
	SWAP(arr[l],arr[ir-1]);   /* a[l+1] <= a[l] <= a[ir].             */
      }
      if(arr[l-1]>arr[ir-1]){
	SWAP(arr[l-1],arr[ir-1]);
      }
      if(arr[l]>arr[l-1]){
	SWAP(arr[l],arr[l-1]);
      }
      i=l+1;                      /* Initialize pointers for partitioning */
      j=ir;
      a=arr[l-1];                 /* Partitioning element                 */
      for(;;){                    /* Beginning of innermost loop          */
	do i++; while(arr[i-1]<a);/* Scan up to find element > a          */
	do j--; while(arr[j-1]>a);/* Scan down to find element < a        */
	if(j<i) break;            /* Pointers crossed. Partitioning done  */
	SWAP(arr[i-1],arr[j-1]);  /* Exchange elements                    */
      }                           /* End of innermost loop                */
      arr[l-1]=arr[j-1];          /* Insert partitioning element          */
      arr[j-1]=a;
      jstack+=2;
      if(jstack>NSTACK) prerr("NSTACK too small in sort.");
      if(ir-i+1>=j-l){
	istack[jstack]=ir;
	istack[jstack-1]=i;
	ir=j-1;
      }else{
	istack[jstack]=j-1;
	istack[jstack-1]=l;
	l=i;
      }
    }
  }
  free_ivector(istack,1,NSTACK);
}

/* allocate an int vector with subscript range v[]nl..nh] */
int *ivector(long nl, long nh)
{
  int *v;
  v=(int *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
  if(!v) prerr("Allocation error in ivector()");
  return v-nl+NR_END;
}
/* Free an int vector, allocated with ivector() */
void free_ivector(int *v, long nl, long nh)
{
  free((FREE_ARG) (v+nl-NR_END));
}
/* allocate an double vector with subscript range v[]nl..nh] */
double *dvector(long nl, long nh)
{
  double *v;
  v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
  if(!v) prerr("Allocation error in dvector()");
  return v-nl+NR_END;
}
/* Free an int vector, allocated with dvector() */
void free_dvector(double *v, long nl, long nh)
{
  free((FREE_ARG) (v+nl-NR_END));
}
/* Error handler */
void prerr(char error_text[])
{
  fprintf(stderr,"Run-time error\n");
  fprintf(stderr,"%s\n",error_text);
  fprintf(stderr,"Exiting\n");
  exit(1);
}
