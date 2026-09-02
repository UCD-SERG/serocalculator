/* Joint (shared latent infection time) negative log-likelihood for several
 * biomarkers measured on the same subjects.
 *
 * `serocalc.c` evaluates one biomarker at a time: for each subject it
 * integrates the density of that biomarker's reading over the subject's
 * (unobserved) time since seroconversion, and `log_likelihood()` then sums
 * the per-biomarker results, which treats the biomarkers as if each had its
 * own latent infection time (a composite / independence likelihood; see
 * issue #637). This file evaluates the model the methodology article
 * describes instead: one integral per subject over a shared latent time t,
 * with the per-biomarker conditional densities multiplied inside it
 * (issue #646).
 *
 * The seroresponse and noise models are the same as in `serocalc.c`:
 *
 * * Given a seroconversion t days before sampling and curve parameters
 *   (A = peak level y1, k = decay rate alpha, d = shape r - 1), the
 *   noise-free antibody level is y*(t) = A / (1 + d A^d k t)^(1/d).
 *   A subject who has never seroconverted has y* = 0.
 * * Biological noise (nu > 0) adds an independent Uniform(0, nu) term;
 *   measurement noise (eps > 0) multiplies by an independent
 *   Uniform(1 - eps, 1 + eps) factor. Readings at or below `y.low` are
 *   treated as left-censored and readings at or above `y.high` as
 *   right-censored, as in `NLLFf()`.
 * * A subject of age a has never seroconverted with probability
 *   Qa = exp(-lambda a); otherwise the time since the last seroconversion
 *   has (unnormalised) density Pa (lambda exp(-lambda t) + exp(-lambda a)/a)
 *   on (0, a), with Pa = 1 - Qa. This mirrors `dnsF()` exactly, so with a
 *   single biomarker the joint likelihood reduces to the marginal one.
 *
 * Uncertainty in the curve parameters is handled as in `NLLFf()`, by
 * averaging the per-subject likelihood over posterior draws; the draws are
 * paired across biomarkers (draw s of biomarker 1 goes with draw s of
 * biomarker 2), so that a subject's biomarkers are evaluated under one
 * coherent parameter set. Pairing is the caller's responsibility
 * (`.align_joint_curve_params()` on the R side).
 *
 * Because every noise model has bounded support, the set of latent times
 * compatible with a reading is an interval, and the set compatible with all
 * of a subject's readings is the intersection of those intervals. The
 * integral over t is therefore taken only over that intersection, by the
 * midpoint rule with `nt` nodes; the integrand is smooth there, so a modest
 * `nt` is accurate, and an empty intersection costs nothing.
 */

#include <R.h>
#include <Rmath.h>
#include <float.h>

/* How a reading enters the likelihood. */
#define JOINT_UNCENSORED 0
#define JOINT_LEFT_CENSORED 1
#define JOINT_RIGHT_CENSORED 2

static double clamp01(double x) {
  if (x < 0) return 0;
  if (x > 1) return 1;
  return x;
}

/* Noise-free antibody level t days after seroconversion. */
static double ystar_at(double t, double A, double k, double d) {
  return A / pow(1 + d * pow(A, d) * k * t, 1 / d);
}

/* Time since seroconversion at which the noise-free level equals y, capped
 * to [0, age]: levels at or above the peak map to t = 0, and levels at or
 * below the level reached at age `age` map to t = age. Inverts ystar_at(). */
static double t_at_ystar(double y, double age, double A, double k, double d) {
  double ya = ystar_at(age, A, k, d);
  if (y >= A) return 0;
  if (y <= ya) return age;
  return (pow(y, -d) - pow(A, -d)) / (k * d);
}

/* P(Y <= y | y*) under the noise model. */
static double cond_prob(double y, double ystar, double nu, double eps) {
  if (nu <= 0 && eps <= 0) {                 /* no noise: Y = y* */
    return (y >= ystar) ? 1 : 0;
  }
  if (eps <= 0) {                            /* biological noise only */
    return clamp01((y - ystar) / nu);
  }
  if (nu <= 0) {                             /* measurement noise only */
    if (ystar <= 0) return (y >= 0) ? 1 : 0;
    return clamp01((y / ystar - (1 - eps)) / (2 * eps));
  }
  /* both: Y = (y* + B) M with B ~ U(0, nu), M ~ U(1 - eps, 1 + eps).
   * P(Y <= y | B = b) = clamp((y / (y* + b) - (1 - eps)) / (2 eps)), which
   * equals 1 for b <= b1 and 0 for b >= b2; integrate over b in closed form. */
  {
    double b1 = y / (1 + eps) - ystar;
    double b2 = y / (1 - eps) - ystar;
    double prb;
    if (b1 < 0) b1 = 0;
    if (b1 > nu) b1 = nu;
    if (b2 < 0) b2 = 0;
    if (b2 > nu) b2 = nu;
    prb = b1;
    if (b2 > b1 && ystar + b1 > 0) {
      prb += (y * log((ystar + b2) / (ystar + b1)) - (1 - eps) * (b2 - b1)) /
             (2 * eps);
    }
    return prb / nu;
  }
}

/* Density of Y at y given y*, under the noise model. Undefined (a point
 * mass) without noise; the R side refuses that case for the joint model. */
static double cond_dens(double y, double ystar, double nu, double eps) {
  if (nu <= 0 && eps <= 0) return 0;
  if (eps <= 0) {                            /* biological noise only */
    return (ystar < y && y < ystar + nu) ? 1 / nu : 0;
  }
  if (nu <= 0) {                             /* measurement noise only */
    if (ystar <= 0) return 0;
    return (ystar * (1 - eps) < y && y < ystar * (1 + eps))
      ? 1 / (2 * eps * ystar) : 0;
  }
  {                                          /* both */
    double zmin = ystar > y / (1 + eps) ? ystar : y / (1 + eps);
    double zmax = ystar + nu < y / (1 - eps) ? ystar + nu : y / (1 - eps);
    if (zmin < zmax && zmin > 0) return log(zmax / zmin) / (2 * eps * nu);
    return 0;
  }
}

/* One biomarker's factor in the integrand, given the noise-free level. */
static double cond_term(int type, double y, double ystar, double nu,
                        double eps, double ylo, double yhi) {
  switch (type) {
  case JOINT_LEFT_CENSORED:
    return cond_prob(ylo, ystar, nu, eps);
  case JOINT_RIGHT_CENSORED:
    return 1 - cond_prob(yhi, ystar, nu, eps);
  default:
    return cond_dens(y, ystar, nu, eps);
  }
}

/* Range of noise-free levels y* for which cond_term() can be non-zero.
 * `lo` may be negative (meaning "no lower bound"); `hi` may be DBL_MAX. */
static void ystar_support(int type, double y, double nu, double eps,
                          double ylo, double yhi, double *lo, double *hi) {
  switch (type) {
  case JOINT_LEFT_CENSORED:
    *lo = -1;
    *hi = (eps > 0) ? ylo / (1 - eps) : ylo;
    break;
  case JOINT_RIGHT_CENSORED:
    *lo = ((eps > 0) ? yhi / (1 + eps) : yhi) - nu;
    *hi = DBL_MAX;
    break;
  default:
    *lo = ((eps > 0) ? y / (1 + eps) : y) - nu;
    *hi = (eps > 0) ? y / (1 - eps) : y;
    break;
  }
}

/* Negative log-likelihood, summed over subjects.
 *
 * yy, obs: nsubj x nbio, column-major; obs[i, b] = 1 if biomarker b was
 *          measured on subject i (missing readings contribute a factor of 1).
 * aa:      ages (years) of the nsubj subjects.
 * nu, eps, yLo, yHi: per-biomarker noise parameters and detection limits.
 * A, k, d: nmc x nbio, column-major; paired posterior draws of the curve
 *          parameters (peak, decay rate in 1/years, shape - 1).
 * nt:      number of midpoint nodes for the integral over t.
 * ndrop:   on return, the number of subjects whose readings had zero
 *          likelihood under every draw (no latent time is compatible with
 *          all of them); like `NLLFf()`, these contribute nothing.
 */
static double NLLFjoint(double lambda, double *yy, int *obs, double *aa,
                        int nsubj, int nbio, double *nu, double *eps,
                        double *yLo, double *yHi, double *A, double *k,
                        double *d, int nmc, int nt, int *ndrop) {
  double llf = 0;
  int *type = (int *) R_alloc(nbio, sizeof(int));
  int subj, b, s, j;

  *ndrop = 0;

  for (subj = 0; subj < nsubj; subj++) {
    double age = aa[subj];
    double Qa = exp(-lambda * age);
    double Pa = 1 - Qa;
    double EXPla = Qa / age;
    double never, integ, rho;

    for (b = 0; b < nbio; b++) {
      double y = yy[subj + nsubj * b];
      if (y <= yLo[b]) type[b] = JOINT_LEFT_CENSORED;
      else if (y >= yHi[b]) type[b] = JOINT_RIGHT_CENSORED;
      else type[b] = JOINT_UNCENSORED;
    }

    /* never seroconverted: y* = 0 for every biomarker, no curve parameters */
    never = Qa;
    for (b = 0; b < nbio; b++) {
      if (!obs[subj + nsubj * b]) continue;
      never *= cond_term(type[b], yy[subj + nsubj * b], 0, nu[b], eps[b],
                         yLo[b], yHi[b]);
      if (never <= 0) break;
    }

    /* seroconverted at some t in (0, age): average over paired draws */
    integ = 0;
    for (s = 0; s < nmc; s++) {
      double tlo = 0, thi = age, dt, sum;

      /* intersect the latent-time intervals compatible with each reading */
      for (b = 0; b < nbio; b++) {
        double lo, hi, t1, t2;
        if (!obs[subj + nsubj * b]) continue;
        ystar_support(type[b], yy[subj + nsubj * b], nu[b], eps[b], yLo[b],
                      yHi[b], &lo, &hi);
        /* y* decreases in t, so the upper level bound gives the lower time */
        t1 = t_at_ystar(hi, age, A[s + nmc * b], k[s + nmc * b], d[s + nmc * b]);
        t2 = (lo <= 0) ? age
          : t_at_ystar(lo, age, A[s + nmc * b], k[s + nmc * b], d[s + nmc * b]);
        if (t1 > tlo) tlo = t1;
        if (t2 < thi) thi = t2;
        if (tlo >= thi) break;
      }
      if (tlo >= thi) continue;

      dt = (thi - tlo) / nt;
      sum = 0;
      for (j = 0; j < nt; j++) {
        double t = tlo + (j + 0.5) * dt;
        double prod = Pa * (lambda * exp(-lambda * t) + EXPla);
        for (b = 0; b < nbio; b++) {
          double ystar;
          if (!obs[subj + nsubj * b]) continue;
          ystar = ystar_at(t, A[s + nmc * b], k[s + nmc * b], d[s + nmc * b]);
          prod *= cond_term(type[b], yy[subj + nsubj * b], ystar, nu[b],
                            eps[b], yLo[b], yHi[b]);
          if (prod <= 0) break;
        }
        sum += prod;
      }
      integ += sum * dt;
    }

    rho = never + integ / nmc;
    if (rho > 0) llf -= log(rho);
    else (*ndrop)++;
  }
  return llf;
}

/* .C() entry point */
void negloglik_joint(double *res, double *lambda, double *y, int *obs,
                     double *a, int *nsubj, int *nbio, double *nu,
                     double *eps, double *ylo, double *yhi, double *y1,
                     double *alpha, double *d, int *nmc, int *nt,
                     int *ndrop) {
  *res = NLLFjoint(*lambda, y, obs, a, *nsubj, *nbio, nu, eps, ylo, yhi, y1,
                   alpha, d, *nmc, *nt, ndrop);
}
