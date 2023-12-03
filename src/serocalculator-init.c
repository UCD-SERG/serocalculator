#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void abr(void *, void *, void *, void *);
extern void adstattwor(void *, void *, void *, void *, void *);
extern void agr(void *, void *, void *, void *);
extern void ar(void *, void *, void *);
extern void br(void *, void *);
extern void drndadar(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndadarenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndadr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndadrenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndkpar(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndkparenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndkpr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndkprenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksapr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksaprenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksar(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksarenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndkspr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksprenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void drndksrenewr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void kpdevtwor(void *, void *, void *, void *, void *);
extern void ksdevtwor(void *, void *, void *, void *, void *);
extern void ksprobtwor(void *, void *, void *, void *, void *);
extern void negloglik(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void rndindr(void *, void *);
extern void rndlnormr(void *, void *);
extern void rndunifr(void *, void *);
extern void srespintervr(void *, void *, void *, void *, void *, void *, void *, void *);
extern void srespmemryr(void *, void *, void *, void *, void *, void *);
extern void sresprenewr(void *, void *, void *, void *, void *, void *);
extern void sympr(void *, void *, void *);
extern void t1r(void *, void *, void *);
extern void tintervr(void *, void *);
extern void transfr(void *, void *, void *, void *);
extern void y1r(void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"abr",            (DL_FUNC) &abr,             4},
    {"adstattwor",     (DL_FUNC) &adstattwor,      5},
    {"agr",            (DL_FUNC) &agr,             4},
    {"ar",             (DL_FUNC) &ar,              3},
    {"br",             (DL_FUNC) &br,              2},
    {"drndadar",       (DL_FUNC) &drndadar,       15},
    {"drndadarenewr",  (DL_FUNC) &drndadarenewr,  15},
    {"drndadr",        (DL_FUNC) &drndadr,        15},
    {"drndadrenewr",   (DL_FUNC) &drndadrenewr,   15},
    {"drndkpar",       (DL_FUNC) &drndkpar,       15},
    {"drndkparenewr",  (DL_FUNC) &drndkparenewr,  15},
    {"drndkpr",        (DL_FUNC) &drndkpr,        15},
    {"drndkprenewr",   (DL_FUNC) &drndkprenewr,   15},
    {"drndksapr",      (DL_FUNC) &drndksapr,      15},
    {"drndksaprenewr", (DL_FUNC) &drndksaprenewr, 15},
    {"drndksar",       (DL_FUNC) &drndksar,       15},
    {"drndksarenewr",  (DL_FUNC) &drndksarenewr,  15},
    {"drndkspr",       (DL_FUNC) &drndkspr,       15},
    {"drndksprenewr",  (DL_FUNC) &drndksprenewr,  15},
    {"drndksr",        (DL_FUNC) &drndksr,        15},
    {"drndksrenewr",   (DL_FUNC) &drndksrenewr,   15},
    {"kpdevtwor",      (DL_FUNC) &kpdevtwor,       5},
    {"ksdevtwor",      (DL_FUNC) &ksdevtwor,       5},
    {"ksprobtwor",     (DL_FUNC) &ksprobtwor,      5},
    {"negloglik",      (DL_FUNC) &negloglik,      14},
    {"rndindr",        (DL_FUNC) &rndindr,         2},
    {"rndlnormr",      (DL_FUNC) &rndlnormr,       2},
    {"rndunifr",       (DL_FUNC) &rndunifr,        2},
    {"srespintervr",   (DL_FUNC) &srespintervr,    8},
    {"srespmemryr",    (DL_FUNC) &srespmemryr,     6},
    {"sresprenewr",    (DL_FUNC) &sresprenewr,     6},
    {"sympr",          (DL_FUNC) &sympr,           3},
    {"t1r",            (DL_FUNC) &t1r,             3},
    {"tintervr",       (DL_FUNC) &tintervr,        2},
    {"transfr",        (DL_FUNC) &transfr,         4},
    {"y1r",            (DL_FUNC) &y1r,             3},
    {NULL, NULL, 0}
};

void R_init_serocalculator(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
