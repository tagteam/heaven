#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP heaven_daysnonhosp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP heaven_innerprocess(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP heaven_Matcher(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP heaven_split2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP heaven_timesTwo(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"heaven_daysnonhosp",  (DL_FUNC) &heaven_daysnonhosp,  5},
    {"heaven_innerprocess", (DL_FUNC) &heaven_innerprocess, 7},
    {"heaven_Matcher",      (DL_FUNC) &heaven_Matcher,      9},
    {"heaven_split2",       (DL_FUNC) &heaven_split2,       5},
    {"heaven_timesTwo",     (DL_FUNC) &heaven_timesTwo,     1},
    {NULL, NULL, 0}
};

void R_init_heaven(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
