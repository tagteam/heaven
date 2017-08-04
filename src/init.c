#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _heaven_daysnonhosp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _heaven_innerprocess(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _heaven_Matcher(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _heaven_split2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _heaven_timesTwo(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_heaven_daysnonhosp",  (DL_FUNC) &_heaven_daysnonhosp,  5},
    {"_heaven_innerprocess", (DL_FUNC) &_heaven_innerprocess, 7},
    {"_heaven_Matcher",      (DL_FUNC) &_heaven_Matcher,      9},
    {"_heaven_split2",       (DL_FUNC) &_heaven_split2,       5},
    {"_heaven_timesTwo",     (DL_FUNC) &_heaven_timesTwo,     1},
    {NULL, NULL, 0}
};

void R_init_heaven(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
