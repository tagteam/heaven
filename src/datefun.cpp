#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::DateVector datefun(
    NumericVector sv
) {
  return DateVector(sv);
}

