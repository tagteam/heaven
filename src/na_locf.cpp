#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function to carry last observation
// forward for integer variables.  It is not exported.

// [[Rcpp::export]]
IntegerVector na_locf(IntegerVector x) {
  int *p=x.begin(), *end=x.end();
  int v=*p; p++;
  while(p<end){
    while(p<end && !IntegerVector::is_na(*p)) p++;
    v=*(p-1);
    while(p<end && IntegerVector::is_na(*p)){
      *p=v;
      p++;
    }
  }
  return x;
}


