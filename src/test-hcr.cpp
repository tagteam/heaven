#include <RcppArmadillo.h>
using namespace Rcpp;

//' @title Helene's first cpp function
//' @param dt data.frame or data.table which provides variables: pnr, recno, inddto, uddto, pattype
//' @return data.frame where all intermediate admission records are removed 
//' @examples
//' @export 
//' @author Helene Charlotte Rytgaard

// [[Rcpp::export]]
int timesTwo(int x) {
  return x * 2;
}


