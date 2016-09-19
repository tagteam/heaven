// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp; 

// test-hcr.cpp --- 
//----------------------------------------------------------------------
// author: Helene Charlotte Rytgaard
// created: Sep 19 2016 
// Version: 
// last-updated: Sep 19 2016 
//           By: Helene Charlotte Rytgaard
//     Update #: 1
//----------------------------------------------------------------------
// 
// Commentary: 
// 
// Change Log:
//----------------------------------------------------------------------
// 
// Code:
//code from hell----
//' R-version of the sas code from hell.
//'
//' @title Helene test cpp 
//' @param 
//' @return 
//' @examples
//' @export 
//' @author Helene Charlotte Rytgaard

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

