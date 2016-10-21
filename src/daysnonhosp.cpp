#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]] 
Rcpp::NumericVector daysnonhosp(Rcpp::NumericVector pnr,
                                Rcpp::NumericVector eksd,
                                Rcpp::NumericVector pnrdates,
                                Rcpp::NumericVector inddto, 
                                Rcpp::NumericVector uddto
                                  ) {

  int n = pnr.size(); 
    
  double D;
  Rcpp::NumericVector H(n);
  
  for (int j = 0; j < (n-1); ++j) {
    
    D = 0; 
    
    NumericVector yp = Rcpp::as<Rcpp::NumericVector>(wrap(arma::find(as<arma::vec>(pnrdates) == pnr[j])));

    for (int q = 0; q < yp.size(); ++q) { 
      
      double yk = yp[q];
      
      if (eksd[j] == eksd[j] && eksd[j+1] == eksd[j+1] &&  
          eksd[j] <= inddto[yk] && eksd[j+1] >= inddto[yk] && pnr[j+1] == pnr[j])
        D += max(NumericVector::create(0, min(NumericVector::create(uddto[yk], eksd[j+1])) - 
          max(NumericVector::create(inddto[yk], eksd[j]))));  
    }
    
    H[j] = max(NumericVector::create(1, eksd[j+1] - eksd[j] - D));
  }

  return H; 
}
  
