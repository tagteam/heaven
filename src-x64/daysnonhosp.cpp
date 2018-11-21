#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]] 
Rcpp::NumericVector daysnonhosp(Rcpp::NumericVector id,
                                Rcpp::NumericVector pdate,
                                Rcpp::NumericVector iddates,
                                Rcpp::NumericVector inddto, 
                                Rcpp::NumericVector uddto
                                  ) {

  int n = id.size(); 
    
  double D;
  Rcpp::NumericVector H(n);
  
  for (int j = 0; j < (n-1); ++j) {
    
    D = 0; 
    
    NumericVector yp = Rcpp::as<Rcpp::NumericVector>(wrap(arma::find(as<arma::vec>(iddates) == id[j])));

    for (int q = 0; q < yp.size(); ++q) { 
      
      double yk = yp[q];
      
      if (pdate[j] == pdate[j] && pdate[j+1] == pdate[j+1] &&  
          pdate[j] <= inddto[yk] && pdate[j+1] >= inddto[yk] && id[j+1] == id[j])
        D += max(NumericVector::create(0, min(NumericVector::create(uddto[yk], pdate[j+1])) - 
          max(NumericVector::create(inddto[yk], pdate[j]))));  
    }
    
    H[j] = max(NumericVector::create(1, pdate[j+1] - pdate[j] - D));
  }

  return H; 
}
  
