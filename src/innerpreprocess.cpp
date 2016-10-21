#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool innerpreprocess(double pnr, 
                     std::string treatname,
                     Rcpp::NumericVector strnum,
                     Rcpp::NumericVector eksd,
                     Rcpp::NumericVector apk,
                     Rcpp::NumericVector packsize,
                     Rcpp::NumericVector dval
                       ) {

  bool baddata = 0; 
  int J = dval.size(); 
    
  for (int k1 = 0; k1 < eksd.size(); k1++) {
    if(eksd[k1] < 0 || apk[k1] < 0.0001 || packsize[k1] < 0.5) {
      Rcout << "Warning - non-valid prescription - treatment=" << treatname << "pnr=" << pnr << 
        ", eksd=" << eksd[k1] << ", packsize=" << packsize[k1] << std::endl;
      baddata = 1; 
    }
  }
  for (int k2 = 0; k2 < strnum.size(); k2++) {
    for (int j = 0; j < J; j++) {
      if (strnum[k2] == dval[j]) {
        j = J; 
      } else if (j == J-1) {
        Rcout << "Warning - not all doses are defined - treatment=" << treatname  <<  
          ", strnum=" << strnum[k2] << std::endl;
        baddata = 1;
      }
    }
  }  
  
  
 //' if (typ_dos[0] > typ_dos[1] || typ_dos[1] > typ_dos[2] || typ_dos[2] > typ_dos[3]) {
 //'  Rcout << "Advarsel - typiske doser ik1e ascenderende" << std::endl;
 //'  }
  
  return baddata; 
}

