#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool innerpreprocess(double id, 
                     std::string treatname,
                     Rcpp::NumericVector strength,
                     Rcpp::NumericVector pdate,
                     Rcpp::NumericVector apk,
                     Rcpp::NumericVector packsize,
                     Rcpp::NumericVector dval
                       ) {

  bool baddata = 0; 
  int J = dval.size(); 
    
  for (int k1 = 0; k1 < pdate.size(); k1++) {
    if(pdate[k1] < 0 || apk[k1] < 0.0001 || packsize[k1] < 0.5) {
      Rcout << "Warning - non-valid prescription - treatment=" << treatname << "id=" << id << 
        ", pdate=" << pdate[k1] << ", packsize=" << packsize[k1] << std::endl;
      baddata = 1; 
    }
  }
  for (int k2 = 0; k2 < strength.size(); k2++) {
    for (int j = 0; j < J; j++) {
      if (strength[k2] == dval[j]) {
        j = J; 
      } else if (j == J-1) {
        Rcout << "Warning - not all doses are defined - treatment=" << treatname  <<  
          ", strength=" << strength[k2] << std::endl;
        baddata = 1;
      }
    }
  }  
  
  
 //' if (typ_dos[0] > typ_dos[1] || typ_dos[1] > typ_dos[2] || typ_dos[2] > typ_dos[3]) {
 //'  Rcout << "Advarsel - typiske doser ik1e ascenderende" << std::endl;
 //'  }
  
  return baddata; 
}

