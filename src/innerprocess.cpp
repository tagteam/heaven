#include <RcppArmadillo.h>
#include <innerpreprocess.cpp>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector innerprocess(Rcpp::DataFrame indata,
                             Rcpp::List doses, 
                             std::string treatname
                               ) {
  
  Rcpp::NumericVector dval = doses["value"];
  Rcpp::NumericVector dmin = doses["min"];
  Rcpp::NumericVector dmax = doses["max"];
  Rcpp::NumericVector ddef = doses["def"];
  
  Rcpp::NumericVector id       = indata["id"];
  Rcpp::NumericVector pdate    = indata["pdate"];
  Rcpp::NumericVector strength = indata["strength"];
  Rcpp::NumericVector apk      = indata["apk"];
  Rcpp::NumericVector packsize = indata["packsize"];

  Rcpp::NumericVector T = wrap(unique(as<arma::vec>(eksd)));
  
  double K = T.size();
  double J = dval.size(); 
  
  Rcpp::NumericVector c(K);
  Rcpp::NumericVector D(K);
  Rcpp::NumericVector S(K);
  Rcpp::NumericMatrix n(K, J); 
  
  Rcpp::NumericVector strengthunique = wrap(unique(as<arma::vec>(strength)));

  bool baddata = innerpreprocess(id[0], treatname, strengthunique, pdate, apk, packsize, dval);
  
  DataFrame outdata;
  
  if (baddata) {
    
    Rcout << std::endl << "Warning - non-valid data input for the treatment named " <<
      treatname << std::endl;
    Rcout << "The computations for this treatment will not be performed" << std::endl << std::endl;
    
    outdata = Rcpp::DataFrame::create(Rcpp::Named("id") = id);
    
  } else {

    //'--- for each date collect prescriptions to one
    
    for (int k = 0; k < K; k++) {
      
      NumericVector evec = Rcpp::as<Rcpp::NumericVector>(wrap(arma::find(as<arma::vec>(pdate) == T[k])));
      
      int nevec = evec.size();
      
      Rcpp::NumericVector strength1 = strength[evec];
      Rcpp::NumericVector apk1      = apk[evec];
      Rcpp::NumericVector packsize1 = packsize[evec];
      
      for (int j = 0; j < J; j++) {
        for (int e = 0; e < nevec; e++) {
          if (strength1[e] == dval[j]) {
            n(k, j) += apk1[e] * packsize1[e] * strength1[e];
          }
        }
        D[k] += n(k, j);
        n(k, j) = n(k, j) / (double) dmin[j];
        c[k] += (n(k, j) > 0);
        S[k] += (n(k, j) > 0) * dval[j];
      }
      
      S[k] = S[k] / (double) c[k];
    }
    
    outdata =  Rcpp::DataFrame::create(Rcpp::Named("id")      = id,
                                       Rcpp::Named("pdate")     = T
    );
  }
  
  return(D);
}

  
/*** R
dpp1 <- preprocess(d)

test = innerprocess(dpp1$first1,
                    d$drugs$first1$doses, 
                    "first1")
test

dpp2 <- lapply(1:length(dpp1), function(p) {
  
  pnrunique <- unique(dpp1[[p]]$pnr)
  
  doses     <- d$drugs[[p]]$doses
  
  if (dim(dat)[1] > 0 & length(doses) > 0) 
    do.call("c", lapply(1:length(pnrunique), function(i) {
      dat       <- dpp1[[p]][dpp1[[p]]$pnr == pnrunique[i], ]
      innerprocess(dat, doses, names(dpp1)[p]) 
    }))
      
})  
dpp2

*/
    
  
  
   

  
