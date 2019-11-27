// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::export]]
Rcpp::NumericVector countDrugs(Rcpp::DataFrame mix,
		      Rcpp::DataFrame db){
  arma::vec id =Rcpp::as<arma::vec>(mix["pnr"]);
  arma::vec casedate =Rcpp::as<arma::vec>(mix["case.date"]);
  arma::vec dbid =Rcpp::as<arma::vec>(db["pnr"]);
  arma::vec eksd =Rcpp::as<arma::vec>(db["eksd"]);
  uword NOBS=id.size();
  uword NP=dbid.size();
  arma::vec purchases(NOBS,fill::zeros);
  for (uword i = 0; i< NOBS; i++){
    for (uword j = 0; j<NP;j++){
      if((dbid(j)==id(i)) & (eksd(j)<casedate(i)) & (eksd(j)>(casedate(i)-10*365.25)))
	purchases(i)++;
    }
    // purchases(i)=arma::sum(dbid==id(i) & eksd<casedate(i) & eksd>(casedate(i)-10*365.25));
  }
  return(wrap(purchases));
}

