#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
DataFrame nccSamplingCpp(arma::vec pnr, arma::vec time, arma::vec status, double Ncontrols=10){
  arma::vec caseTimes = unique(time.elem(find(status==1)));       // Actual event times
  int n = caseTimes.size();
  int nCases = sum(status==1);                                    // Number of cases
  // Initialize
  arma::vec PNR = zeros<vec>((Ncontrols+1)*nCases);               
  arma::vec TIME = zeros<vec>((Ncontrols+1)*nCases);
  arma::vec STATUS = zeros<vec>((Ncontrols+1)*nCases);
  uword NOBS= caseTimes.size();
  int nn=0;
  // seq function from base
  Rcpp::Environment base("package:base"); 
  Rcpp::Function seq = base["seq"];
  // Loop over event times
  for(int i=0;i<n;++i){                                        
    double t = caseTimes(i);
    arma::uvec Case = (time == t) % (status == 1);                // Case at time t (1/0)
    int ncase = sum(Case);                                        // Number of cases at time t
    arma::uvec atRiskIndex = (time>=t)%(status==0);               // At risk set                            
    arma::vec atRisk = pnr.elem(find((time>=t)%(status==0)));     // PNRs for people at risk
    int ncont = Ncontrols * ncase;                                // Number of controls
    // Sample risk set if risk set is smaller than Ncontrols
    if(ncont>sum(atRiskIndex)) ncont = sum(atRiskIndex);          
    if(ncont>0){                                                  // Do stuff if risk set is not empty
      int newnn = nn+ncase+ncont;                                 // New index to loop over
      arma::uvec TIMEpos = as<uvec>(seq(nn,newnn-1));             // Interval to define values for time
      TIME.elem(TIMEpos).fill(t);                                 // Give value of t in interval
      arma::uvec pos = as<uvec>(seq(nn,nn+ncase-1));              // Interval for status and pnr
      STATUS.elem(pos).fill(1);                                   // Cases
      PNR.elem(pos) = pnr.elem(find((time==t) % (status==1)));    // PNR for cases
      IntegerVector indices = seq(0,sum(atRiskIndex)-1);          // Numbers to sample
      arma::uvec ind = as<uvec>(Rcpp::sample(indices,ncont));     // Sample
      arma::uvec pos2 = as<uvec>(seq(nn+ncase,newnn-1));          // Interval for status and pnr
      PNR.elem(pos2) = atRisk.elem(ind);                          // Controls equal sampled ones
      nn = newnn;                                                 // Set index for next round in the loop
    }
  }
  // Return data.frame with pnr, failure times and status indicators
  return DataFrame::create(Named("pnr")=PNR,Named("time")=TIME,Named("status")=STATUS);
}
