#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
DataFrame nccSamplingCpp(arma::vec pnr, arma::vec time, arma::vec status, double Ncontrols=10){
  arma::vec caseTimes = unique(time.elem(find(status==1)));       // Actual event times
  int nCases = sum(status==1);                                    // Number of cases
  // Initialize
  arma::vec PNR = zeros<vec>((Ncontrols+1)*nCases);               
  arma::vec TIME = zeros<vec>((Ncontrols+1)*nCases);
  arma::vec STATUS = zeros<vec>((Ncontrols+1)*nCases);
  uword NOBS= caseTimes.size();
  int nn=0;
  double CT=0;
  for(uword t = 0;t< NOBS;t++){                                        // Loop over event times
    CT=caseTimes[t];
    arma::uvec Case = (time == CT) % (status == 1);                // Case at time t (1/0)
    int ncase = sum(Case);                                        // Number of cases at time t
    arma::uvec atRiskIndex = (time>=CT)%(status==0);               // At risk set                            
    arma::vec atRisk = pnr(find((time>=CT)%(status==0)));          // PNRs for people at risk
    uword ncont = Ncontrols * ncase;                                // Number of controls
    // Sample risk set if risk set is smaller than Ncontrols
    if(ncont>sum(atRiskIndex)) ncont = sum(atRiskIndex);          
    if(ncont>0){                                                  // Do stuff if risk set is not empty
      int newnn = nn+ncase+ncont;                                 // New index to loop over
      for(int i=nn;i<newnn;++i){
	TIME(i)=t;                                                // The time for the cluster
      }
      // PNR for cases
      arma::vec pnrHelp = pnr.elem(find((time==CT) % (status==1)));
      for(int i=nn;i<nn+ncase;++i){
	STATUS(i) = 1;                                            // Status for cases (1)
	int j=i-nn;
	PNR(i)=pnrHelp(j);                                        // PNRs for cases
      }
      IntegerVector indices = seq(0,ncont-1);                     // Numbers to sample
      IntegerVector ind = Rcpp::sample(indices,ncont);            // Sample
      for(int i=nn+ncase;i<newnn;++i){
	int j = i-nn-ncase;                      
	// Choose controls as the sampled among those at risk
	PNR(i) = atRisk(ind(j));                                  
      }
      nn = newnn;                                                 // Set index for next round in the loop
    }
  }
  // Return data.frame with pnr, failure times and status indicators
  return DataFrame::create(Named("pnr")=PNR,Named("time")=TIME,Named("status")=STATUS);
}
