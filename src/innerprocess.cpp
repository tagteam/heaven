// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//' @title Inner process function.
//' @author Helene Charlotte Rytgaard
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame innerprocess(Rcpp::DataFrame dat,
                                 Rcpp::DataFrame admdat,
                                 Rcpp::List doses, 
                                 std::string treatname,
                                 double N, double maxdepot
                                   ) {
  
  Rcpp::NumericVector dval = doses["value"];
  Rcpp::NumericVector dmin = doses["min"];
  Rcpp::NumericVector dmax = doses["max"];
  Rcpp::NumericVector ddef = doses["def"];
  
  Rcpp::NumericVector id       = dat["id"];
  Rcpp::NumericVector pdate    = dat["pdate"];
  Rcpp::NumericVector strength = dat["strength"];
  Rcpp::NumericVector npack    = dat["npack"];
  Rcpp::NumericVector ppp      = dat["ppp"];
  
  Rcpp::NumericVector admin  = admdat["inddto"];
  Rcpp::NumericVector admout = admdat["uddto"];

  Rcpp::NumericVector T = wrap(unique(as<arma::vec>(pdate)));

  double K = T.size();
  double J = dval.size(); 
  
  Rcpp::NumericVector c(K);
  Rcpp::NumericVector D(K);
  Rcpp::NumericVector S(K);
  Rcpp::NumericMatrix n(K, J); 
  Rcpp::NumericVector H(K);
  Rcpp::NumericVector u(K);
  Rcpp::NumericVector jk(K);
  Rcpp::NumericVector w(K);
  Rcpp::NumericVector M(K);
  Rcpp::NumericVector i0(K);
  
  
  Rcpp::NumericVector idout(K);
  Rcpp::DateVector B(K);
  Rcpp::DateVector E(K);
  Rcpp::NumericVector Enum(K);
  Rcpp::NumericVector R(K);
  Rcpp::NumericVector X(K);
  
  Rcpp::NumericVector strengthunique = wrap(unique(as<arma::vec>(strength)));
 
  DataFrame outdata;
  
  Function formatDate("format.Date");
  
  //'--- for each date collect prescriptions to one
  
  for (int k = 0; k < K; k++) {
    
    NumericVector evec = Rcpp::as<Rcpp::NumericVector>(wrap(arma::find(as<arma::vec>(pdate) == T[k])));
    
    int nevec = evec.size();
    
    Rcpp::NumericVector strength1 = strength[evec];
    Rcpp::NumericVector npack1    = npack[evec];
    Rcpp::NumericVector ppp1      = ppp[evec];
    
    for (int j = 0; j < J; j++) {
      for (int e = 0; e < nevec; e++) {
        if (strength1[e] == dval[j]) {
          n(k, j) += npack1[e] * ppp1[e] * strength1[e];
        }
      }
      D[k] += n(k, j);
      n(k, j) = n(k, j) / (double) dmin[j];
      c[k] += (n(k, j) > 0);
      S[k] += (n(k, j) > 0) * dval[j];
    }
    
    S[k] = S[k] / (double) c[k];
    
    double DH = 0; 
    
    for (int q = 0; q < admin.size(); ++q) { 
      
      if (pdate[k] == pdate[k] && pdate[k+1] == pdate[k+1] &&  
          pdate[k] <= admin[q] && pdate[k+1] >= admin[q])
        DH += max(NumericVector::create(0, min(NumericVector::create(admout[q], pdate[k+1])) - 
          max(NumericVector::create(admin[q], pdate[k]))));  
    }
    
    H[k] = max(NumericVector::create(1, pdate[k+1] - pdate[k] - DH));
    
    double nk = sum(n(k,_));
    
    if (nk > H[k] && k < K-1)
      u[k] = 1;

    for (int j = 0; j < J; j++) {
      if (S[k] >= dval[j]) {
        jk[k] = j;
      }
    }
  
    if (jk[k] == jk[k-1] && k > 0)
      w[k-1] = 1; 
    
    i0[k] = N;
    
    double Dsum; 
    double Hsum; 
    
    for (int l = 1; l < N; ++l) {
      if (k-l >= 0) {
        if (w[k] == 1) {
          if (u[k-l] == 1 && w[k-l] == 1) {
            i0[k] = N-l;
            Dsum += D[k-l];
            Hsum += H[k-l];
          }
          else 
            l = N; 
        } else {
          if (u[k-l] == 1) {
            i0[k] = N-l;
            Dsum += D[k-l];
            Hsum += H[k-l];
        } else 
            l = N; 
        }
      }
    }
    
    if (Hsum > 0)
      M[k] = Dsum / (double) Hsum; 
    else
      M[k] = ddef[jk[k]];
    
    double vmax = (M[k] > dmax[jk[k]]);
    double vmin = (M[k] < dmin[jk[k]]);
    
    X[k] = (1-u[k])*(1-u[k-1]) * ddef[jk[k]] + 
      u[k-1]*w[k]*round(M[k] / (double) dmin[jk[k]]) * dmin[jk[k]] +
      (u[k]*(1-u[k-1]) + u[k-1]*(1-w[k]))*(vmax*dmax[jk[k]] + vmin*dmin[jk[k]] + (1-vmax)*(1-vmin)*ddef[jk[k]]);
    
    if (k > 0)
      R[k] = u[k] * (D[k-1] + R[k-1] - X[k-1]*(Enum[k-1] - T[k-1]));
    
    if (R[k] > maxdepot)
      R[k] = maxdepot;
    
    Enum[k] = (1-u[k])*(1-u[k-1]) * (T[k] - 1 + round((D[k] + R[k]) / (double) ddef[jk[k]])) + 
      (1 - (1-u[k])*(1-u[k-1])) * (T[k] - 1 + round((D[k] + R[k]) / (double) X[k]));
    
    if (k < K-1 && Enum[k] > T[k+1]-1)
      Enum[k] = T[k+1]-1;
    
    idout[k] = id[0];
    B[k]     = as<std::string>(formatDate(wrap(Date(T[k]))));
    E[k]     = as<std::string>(formatDate(wrap(Date(Enum[k]))));
    
  }
  
  outdata =  Rcpp::DataFrame::create(Rcpp::Named("id")     = idout,
                                     Rcpp::Named("X")      = X,
                                     Rcpp::Named("B")      = B,
                                     Rcpp::Named("E")      = E,
                                     Rcpp::Named("R")      = R,
                                     Rcpp::Named("D")      = D,
                                     Rcpp::Named("M")      = M,
                                     Rcpp::Named("S")      = S,
                                     Rcpp::Named("H")      = H,
                                     Rcpp::Named("u")      = u,
                                     Rcpp::Named("w")      = w,
                                     Rcpp::Named("i0")     = i0
                                       );
  
  return(outdata);
}

  
