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
                             double N, 
                             double maxdepot, 
                             bool trace
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
  Rcpp::NumericVector nk(K); 
  Rcpp::NumericVector H(K);
  Rcpp::NumericVector DH(K);
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
    
    for (int e = 0; e < nevec; e++) {
      S[k] += strength1[e];
      for (int j = 0; j < J; j++) {
        if (strength1[e] == dval[j]) {
          double ne = npack1[e] * ppp1[e] * strength1[e]; 
          D[k] += ne;
          n(k, j) += ne / (double) dmin[j];
        }
      }
    }
    
    c[k] = nevec;
    S[k] = S[k] / (double) c[k];
    
    if (k < K-1) {
      for (int q = 0; q < admin.size(); ++q) { 
        if (T[k] == T[k] && T[k+1] == T[k+1])
          DH[k] += max(NumericVector::create(0, min(NumericVector::create(admout[q], T[k+1])) - 
            max(NumericVector::create(admin[q], T[k]))));  
      }
      H[k] = max(NumericVector::create(1, T[k+1] - T[k] - DH[k]));
    } else {
      H[k] = 1;
    }
    
    nk[k] = sum(n(k,_));
    
    if (nk[k] > H[k] && k < K-1)
      u[k] = 1;
    
    for (int j = 0; j < J; j++) {
      if (S[k] >= dval[j]) {
        jk[k] = j;
      }
    }
    
    if (jk[k] == jk[k-1] && k > 0)
      w[k-1] = 1; 
    
    i0[k] = 0;
    
    double Dsum = 0; 
    double Hsum = 0; 
    
    for (int l = 1; l < N+1; ++l) {
      if (k-l >= 0) {
        if (w[k-1] == 1) {
          if (u[k-l] == 1 && w[k-l] == 1) {
            i0[k] = l;
            Dsum += D[k-l];
            Hsum += H[k-l];
          }
          else 
            l = N+1; 
        } else {
          if (u[k-l] == 1) {
            i0[k] = l;
            Dsum += D[k-l];
            Hsum += H[k-l];
          } else 
            l = N+1; 
        }
      }
    }
    
    if (Hsum > 0)
      M[k] = Dsum / (double) Hsum; 
    else 
      M[k] = ddef[jk[k]];
    
    double vmax = (M[k] > dmax[jk[k]]);
    double vmin = (M[k] < dmin[jk[k]]);
    
    X[k] = (1-u[k-1]) * ddef[jk[k]] + 
      u[k-1]*w[k-1]*((1-vmax)*(1-vmin)*round(M[k] / (double) dmin[jk[k]]) * dmin[jk[k]] + vmax*dmax[jk[k]] + vmin*dmin[jk[k]]) +
      (u[k-1]*(1-w[k-1]))*(vmax*dmax[jk[k]] + vmin*dmin[jk[k]] + (1-vmax)*(1-vmin)*ddef[jk[k]]);
    
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
    
    if (trace) {
      if (k < 1)
        Rcout << std::endl << "id = " << id[0] << std::endl;
      Rcout << "uk = " << u[k] << ", S = " << S[k];
      if (i0[k] > 0 && i0[k] > 1)
        Rcout << ", Ik = {k-" << i0[k] << ", ... , k}"<< std::endl;
      else if (i0[k] == 1)
        Rcout << ", Ik = {k-1, k}" << std::endl;
      else
        Rcout << ", Ik = {k}" << std::endl;
      
    }   
    
  }
  
  outdata =  Rcpp::DataFrame::create(Rcpp::Named("id")     = idout,
                                     Rcpp::Named("X")      = X,
                                     Rcpp::Named("B")      = B,
                                     Rcpp::Named("E")      = E,
                                     Rcpp::Named("R")      = R,
                                     Rcpp::Named("D")      = D,
                                     Rcpp::Named("M")      = M,
                                     Rcpp::Named("A")      = S,
                                     Rcpp::Named("c")      = c,
                                     Rcpp::Named("jk")     = jk,
                                     Rcpp::Named("Sjk")    = dval[jk],
                                     Rcpp::Named("H")      = H,
                                     Rcpp::Named("DH")     = DH,
                                     Rcpp::Named("nk")     = nk,
                                     Rcpp::Named("u")      = u,
                                     Rcpp::Named("w")      = w,
                                     Rcpp::Named("i0")     = i0
  );
  
  return(outdata);
}