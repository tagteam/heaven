// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' @title Inner process function.
//' @author Helene Charlotte Rytgaard
//' @export
// [[Rcpp::export]]
Rcpp::List innerprocess(Rcpp::DataFrame dat,
			Rcpp::DataFrame admdat,
			Rcpp::List doses, 
			Rcpp::IntegerVector idunique,
			std::string treatname,
			double N, 
			double maxdepot, 
			bool trace, 
			bool out
			) {
  int NOBS= idunique.size();
  Rcpp::List xxx(NOBS);
  // Rcpp::List outlist(int nobs);
  arma::vec dval = Rcpp::as<arma::vec>(doses["value"]);
  arma::vec dmin = Rcpp::as<arma::vec>(doses["min"]);
  arma::vec dmax = Rcpp::as<arma::vec>(doses["max"]);
  arma::vec ddef = Rcpp::as<arma::vec>(doses["def"]);

  arma::vec INid = Rcpp::as<arma::vec>(dat["id"]);
  arma::vec INpdate = Rcpp::as<arma::vec>(dat["pdate"]);
  arma::vec INstrength = Rcpp::as<arma::vec>(dat["strength"]);
  arma::vec INnpack = Rcpp::as<arma::vec>(dat["npack"]);
  arma::vec INppp = Rcpp::as<arma::vec>(dat["ppp"]);
  
  arma::vec INaid = Rcpp::as<arma::vec>(admdat["id"]);
  arma::vec INadmin = Rcpp::as<arma::vec>(admdat["inddto"]);
  arma::vec INadmax = Rcpp::as<arma::vec>(admdat["uddto"]);
  
  Function formatDate("format.Date");
  
  for (int i = 0; i < NOBS; i++) {
    // subset data //
    arma::uvec aid = find(INaid==i+1);
    arma::vec admin = INadmin.elem(aid);
    arma::vec admax = INadmax.elem(aid);
    
    arma::uvec did = find(INid==i+1);
    arma::vec id = INid.elem(did);
    arma::vec pdate = INpdate.elem(did);
    arma::vec strength = INstrength.elem(did);
    arma::vec npack = INnpack.elem(did);
    arma::vec ppp = INppp.elem(did);
    arma::vec T = unique(pdate);

    double K = T.size();
    double J = dval.size();
  
    arma::vec c(K);
    arma::vec D(K);
    arma::vec S(K);
    arma::mat n(K, J); 
    arma::vec nk(K); 
    arma::vec H(K);
    arma::vec DH(K);
    arma::vec u(K);
    arma::uvec jk(K);
    arma::vec w(K);
    arma::vec M(K);
    arma::vec i0(K);
  
    arma::vec idout(K);
    Rcpp::DateVector B(K);
    Rcpp::DateVector E(K);
    arma::vec Enum(K);
    arma::vec R(K);
    arma::vec X(K);
  
    arma::vec yk(K);
    double ylength;
  
    arma::vec strengthunique = unique(strength);


    //'--- for each date collect prescriptions to one
  
    for (int k = 0; k < K; k++) {
      
      arma::uvec datekid = arma::find(pdate == T(k));
      int ndatekid = datekid.size();
    
      arma::vec strength1 = strength.elem(datekid);
      arma::vec npack1    = npack.elem(datekid);
      arma::vec ppp1      = ppp.elem(datekid);
      
      for (int e = 0; e < ndatekid; e++) {
	S(k) += strength1(e);
	for (int j = 0; j < J; j++) {
	  if (strength1(e) == dval(j)) {
	    double ne = npack1(e) * ppp1(e) * strength1(e); 
	    D(k) += ne;
	    n(k, j) += ne / (double) dmin(j);
	  }
	}
      }
      
      c(k) = ndatekid;
      S(k) = S(k) / (double) c(k);
      
      if (k < K-1) {
	for (int q = 0; q < admin.size(); ++q) { 
	  if (T(k) == T(k) && T(k+1) == T(k+1)){
	    DH(k) += std::max(0.0, ((std::min(admax(q), T(k+1))) - std::max(admin(q), T(k))));  
	  }
	}
	H(k) = std::max(1.0, T(k+1) - T(k) - DH(k));
      } else {
	H(k) = 1;
      }
      nk(k) = sum(n.col(k));
      if (nk(k) > H(k) && k < K-1)
	u(k) = 1;
    
      for (int j = 0; j < J; j++) {
	if (S(k) >= dval(j)) {
	  jk(k) = j;
	}
      }
      if (k > 0 && jk(k) == jk(k-1)) w(k-1) = 1; 
      i0(k) = 0;
      double Dsum = 0; 
      double Hsum = 0; 
      for (int l = 1; l < N+1; ++l) {
	if (k-l >= 0) {
	  if (w(k-1) == 1) {
	    if (u(k-l) == 1 && w(k-l) == 1) {
	      i0(k) = l;
	      Dsum += D(k-l);
	      Hsum += H(k-l);
	    }
	    else 
	      l = N+1; 
	  } else {
	    if (u(k-l) == 1) {
	      i0(k) = l;
	      Dsum += D(k-l);
	      Hsum += H(k-l);
	    } else 
	      l = N+1; 
	  }
	}
      }
      //Rcout << "a" << std::endl;    
      if (Hsum > 0)
	M(k) = Dsum / (double) Hsum; 
      else 
	M(k) = ddef(jk(k));
      double vmax = (M(k) > dmax(jk(k)));
      double vmin = (M(k) < dmin(jk(k)));
      // Rcout << "k = " << k << std::endl;
      // Rcout << "b" << std::endl;    
      // Rcout << "k = " << k << ", jk(k) = " << jk(k);
      if (k>0){
      X(k) = (1-u(k-1)) * ddef(jk(k)) + 
	u(k-1)*w(k-1)*((1-vmax)*(1-vmin)*round(M(k) / (double) dmin(jk(k))) * dmin(jk(k)) + vmax*dmax(jk(k)) + vmin*dmin(jk(k))) +
	(u(k-1)*(1-w(k-1)))*(vmax*dmax(jk(k)) + vmin*dmin(jk(k)) + (1-vmax)*(1-vmin)*ddef(jk(k)));
      }else{
	X(k)=ddef(jk(k));
      }
      if (k > 0)
	R(k) = std::max(0.0, u(k-1) * (D(k-1) + R(k-1) - X(k-1)*(Enum(k-1) - T(k-1) - DH(k-1))));
      if (R(k) > maxdepot) 
	R(k) = maxdepot;
      if (k > 0){      
      Enum(k) = (1-u(k))*(1-u(k-1)) * (T(k) - 1 + round((D(k) + R(k)) / (double) ddef(jk(k)))) + 
	(1 - (1-u(k))*(1-u(k-1))) * (T(k) - 1 + round((D(k) + R(k)) / (double) X(k)));
      }else{
	Enum(k) = (T(k) - 1 + round((D(k) + R(k)) / (double) ddef(jk(k))));
      }
    
      if (k < K-1 && Enum(k) > T(k+1)-1)
	Enum(k) = T(k+1)-1;

      idout(k) = id(0);
          // Rcout << "e" << std::endl;        
      if (trace) {
	if (k < 1)
	  Rcout << std::endl << "id = " << id(0) << std::endl;
	Rcout << "uk = " << u(k) << ", S = " << S(k);
	if (i0(k) > 0 && i0(k) > 1)
	  Rcout << ", Ik = {k-" << i0(k) << ", ... , k}"<< std::endl;
	else if (i0(k) == 1)
	  Rcout << ", Ik = {k-1, k}" << std::endl;
	else
	  Rcout << ", Ik = {k}" << std::endl;
      }   
    
      if (k > 0 && out) {
	if (X(k-1) == X(k) && Enum(k-1) >= (T(k)-1)) {
	  T(k) = T(k-1);
	  yk(k-1) = 0; 
	} else if (X(k-1) != X(k) && Enum(k-1) >= (T(k)-1)) { 
	  T(k) = std::max(T(k), Enum(k-1) + 1);
	  Enum(k) = std::max(Enum(k), T(k) + 1);
	  yk(k-1) = 1; 
	} else if (round(Enum(k-1)) < (T(k)-1)) {
	  yk(k-1) = 2; 
	}
	ylength += yk(k-1);
      }
    
      B(k) = as<std::string>(formatDate(wrap(Date(T(k)))));
      E(k) = as<std::string>(formatDate(wrap(Date(Enum(k)))));
    }
  
    yk(K-1) = 1; 
    ylength += 1;
    arma::vec Sjk = dval(jk); 
    
    if (!out) {
      xxx[i] = Rcpp::DataFrame::create(Rcpp::Named("id")     = idout,
				       Rcpp::Named("X")      = X,
				       Rcpp::Named("B")      = B,
				       Rcpp::Named("E")      = E,
				       Rcpp::Named("R")      = R,
				       Rcpp::Named("D")      = D,
				       Rcpp::Named("M")      = M,
				       Rcpp::Named("A")      = S,
				       Rcpp::Named("c")      = c,
				       Rcpp::Named("jk")     = jk,
				       Rcpp::Named("Sjk")    = Sjk,
				       Rcpp::Named("H")      = H,
				       Rcpp::Named("DH")     = DH,
				       Rcpp::Named("nk")     = nk,
				       Rcpp::Named("u")      = u,
				       Rcpp::Named("w")      = w,
				       Rcpp::Named("i0")     = i0,
				       Rcpp::Named("yj")     = yk);
    } else {
      Rcout << " B = " << B.size() << std::endl;
      Rcout << " E = " << E.size() << std::endl;
      Rcout << " X = " << X.size() << std::endl;
      arma::vec id1(ylength);
      Rcpp::DateVector B1(ylength); 
      Rcpp::DateVector E1(ylength); 
      arma::vec X1(ylength); 
    
      double k1 = 0;
      Rcout << " i = " << i << std::endl;
      Rcout << " K = " << K << std::endl;
    
      for (int k = 0; k < K; k++) {
      
	double K1 = yk(k);
	
	for (int kk = 0; kk < K1; kk++) {
        
	  id1(k1) = id(0);
	  if (!(kk > 0 && K1 == 2)) {
	    B1(k1) = as<std::string>(formatDate(wrap(Date(T(k))))); 
	    E1(k1) = as<std::string>(formatDate(wrap(Date(Enum(k)))));
	    X1(k1) = X(k);
	  } else {
	    B1(k1) = as<std::string>(formatDate(wrap(Date(Enum(k)+1)))); 
	    E1(k1) = as<std::string>(formatDate(wrap(Date(T(k+1)-1)))); 
	    X1(k1) = 0; 
	  } 
	  
	  k1 += 1;
	}
      }
      xxx[i] = Rcpp::DataFrame::create(Rcpp::Named("id") = id1,
				       Rcpp::Named("X") = X1,
				       Rcpp::Named("B") = B1,
				       Rcpp::Named("E") = E1);
    }
  }
  return(xxx);
}
