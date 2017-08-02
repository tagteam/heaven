// [[Rcpp::depends(RcppArmadillo)]]
// # define ARMA_NO_DEBUG
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' @description Inner process of medicin macro
//' @title The heart of the medicin macro
//' @param dat data set
//' @param admdat admission data
//' @param doses doses
//' @param idunique unique subject ids
//' @param drug name of drug variable
//' @param N sample size
//' @param maxdepot see medicine macro
//' @param collapse If \code{TRUE} collapse admission periods when there is not a single day out of hospital in between.
//' @author Helene Charlotte Rytgaard
//' @export
// [[Rcpp::export]]
Rcpp::List innerprocess(Rcpp::DataFrame dat,
			Rcpp::DataFrame admdat,
			Rcpp::List doses, 
			Rcpp::IntegerVector idunique,
			std::string drug,
			double N, 
			double maxdepot,
			bool collapse
			) {
  int NOBS= idunique.size();
  Rcpp::List xxx(NOBS);
  // Rcpp::List outlist(int nobs);
  arma::vec dval = Rcpp::as<arma::vec>(doses["value"]);
  arma::vec dmin = Rcpp::as<arma::vec>(doses["min"]);
  arma::vec dmax = Rcpp::as<arma::vec>(doses["max"]);
  arma::vec ddef = Rcpp::as<arma::vec>(doses["def"]);

  arma::uvec INid = Rcpp::as<arma::uvec>(dat["id"]);
  arma::vec INpdate = Rcpp::as<arma::vec>(dat["pdate"]);
  arma::vec INstrength = Rcpp::as<arma::vec>(dat["strength"]);
  arma::vec INnpack = Rcpp::as<arma::vec>(dat["npack"]);
  arma::vec INppp = Rcpp::as<arma::vec>(dat["ppp"]);
  
  arma::uvec INaid = Rcpp::as<arma::uvec>(admdat["id"]);
  arma::vec INadmin = Rcpp::as<arma::vec>(admdat["inddto"]);
  arma::vec INadmax = Rcpp::as<arma::vec>(admdat["uddto"]);
  
  Function formatDate("format.Date");
  // Rcout << "before loop \n"<< std::endl;  
  for (uword i = 0; i < NOBS; i++) {
    // if (i == 1000 || i == 2000 || i==3000){
    // Rcout << "==============i = " << i << "===============\n\n"<< std::endl;
    // }
    arma::uword thisid = idunique(i);
    arma::uvec did = find(INid==thisid);
    arma::uvec aid = find(INaid==thisid);
    
    // subset data //
    // aid.print();
    arma::vec admin = INadmin.elem(aid);
    arma::vec admax = INadmax.elem(aid);
    
    // Rcout << "d id's" << std::endl;
    // did.print();
    arma::uvec id = INid.elem(did);
    arma::vec pdate = INpdate.elem(did);
    arma::vec strength = INstrength.elem(did);
    arma::vec npack = INnpack.elem(did);
    arma::vec ppp = INppp.elem(did);
    arma::vec T = unique(pdate);
    uword K = T.n_elem;
    uword J = dval.n_elem;
    arma::vec c(K,fill::zeros);
    arma::vec D(K,fill::zeros);
    arma::vec S(K,fill::zeros);
    arma::mat n(K, J,fill::zeros); 
    arma::vec nk(K,fill::zeros); 
    arma::vec H(K,fill::zeros);
    arma::vec DH(K,fill::zeros);
    arma::uvec u(K,fill::zeros);
    arma::uvec jk(K,fill::zeros);
    arma::uvec w(K,fill::zeros);
    arma::uvec yk(K,fill::ones);

    arma::vec M(K);
    arma::vec i0(K);
  
    arma::uvec idout(K);
    Rcpp::DateVector B(K);
    Rcpp::DateVector E(K);
    arma::vec Enum(K);
    arma::vec R(K);
    arma::vec X(K);
  
    uword ylength;
    ylength=0;
  
    arma::vec strengthunique = unique(strength);
    // loop over unique prescription dates
    for (uword k = 0; k < K; k++) {
      
      //--- for each date merge prescriptions to one
      arma::uvec datekid = arma::find(pdate == T(k));
      uword ndatekid = datekid.n_elem;
    
      arma::vec strength1 = strength.elem(datekid);
      arma::vec npack1    = npack.elem(datekid);
      arma::vec ppp1      = ppp.elem(datekid);
      // Rcout << "0b" << std::endl;

      for (uword g = 0; g < ndatekid; g++) {
	S(k) += strength1(g);
	double ne = npack1(g) * ppp1(g) * strength1(g); 
	//--- compute total amount of drug purchased on date Tk. 
	D(k) += ne;
	// n(,) is the matrix which contains the total amount of smallest units:
	// one row for each date
	// one column for each drug strength
	for (uword j = 0; j < J; j++) {
	  if (strength1(g) == dval(j)) {
	    n(k, j) += ne / (double) dmin(j);
	  }
	}
      }
      // Rcout << "0c" << std::endl;      
      if (!collapse)
	c(k) = ndatekid;
      S(k) = S(k) / (double) ndatekid;

      // Compute number of days non-hospitalized in the period from Tk to Tk+1
      if (k < K-1) {
	for (uword q = 0; q < admin.size(); ++q) {
	  DH(k) += std::max(0.0, ((std::min(admax(q), T(k+1))) - std::max(admin(q), T(k))));  
	}
	H(k) = std::max(1.0, T(k+1) - T(k) - DH(k));
      } else {
	H(k) = 1;
      }
      // Rcout << "here" << std::endl;
      
      // maximal number of days of drug supply 
      
      nk(k) = sum(n.row(k));
      
      // check if there is overlap (if the current period reaches the next)
      if (nk(k) > H(k) && k < K-1)
	u(k) = 1;
      // Rcout << "or here" << std::endl;
      // identify the nearest drug strength that does not exceed the first preliminary average strength
      for (uword j = 0; j < J; j++) {
	if (S(k) >= dval(j)) {
	  jk(k) = j;
	}
      }
      // Rcout << "0d" << std::endl;
      // compute averages over the relevant periods:
      // Dsum is the numerator,
      // Hsum is the denominator 
      if (k > 0 && jk(k) == jk(k-1)) w(k-1) = 1; 
      i0(k) = 0;
      double Dsum = 0; 
      double Hsum = 0;
      // Rcout << "0e" << std::endl;
      for (uword l = 1; l < N+1; ++l) {
	if (k>=l) {
	  // check first if dosis is the same
	  // Rcout << "k-l=" << (k-l) << std::endl;
	  // Rcout << "k=" << k << std::endl;
	  // Rcout << "l=" << l << std::endl;
	  if (w(k-1) == 1) {
	    // check if overlap, i.e. Case I
	    if (u(k-l) == 1 && w(k-l) == 1) {
	      i0(k) = l;
	      Dsum += D(k-l);
	      Hsum += H(k-l);
	    }
	    // if not overlap
	    else 
	      l = N+1; 
	  } else { // if dosis not the same
	    if (u(k-l) == 1) { // if overlap, i.e. Case II
	      i0(k) = l;
	      Dsum += D(k-l);
	      Hsum += H(k-l);
	    } else // if not overlap
	      l = N+1; 
	  }
	}
      }
      // Rcout << "a" << std::endl;

      // average dosis
      if (Hsum > 0)
	M(k) = Dsum / (double) Hsum; 
      else 
	M(k) = ddef(jk(k));
      double vmax = (M(k) > dmax(jk(k)));
      double vmin = (M(k) < dmin(jk(k)));
      
      // Final dose formula
      if (k>0){
	X(k) = (1-u(k-1)) * ddef(jk(k)) + 
	  u(k-1)*w(k-1)*((1-vmax)*(1-vmin)*round(M(k) / (double) dmin(jk(k))) * dmin(jk(k)) + vmax*dmax(jk(k)) + vmin*dmin(jk(k))) +
	  (u(k-1)*(1-w(k-1)))*(vmax*dmax(jk(k)) + vmin*dmin(jk(k)) + (1-vmax)*(1-vmin)*ddef(jk(k)));
      }else{
	X(k)=ddef(jk(k));
      }

      // compute leftover dosis
      if (k > 0)
	R(k) = std::max(0.0, u(k-1) * (D(k-1) + R(k-1) - X(k-1)*(Enum(k-1) - T(k-1) - DH(k-1))));
      if (R(k) > maxdepot) 
	R(k) = maxdepot;

      // compute the end dates of exposure
      if (k > 0){      
	Enum(k) = (1-u(k))*(1-u(k-1)) * (T(k) - 1 + round((D(k) + R(k)) / (double) ddef(jk(k)))) + 
	  (1 - (1-u(k))*(1-u(k-1))) * (T(k) - 1 + round((D(k) + R(k)) / (double) X(k)));
      }else{
	Enum(k) = (T(k) - 1 + round((D(k) + R(k)) / (double) ddef(jk(k))));
      }
    
      if (k < K-1 && Enum(k) > T(k+1)-1)
	Enum(k) = T(k+1)-1;

      idout(k) = id(0);
      
      // check if periods can be concatenated
      if (k > 0 && collapse) {
	if (X(k-1) == X(k) && Enum(k-1) >= (T(k)-1)) { 
	  // if doses are the same, and gap is smaller than 1 day
	  // then keep the last.  
	  T(k) = T(k-1); // set start date to start date of previous period
	  // rows with yk = 0 will be removed 
	  yk(k-1) = 0; 
	} else if (X(k-1) != X(k) && Enum(k-1) >= (T(k)-1)) { // check if doses are not the same, gap smaller than 1 day
	  T(k) = std::max(T(k), Enum(k-1) + 1);
	  Enum(k) = std::max(Enum(k), T(k) + 1);
	  // rows with yk = 1 will be kept
	  yk(k-1) = 1; 
	} else if (round(Enum(k-1)) < (T(k)-1)) { // gap is larger than 1 day
	  // rows with yk = 2 will introduce a period with 0 exposure
	  yk(k-1) = 2; 
	}
	ylength += yk(k-1); // this will define length of output data
      }
    
      B(k) = as<std::string>(formatDate(wrap(Date(T(k)))));
      E(k) = as<std::string>(formatDate(wrap(Date(Enum(k)))));
    } // end loop over unique prescription dates
  
    yk(K-1) = 1; 
    ylength += 1;
    arma::vec Sjk = dval(jk); 
    
    if (!collapse) {
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
      arma::uvec id1(ylength,fill::zeros);
      // Rcout << " ylength "  << ylength << std::endl;
      // id1+=id(0);
      Rcpp::DateVector B1(ylength); 
      Rcpp::DateVector E1(ylength); 
      arma::vec X1(ylength); 
      uword k1 = 0;
      // remove rows in continuous exposure periods with same exposure
      for (uword k = 0; k < K; k++) {
	// decide if row should be kept or removed:
	// yk(k)=0 collapse intervals with same exposure
	// yk(k)=2 add intervals with zero exposure
	if (yk(k)==2){
	  // create a row with zero exposure
	  id1(k1) = id(0);
	  B1(k1) = Enum(k)+1; 
	  E1(k1) = T(k+1)-1;
	  X1(k1) = 0; 
	  k1 += 1;
	}
	if (yk(k)>0){
	  // only keep relevant rows
	  id1(k1) = id(0);
	  B1(k1) = T(k);
	  E1(k1) = E(k);
	  X1(k1) = X(k);
	  k1 += 1;
	}
      }
      // to speed up we would like to return a matrix instead of a data fram
      // but B1 and E1 are date format
      // arma:mat xxxi = arma::zeros(ylength,4);
      // xxxi.col(0) = arma::conv_to<arma::vec>::from(id1);
      // xxxi.col(1)=arma::conv_to<arma::vec>::from(X1);
      // xxxi.col(2)=B1;
      // xxxi.col(3)=E1;
      // xxx[i] = xxxi;
      xxx[i] = Rcpp::DataFrame::create(Rcpp::Named("id") = id1,
       Rcpp::Named("X") = X1,
       Rcpp::Named("B") = B1,
       Rcpp::Named("E") = E1);
    }
  }
  // Rcout << "after loop \n"<< std::endl;  
  return(xxx);
}
