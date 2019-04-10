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
//' @param prescriptionwindow prescription window
//' @param maxdepot see medicine macro
//' @author Helene Charlotte Rytgaard and Thomas Alexander Gerds
//' @export
// [[Rcpp::export]]
Rcpp::List innerMedicinMacro(Rcpp::DataFrame dat,
			Rcpp::DataFrame admdat,
			Rcpp::List doses, 
			NumericVector idunique,
			double prescriptionwindow, 
			double maxdepot
			) {
  // NOTATION
  // T(k) sequence of purchase dates
  // S(k) strength
  // currentpurchase(k) total purchase at T(k)
  
  // n(k) exposure days
  // B(k) starts of exposure periods
  // E(k) estimated ends of exposure periods
  
  uword NOBS= idunique.size();
  Rcpp::List xxx(NOBS);
  // Rcpp::List outlist(int nobs);
  arma::vec drugstrengths = Rcpp::as<arma::vec>(doses["value"]);
  arma::vec dmin = Rcpp::as<arma::vec>(doses["min"]);
  arma::vec dmax = Rcpp::as<arma::vec>(doses["max"]);
  arma::vec ddef = Rcpp::as<arma::vec>(doses["def"]);

  arma::vec INid = Rcpp::as<arma::vec>(dat["pnr"]); 
  arma::vec INpdate = Rcpp::as<arma::vec>(dat["eksd"]);
  arma::vec INstrength = Rcpp::as<arma::vec>(dat["strnum"]);
  arma::vec INnpack = Rcpp::as<arma::vec>(dat["packsize"]);
  arma::vec INppp = Rcpp::as<arma::vec>(dat["apk"]);
  
  arma::vec INaid = Rcpp::as<arma::vec>(admdat["pnr"]); 
  arma::vec INadmin = Rcpp::as<arma::vec>(admdat["inddto"]);
  arma::vec INadmax = Rcpp::as<arma::vec>(admdat["uddto"]);
  //  Function formatDate("format.Date");
  for (uword i = 0; i < NOBS; i++) {
    // if (i == 1000 || i == 2000 || i==3000){
    // Rcout << "==============i = " << i << "===============\n\n"<< std::endl;
    // }

    double thisid = idunique(i); 
    arma::uvec did = find(INid==thisid); //OK, index (?)
    arma::uvec aid = find(INaid==thisid); //OK, index (?)
    
    // subset data //
    // aid.print();
    arma::vec admin = INadmin.elem(aid);
    arma::vec admax = INadmax.elem(aid);
    
    // Rcout << "d id's" << std::endl;
    // did.print();
    arma::vec id = INid.elem(did); 
    arma::vec pdate = INpdate.elem(did);
    arma::vec strength = INstrength.elem(did);
    arma::vec npack = INnpack.elem(did);
    arma::vec ppp = INppp.elem(did);
    arma::vec T = unique(pdate);
    uword K = T.n_elem;
    uword J = drugstrengths.n_elem;
    // arma::vec c(K,fill::zeros);
    arma::vec currentpurchase(K,fill::zeros);
    arma::vec S(K,fill::zeros);
    arma::mat n(K, J,fill::zeros); 
    arma::vec maximalreach(K,fill::zeros); 
    arma::vec daysperiod(K,fill::zeros);
    arma::vec dayshospital(K,fill::zeros);
    arma::uvec reach(K,fill::zeros);
    arma::uvec jk(K,fill::zeros);
    arma::uvec w(K,fill::zeros);
    arma::uvec yk(K,fill::ones);

    arma::vec M(K);
    arma::vec i0(K);
  
    arma::vec idout(K);
    // Rcpp::DateVector B(K);
    arma::vec B(K,fill::zeros);
    // Rcpp::DateVector E(K);
    arma::vec E(K,fill::zeros);
    arma::vec EndExposure(K,fill::zeros);
    arma::vec stash(K,fill::zeros);
    arma::vec X(K,fill::zeros);
  
    uword ylength;
    ylength=0;
  
    arma::vec strengthunique = unique(strength);
    // loop over unique prescription dates
    for (uword k = 0; k < K; k++) {
      
      //--- for each date merge prescriptions to one
      arma::uvec datekid = arma::find(pdate == T(k));
      uword ndatekid = datekid.n_elem;

      // arma::vec dates1 = pdate.elem(datekid);
      arma::vec strength1 = strength.elem(datekid);
      arma::vec npack1    = npack.elem(datekid);
      arma::vec ppp1      = ppp.elem(datekid);
      // Rcout << "0b" << std::endl;
      
      for (uword g = 0; g < ndatekid; g++) {
	S(k) += strength1(g);
	double numberunits = npack1(g) * ppp1(g) * strength1(g);
	//--- compute total amount of drug purchased on date Tk. 
	// Rcout << "T(k)=" << T(k) << std::endl;
	// Rcout << "dates1(g)=" << dates1(g) << std::endl;
	// Rcout << "strength1(g)=" << strength1(g) << std::endl;
	// Rcout << "npack1(g)=" << npack1(g) << std::endl;
	// Rcout << "ppp1(g)=" << ppp1(g) << std::endl;      
	// Rcout << "currentpurchase(k)=" << currentpurchase(k) << std::endl;      
	currentpurchase(k) += numberunits;
	// Rcout << "currentpurchase(k)=" << currentpurchase(k) << std::endl;
	// Rcout << "-------------------" << std::endl;
	for (uword j = 0; j < J; j++) {
	  if (strength1(g) == drugstrengths(j)) {
	    // n(,) is the matrix which contains the total amount of smallest units:
	    // one row for each date
	    // one column for each drug strength
	    // Rcout << "numberunits=" << numberunits << std::endl;
	    // Rcout << "dmin(j)=" << dmin(j) << std::endl;
	    n(k, j) += numberunits / (double) dmin(j);
	  }
	}
      }
      // Rcout << "Armadillo matrix n is" << std::endl << n << std::endl;
      S(k) = S(k) / (double) ndatekid;

      // Compute number of days non-hospitalized in the period from Tk to Tk+1
      if (k < K-1) {
	for (uword q = 0; q < admin.size(); ++q) {
	  dayshospital(k) += std::max(0.0, ((std::min(admax(q), T(k+1))) - std::max(admin(q), T(k))));  
	}
	daysperiod(k) = std::max(1.0, T(k+1) - T(k) - dayshospital(k));
      } else {
	daysperiod(k) = -9;
      }
      
      // maximal number of days of drug supply 
      
      maximalreach(k) = sum(n.row(k));
      
      // check if there is overlap (if the current period reaches the next)
      // Rcout << "------k=" << k << std::endl;      
      // Rcout << "daysperiod(k)=" << daysperiod(k) << std::endl;
      // Rcout << "number days by current purchase maximalreach(k): " <<  maximalreach(k) << std::endl;
      // Rcout << "stash(k): " <<  stash(k) << std::endl;
      // Rcout << "number days covered (since T(k-1)): maximalreach(k)=" << maximalreach(k) + stash(k) << std::endl;
      // hahaha: due to collapsing periods we need to add the stash to maximalreach
      if (k<K-1){
	if (stash(k) + maximalreach(k) > daysperiod(k) && k < K-1) reach(k) = 1;
	// Rcout << "reach(k)=" << reach(k) << std::endl;      
      }else{
	reach(k) = 99;
      }
      // identify the nearest drug strength that does not exceed the first preliminary average strength
      for (uword j = 0; j < J; j++) {
	if (S(k) >= drugstrengths(j)) {
	  jk(k) = j;
	}
      }
      // compute averages over the relevant periods:
      // Dsum is the numerator,
      // Hsum is the denominator 
      if (k > 0 && jk(k) == jk(k-1)) w(k-1) = 1; 
      i0(k) = 0;
      double Dsum = 0; 
      double Hsum = 0;
      for (uword l = 1; l < prescriptionwindow+1; ++l) {
	if (k>=l) {
	  // check first if dosis is the same
	  if (w(k-1) == 1) {
	    // check if overlap, i.e. Case I
	    if (reach(k-l) == 1 && w(k-l) == 1) {
	      i0(k) = l;
	      Dsum += currentpurchase(k-l);
	      Hsum += daysperiod(k-l);
	    }
	    // if not overlap
	    else 
	      l = prescriptionwindow+1; 
	  } else { // if dosis not the same
	    if (reach(k-l) == 1) { // if overlap, i.e. Case II
	      i0(k) = l;
	      Dsum += currentpurchase(k-l);
	      Hsum += daysperiod(k-l);
	    } else // if not overlap
	      l = prescriptionwindow+1; 
	  }
	}
      }

      // average dosis
      if (Hsum > 0)
	M(k) = Dsum / (double) Hsum; 
      else 
	M(k) = ddef(jk(k));
      double vmax = (M(k) > dmax(jk(k)));
      double vmin = (M(k) < dmin(jk(k)));
      
      // Final dose formula
      if (k>0){
	if (reach(k-1)==0){ // cannot reach
	  X(k) = ddef(jk(k));
	}else{// reach(k-1)=1 can reach
	  X(k)= w(k-1)*((1-vmax)*(1-vmin)*round(M(k) / (double) dmin(jk(k))) * dmin(jk(k)) + vmax*dmax(jk(k)) + vmin*dmin(jk(k))) +
	    (1-w(k-1))*(vmax*dmax(jk(k)) + vmin*dmin(jk(k)) + (1-vmax)*(1-vmin)*ddef(jk(k)));
	}
      }else{
	X(k)=ddef(jk(k));
      }
      // ----------------------------------------------------------------------------------------------
      // compute the end dates of exposure
      // ----------------------------------------------------------------------------------------------
      // Rcout << "T(k)=" << T(k) << std::endl;
      // Rcout << "currentpurchase(k)=" << currentpurchase(k) << std::endl;
      // Rcout << "stash(k)=" << stash(k) << std::endl;      
      EndExposure(k) = (T(k) - 1.0 + floor((currentpurchase(k) + stash(k)) / (double) X(k)));
      // set all id values
      idout(k) = id(0); 
      // -------------------------------------------------------------------------------------------------------
      // calculate stash (rest) for next period (leftover dosis from current period)
      // and truncate EndExposure at T(k+1)-1 
      // -------------------------------------------------------------------------------------------------------
      // important: we first calculate the stash for the next period and then reset the start time
      // we have a rest
      if (k<K-1) {// T(K-1) is the last date
	if (EndExposure(k) >= T(k+1)-1){
	  // Rcout << "T(k+1)-1=" << T(k+1)-1 << std::endl;
	  // Rcout << "EndExposure(k)=" << EndExposure(k) << std::endl;
	  // Rcout << "(EndExposure(k) - T(k+1) + 1 - dayshospital(k)):=" << (EndExposure(k) - T(k+1) + 1 - dayshospital(k)) << std::endl;	  
          // stash(k+1)=(currentpurchase(k) + stash(k) - X(k)*(EndExposure(k) - T(k+1) + 1 - dayshospital(k)));
          stash(k+1)= X(k)*(EndExposure(k) - T(k+1) + 1 - dayshospital(k));
	  // Rcout << "stash(k+1)=" << stash(k+1) << std::endl;
	  if (stash(k+1) > maxdepot) stash(k+1) = maxdepot;
	  EndExposure(k)=  T(k+1)-1;
	} // no else because we just use the remaining units until the stash is completely empty
      }// no else because stash(k+1) is initialized as 0.0 
      // -------------------------------------------------------------------------------------------------------
      // check if previous period can be collapsed. if so, reset T(k) to T(k-1)
      // -------------------------------------------------------------------------------------------------------
      if (k > 0) {
	if (X(k-1) == X(k) && EndExposure(k-1) >= (T(k)-1)) {
	  // if doses are the same, and gap is smaller than 1 day
	  // then keep the last.
	  T(k) = T(k-1); // set start date to start date of previous period
	  // rows with yk = 0 will be removed 
	  yk(k-1) = 0; 
	} else{
	  if (X(k-1) != X(k) && EndExposure(k-1) >= (T(k)-1)) {
	    // doses are not the same, gap smaller than 1 day
	    T(k) = std::max(T(k), EndExposure(k-1) + 1);
	    EndExposure(k) = std::max(EndExposure(k), T(k) + 1);
	    // rows with yk = 1 will be kept
	    yk(k-1) = 1; 
	  }else{
	    if (round(EndExposure(k-1)) < (T(k)-1)) { // gap is larger than 1 day
	      // rows with yk = 2 will introduce a period with 0 exposure
	      yk(k-1) = 2;
	    }
	  }
	}
	ylength += yk(k-1); // this defines the length of output data 
      }
      B(k) = T(k);
      E(k) = EndExposure(k);
    } // end loop over unique prescription dates
  
    yk(K-1) = 1; 
    ylength += 1;
    arma::vec Sjk = drugstrengths(jk); 
    
    // -------------------------------------------------------------------------------------------------------
    // collapse periods
    // -------------------------------------------------------------------------------------------------------
    arma::vec id1(ylength,fill::zeros);
    arma::vec B1(ylength); 
    arma::vec E1(ylength); 
    arma::vec X1(ylength); 
    uword k1 = 0;
    // remove rows in continuous exposure periods with same exposure
    for (uword k = 0; k < K; k++) {
      // decide if row should be kept or removed:
      // yk(k)=0 means remove row (to collapse intervals with same exposure)
      // yk(k)=1 means 
      // yk(k)=2 means to add intervals with zero exposure
      // Rcout << "_________k=" << k << "________" << std::endl;
      // Rcout << "yk(k)=" << yk(k) << std::endl;
      // Rcout << "T(k)=" << T(k) << std::endl;
      // Rcout << "E(k)=" << E(k) << std::endl;
      // Rcout << "EndExposure(k)=" << EndExposure(k) << std::endl;
      if (yk(k)==2){
	// create a row with zero exposure
	id1(k1) = id(0);
	B1(k1) = EndExposure(k)+1;
	// Rcout << "T(k+1)=" << T(k) << std::endl;
	E1(k1) = T(k+1)-1;
	X1(k1) = 0; 
	k1 += 1;
      }
      if (yk(k)>0){
	// only keep relevant rows
	id1(k1) = id(0);
	B1(k1) = T(k);
	// Rcout << "(k)=" << k << std::endl;
	E1(k1) = E(k);
	// Rcout << "EndExposure(k)=" << EndExposure(k) << std::endl;
	X1(k1) = X(k);
	k1 += 1;
      }
    }
    // FIXME:
    // to speed up we would like to return a matrix instead of a data frame
    // but B1 and E1 are date format
    xxx[i] = Rcpp::DataFrame::create(Rcpp::Named("pnr") = id1,
				     Rcpp::Named("X") = X1,
				     Rcpp::Named("B") = B1,
				     Rcpp::Named("E") = E1);
  }
  // Rcout << "after loop \n"<< std::endl;  
  return(xxx);
}
