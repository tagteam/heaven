// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
#include <vector>
#include <string>
using namespace Rcpp;
// [[Rcpp::export]]
DataFrame split2  (IntegerVector   pnr, //ID
                   IntegerVector   inn,  //Start intervals
                   IntegerVector   out, //End intervals
                   IntegerVector   dato, // Split dates - NA interpreted as zero
                   IntegerVector   dead // Event at end of interval 0/1
){
  std::vector<int> Opnr; //ID output
  std::vector<int> Oin, Oout, Odato, Odead; // Output: in/out/split date, event
  int dim;
  Rcpp::DataFrame OUT; // result!
  
  dim=pnr.size();
  Opnr.reserve(dim*2); // Allow 80% split before vector is moved
  Oin.reserve(dim*2);
  Oout.reserve(dim*2);
  Odato.reserve(dim*2);
  Odead.reserve(dim*2);
  
  for(int i=0; i<dim; i++ ){
    
    if(dato(i)>out(i) || dato(i)<-2000000000){ // date later than period or integer missing - set Odato to zero
      Opnr.push_back(pnr[i]);
      Oin.push_back(inn(i));
      Oout.push_back(out(i));
      Odato.push_back(0);
      Odead.push_back(dead(i));
    }
    else if (dato(i)<=inn(i)){ // date prior to period or touching period - set Odato to 1
      Opnr.push_back(pnr[i]);
      Oin.push_back(inn(i));
      Oout.push_back(out(i));
      Odato.push_back(1);
      Odead.push_back(dead(i));
    }
    else if((dato(i)>inn(i)) && (dato(i)<out(i))){ // date IN period create 2 records
      Opnr.push_back(pnr[i]);
      Oin.push_back(inn(i));
      Oout.push_back(dato(i));
      Odato.push_back(0);
      Odead.push_back(0);
      Opnr.push_back(pnr[i]);
      Oin.push_back(dato(i));
      Oout.push_back(out(i));
      Odato.push_back(1);
      Odead.push_back(dead(i));
    }
    else if(dato(i)==out(i)) { // date eq end of period - create 2 records with event, otherwise create as if date>period
      if (dead(i)==0 || inn(i)==out(i)){ // No changes
        Opnr.push_back(pnr[i]);
        Oin.push_back(inn(i));
        Oout.push_back(out(i));
        Odato.push_back(1);
        Odead.push_back(dead(i));
      }
      else{
        Opnr.push_back(pnr[i]);
        Oin.push_back(inn(i));
        Oout.push_back(dato(i));
        Odato.push_back(0);
        Odead.push_back(0);
        Opnr.push_back(pnr[i]);
        Oin.push_back(dato(i));
        Oout.push_back(out(i));
        Odato.push_back(1);
        Odead.push_back(dead(i));
      }
    }
  }
  OUT=DataFrame::create(_["pnrnum"]=Opnr, _["inn"]=Oin, _["out"]=Oout, _["dato"]=Odato, _["dead"]=Odead );
  return(OUT);
}



//*** R
//pnr <- c("A","A","B","B") #ID
//inn <-  c(0,100,100,200) #Start intervals
//out <- c(100,200,200,300) #End intervals
//dato <- c(100,100,250,250) # Split dates - NA interpreted as zero
//dead <- c(0,1,0,1) # Event at end of interval 0/1
//split2(pnr,inn,out,dato,dead)
//*/
