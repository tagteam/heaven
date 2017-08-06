#include <Rcpp.h>
#include <vector>
#include <string>

using namespace Rcpp;

//' @title Split2
//' @author Christian Torp-Pedersen
//' @export
// [[Rcpp::export]]

DataFrame split2 (std::vector<std::string> pnr, //ID
                  IntegerVector   inn,  //Start intervals
                  IntegerVector   out, //End intervals
                  IntegerVector   dato, // Split dates - NA interpreted as zero
                  IntegerVector   dead // Event at end of interval 0/1
){
  std::vector<std::string> Opnr; //ID output
  std::vector<int> Oin, Oout, Odato, Odead; // Output: in/out/split date, event
  int dim;
  Rcpp::DataFrame OUT; // result!
 
  dim=pnr.size();
  Opnr.reserve(dim*1.8); // Allow 80% split before vector is moved
  Oin.reserve(dim*1.8);
  Oout.reserve(dim*1.8);
  Odato.reserve(dim*1.8);
  Odead.reserve(dim*1.8);
    
  for(int i=0; i<dim; i++ ){
    if (dato(i)<=inn(i)){
      Opnr.push_back(pnr[i]);
      Oin.push_back(inn(i));
      Oout.push_back(out(i));
      Odato.push_back(1);
      Odead.push_back(dead(i));
    }
      else if(dato(i)>out(i)){
        Opnr.push_back(pnr[i]);
        Oin.push_back(inn(i));
        Oout.push_back(out(i));
        Odato.push_back(0);
        Odead.push_back(dead(i));
      }
        else if((dato(i)>inn(i)) & (dato(i)<=out(i))){
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
  OUT=DataFrame::create(_["pnr"]=Opnr, _["inn"]=Oin, _["out"]=Oout, _["dato"]=Odato, _["dead"]=Odead );
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
