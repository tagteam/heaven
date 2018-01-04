#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
// [[Rcpp::export]]
List splitDate(IntegerVector inn, // Starttimes - base data
             IntegerVector out, // Endtimes - base data
             IntegerVector event, // Event at end of interval 0/1 - base data
             IntegerVector mergevar, // Merge variable, multiple records can have same pnr - base data
             IntegerVector value, // Number in sequence for each record prior to next split
             IntegerVector seq // Next in sequence to split by
) { 
// This function is intended for creating split data for lexis survival analyses. It handles
// splitting by a defined date from R - age, calender time, time since a key event etc.
// The sequence number is expected to be zero prior to any split.  For each call of the function
// no change is made when the next calling date is after the interval, "1" is added to output "value" 
// when "num" is after and there is a split when "num" is in the interval.

  // Define output vectors
  std::vector<int> Omergevar;  
  Omergevar.reserve(mergevar.size()*5);
  std::vector<int> Oinn;  // Starttimes output
  Oinn.reserve(mergevar.size()*5);
  std::vector<int> Oout; // Endtimes output
  Oout.reserve(mergevar.size()*5);
  std::vector<int> Oevent; // Event at end 0/1
  Oevent.reserve(mergevar.size()*2);
  std::vector<int> Ovalue; // Value for output 0.1,2...
  Ovalue.reserve(mergevar.size()*5);

  for (int i=0; i<mergevar.size(); i++){
    if (seq(i)<=inn(i)){ // seq to the left of interval
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(inn(i));
      Oout.push_back(out(i));
      Oevent.push_back(event[i]);
      Ovalue.push_back(value[i]+1);
    }
    else
    if (seq(i)>=out(i)){ // To the right - record passed without change
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(inn(i));
      Oout.push_back(out(i));
      Oevent.push_back(event[i]);
      Ovalue.push_back(value[i]);
    }
    else
    if (seq(i)>inn(i) && seq(i)<out(i)){// interval to be split
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(inn(i));
      Oout.push_back(seq(i));
      Oevent.push_back(0);
      Ovalue.push_back(value[i]);
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(seq(i));
      Oout.push_back(out(i));
      Oevent.push_back(event[i]);
      Ovalue.push_back(value[i]+1);
    }
    
  }
  return (Rcpp::List::create(Rcpp::Named("pnrnum")=Omergevar, 
                            Rcpp::Named("inn") = Oinn,
                            Rcpp::Named("out") = Oout,
                            Rcpp::Named("event") = Oevent,
                            Rcpp::Named("value") = Ovalue));  
}
    


///*** R
//library(data.table)
//# For real use the following vectors will have lengths of up to 500,000,000 records
//pnr <- c("A","B","C","D","E")
//#         A   B   C   D   E   
//inn <- c(100,100,150,100,133)
//out <- c(200,150,200,133,167)
//event <- c(1,0,1,0,0,1)
//seq <- c(50,125,250,50,150)
//value <- c(0,0,0,5,5)
//mergevar <- 1:5

//temp <- splitDate(pnr,inn,out,event,mergevar,value,seq)
//setDT(temp)
//setkey(temp,pnr,inn,out)
//temp[]
//*/
