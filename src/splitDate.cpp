#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
// [[Rcpp::export]]
List splitDate(IntegerVector inn, // Starttimes - base data
               IntegerVector out, // Endtimes - base data
               IntegerVector event, // Event at end of interval 0/1 - base data
               IntegerVector mergevar, // Merge variable, multiple records can have same pnr - base data
               IntegerVector seq // Vector of date values to split by
) 
{ 
  // This function is intended for creating split data for lexis survival analyses. It handles
  // splitting by a defined sequence of dates from R - age, calender time, time since a key event etc.
  // The sequence number is expected to be zero prior to first split.  The variable .
  
  // Define output vectors
  
  std::vector<int> Omergevar;  
  Omergevar.reserve(mergevar.size()*seq.size());
  std::vector<int> Oinn;  // Starttimes output
  Oinn.reserve(mergevar.size()*seq.size());
  std::vector<int> Oout; // Endtimes output
  Oout.reserve(mergevar.size()*seq.size());
  std::vector<int> Oevent; // Event at end 0/1
  Oevent.reserve(mergevar.size()*seq.size());
  std::vector<int> Ovalue; // Value for output 0.1,2...
  Ovalue.reserve(mergevar.size()*seq.size());
  
  for (int i=0; i<mergevar.size(); i++){ // Loop along base data
    int seq_num=0; // Number in seq of the first record to create for each record in input
    for (int ii=0; ii<seq.size(); ii++){// Create records
      seq_num++;// next seq starts with one
      if(seq(ii)>=out(i)){//Not reached seq
        Omergevar.push_back(mergevar(i));
        Oinn.push_back(inn(i));
        Ovalue.push_back(seq_num-1);
        Oout.push_back(out(i));
        Oevent.push_back(event(i));
        break; //Done with base record
      }   
      else
        if(inn(i)>seq(ii) && ii==(seq.size()-1)){//past seq AND last seq
          Omergevar.push_back(mergevar(i));
          Oinn.push_back(inn(i));
          Ovalue.push_back(seq_num); // final seq-value
          Oout.push_back(out(i));
          Oevent.push_back(event(i)); 
          break;
        } 
        else
          if(inn(i)<seq(ii) && out(i)>seq(ii)){ //split situation - duration at least 1 day
            Omergevar.push_back(mergevar(i));
            Oinn.push_back(inn(i));
            Ovalue.push_back(seq_num-1); //value prior to seq
            Oout.push_back(seq(ii));
            Oevent.push_back(0); // no event
            // and reset start of base record
            inn(i)=seq(ii);
            if(ii==seq.size()-1){
              Omergevar.push_back(mergevar(i));
              Oinn.push_back(inn(i));
              Ovalue.push_back(seq_num); 
              Oout.push_back(out(i));
              Oevent.push_back(event(i));
              break;
            }  
          }
          else
            if(out(i)==seq(ii) && event(i)==1){ // Also split with zero record in case of event 
              Omergevar.push_back(mergevar(i));
              Oinn.push_back(inn(i));
              Ovalue.push_back(seq_num-1); //value prior to seq
              Oout.push_back(seq(ii));
              Oevent.push_back(0); // no event
              // and reset start of base record
              inn(i)=seq(ii);
              if(ii==seq.size()-1){
                Omergevar.push_back(mergevar(i));
                Oinn.push_back(inn(i));
                Ovalue.push_back(seq_num); 
                Oout.push_back(out(i));
                Oevent.push_back(event(i)); 
                break;
              }
            } 
    }  //end seq-loop 
  } // end base-loop
  return (Rcpp::List::create(Rcpp::Named("pnrnum")=Omergevar, 
                             Rcpp::Named("inn") = Oinn,
                             Rcpp::Named("out") = Oout,
                             Rcpp::Named("event") = Oevent,
                             Rcpp::Named("value") = Ovalue));  
}



// /*** R
// library(data.table)
// # For real use the following vectors will have lengths of up to 500,000,000 records
// pnr <- c("A","B","C","D","E")
// #         A   B   C   D   E   
// inn <- c(100,100,150,25,400)
// out <- c(200,150,200,50,500)
// event <- c(1,0,1,0,0,1)
// seq <- c(50,125)  #,150,200,250)
// value <- c(0,0,0,5,5)
// mergevar <- 1:5
// 
// temp <- splitDate(inn,out,event,mergevar,seq)
// setDT(temp)
// setkey(temp,pnrnum,inn,out)
// temp[]
// */
