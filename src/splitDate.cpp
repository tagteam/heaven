#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
//' @export
// [[Rcpp::export]]
List splitDate(NumericVector inn, // Starttimes - base data
               NumericVector out, // Endtimes - base data
               IntegerVector event, // Event at end of interval 0/1 - base data
               IntegerVector mergevar, // Merge variable, multiple records can have same pnr - base data
               IntegerVector seq, // Vector of date values to split by
               IntegerVector varname // Value to be added to each split date (such as birthdate)     
) 
{ 
  std::vector<int> Omergevar;  
  Omergevar.reserve(mergevar.size()*seq.size());
  std::vector<double> Oinn;  // Starttimes output
  Oinn.reserve(mergevar.size()*seq.size());
  std::vector<double> Oout; // Endtimes output
  Oout.reserve(mergevar.size()*seq.size());
  std::vector<int> Oevent; // Event at end 0/1
  Oevent.reserve(mergevar.size()*seq.size());
  std::vector<int> Ovalue; // Value for output 0.1,2...
  Ovalue.reserve(mergevar.size()*seq.size());
  std::vector<int> seq_plus(seq.size()); // seq+value for each case
  
  
  for (int i=0; i<mergevar.size(); i++){ // Loop along base data;
    int seq_num=0; // Number in seq of the first record to create for each record in input
    for (int j=0; j<seq.size(); j++)  seq_plus[j]=seq(j)+varname[i]; // Final vector to split by defined by sum of vector and varname
    for (int ii=0; ii<seq.size(); ii++){// Create records - loop through seq
      seq_num++;// next seq starts with one
      if(seq_plus[ii]>=out(i)){//Seq_plus values >= record, output record and break
        Omergevar.push_back(mergevar(i));
        Oinn.push_back(inn(i));
        Ovalue.push_back(seq_num-1);
        Oout.push_back(out(i));
        Oevent.push_back(event(i));
        break; //Done with base record
      }   
      else
        if(inn(i)>seq_plus[ii] && ii==(seq.size()-1)){//past seq AND last seq - output and break
          Omergevar.push_back(mergevar(i));
          Oinn.push_back(inn(i));
          Ovalue.push_back(seq_num); // final seq-value
          Oout.push_back(out(i));
          Oevent.push_back(event(i)); 
          break;
        } 
        else
          if(inn(i)<=seq_plus[ii] && out(i)>seq_plus[ii]){ //split situation - duration at least 1 day
            Omergevar.push_back(mergevar(i));
            Oinn.push_back(inn(i));
            Ovalue.push_back(seq_num-1); //value prior to seq
            Oout.push_back(seq_plus[ii]);
            Oevent.push_back(0); // no event
            // and reset start of base record ready for next value in seq_plus
            inn(i)=seq_plus[ii];
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
            if(out(i)==seq_plus[ii] && event(i)==1){ // Also split with zero record in case of event 
              Omergevar.push_back(mergevar(i));
              Oinn.push_back(inn(i));
              Ovalue.push_back(seq_num-1); //value prior to seq
              Oout.push_back(seq_plus[ii]);
              Oevent.push_back(0); // no event
              // and reset start of base record ready for next value in seq_plus
              inn(i)=seq_plus[ii];
              if(ii==seq.size()-1){ // output of last record
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
// seq <-  175  #c(50,125)  #,150,200,250)
// value <- c(0,0,0,5,5)
// mergevar <- 1:5
// varname <- 0
// 
// temp <- splitDate(inn,out,event,mergevar,seq,varname)
// setDT(temp)
// setkey(temp,pnrnum,inn,out)
// temp[]
// */
