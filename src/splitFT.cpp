#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
//' @description Split by intervals of dates
//' @title splitFT
//' @author Christian Torp-Pedersen
//' @export
// [[Rcpp::export]]
List splitFT(IntegerVector pnrnum, // PNR as sequence number - base data
             IntegerVector inn, // Starttimes - base data
             IntegerVector out, // Endtimes - base data
             IntegerVector event, // Event at end of interval 0/1 - base data
             IntegerVector mergevar, // Merge variable, multiple records can have same pnr - base data
             IntegerVector Spnrnum, // Sequence number of pnr in split guide
             std::vector<std::string> val, // Value of name to provide to output for interval - split guide
             IntegerVector start, // Interval start - split guide
             IntegerVector end // Interval end - split guide
) { 
// This function is intended for creating split data for lexis survival analyses. It handles
// splitting by a set of splitting guide vectors which contains (Spnrnum,start,end,val) 
// - to be used for sequential splitting of base data (pnrnum,mergevar,inn,out,event).
// There are 2 ids because data might have been pre-split - this i handled in the R program.
// The program has this flow:
// The function iterates through consecutive records in the base data.  When a new pnrnum is
// found it starts building vectors of matching base data and splitting guide from ONE pnrnum.
// When the vectors have been bild for ONE pnrnum the splitting is conducted.
// When the splitting is ended the resulting vectors are copied to output vectors;

  // Define output vectors
  std::vector<int> Opnrnum;
  Opnrnum.reserve(pnrnum.size()*2);
  std::vector<int> Omergevar;  
  Omergevar.reserve(pnrnum.size()*2);
  std::vector<int> Oinn;  // Starttimes output
  Oinn.reserve(pnrnum.size()*2);
  std::vector<int> Oout; // Endtimes output
  Oout.reserve(pnrnum.size()*2);
  std::vector<int> Oevent; // Event at end 0/1
  Oevent.reserve(pnrnum.size()*2);
  std::vector<std::string> Oval; // output value
  Oval.reserve(pnrnum.size()*2);
  
  unsigned int l1=pnrnum.size(); // length of base data
  unsigned int l2=Spnrnum.size(); // length of split guide
  unsigned int counter=0; // counts through split guide
  int START=0; //marks start of splitting guide to pnr-range
  int SLUT=0; // End of splitting guide for pnr-range
  int INN=-1; // start of pnr-range in output data
  int OUT=-1; // end of pnr-range in output data
  int noSplit=0; // There is a splitting guide to split by for a given id!
  int FIRST=0; // First in pnr sequence
  int OUText=0; // Lengthening of out from splitting
  // Loop through all data in base data and split when a sequence of identical pnr-s have
  // been identified.  The base data are added to output vectors sequentially and split
  // prior to adding next pnr
  for(unsigned int i=0; i<l1; i++){ // Loop through base data
    if (i<1 || (i!=0 && pnrnum(i) != pnrnum(i-1))){ // new pnr identified
      noSplit=0; // Splitting guide expected as default
      INN=OUT+1;  // Start of next pnr-sequence except for firt record
      FIRST=1;
      while (Spnrnum(counter)<pnrnum(i) && counter<l2-2) counter+=1;
      //Searching in splitting guide until match found or passed
      if (Spnrnum(counter)>pnrnum(i) || Spnrnum(counter)<pnrnum(i)) {noSplit=1;
      //passed or reached end without splitting guide
      }
      else {
        if (Spnrnum(counter)==pnrnum(i)){ //Found one, or some
          // start of matchin pnrs
          START=counter;
          while(Spnrnum(counter)==pnrnum(i)){ //Going through list of matches
            counter+=1; 
            SLUT=counter;
            if (counter>=l2-1){
              SLUT=l2-1;
              break; // Reached end;  
            } 
            else
              if (Spnrnum(counter) != pnrnum(i)){  //Now just passed;
                counter-=1;
                SLUT=counter;
                break;
              }
          } // end while pnr equal
        } //end  Spnrnum==pnrnum
      } // end found one - or some
    } // end new pnr - START, SLUT, INN, OUT have been identified
    if (FIRST==1 || (i !=0 && pnrnum(i)==pnrnum[i-1])){ // in series of identical pnr-s
      // collect pnr-series
      OUT+=1;
      Opnrnum.push_back(pnrnum(i));
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(inn(i));
      Oout.push_back(out(i));
      Oevent.push_back(event(i));
      Oval.push_back("");
      if (FIRST==1) FIRST -=1;
    } 
    if (i==l1-1 || (i<l1-1 && pnrnum(i) != pnrnum(i+1))){
    // End consecutive pnrnumbers - start splitting
      if (noSplit==1){ //Nothing to split
      }
      else{ // Something to split
        for (int ii=START; ii<=SLUT; ii++){ // Outer loop - splitting guide
          OUT=OUT+OUText; // Extension of OUT from splitting by prior split guide record;
          INN=INN+OUText;
          OUText=0; // reset;
          for (int iii=INN; iii<=OUT; iii++){ // innter loop - base data
            if (start(ii)>Oout[iii] || Oinn[iii]>end(ii)) continue; // no overlap, no action
            else
            if (start(ii)<=Oinn[iii] && end(ii)>=Oout[iii]) Oval[iii]=val[ii]; // surrounded 
            else
            if (start(ii)<=Oinn[iii] && end(ii)>=Oout[iii] && Oout[iii]==Oinn[iii])
              Oval[iii]=val[ii]; // Record length zero
            else  
            if (end(ii)>=Oinn[iii] && end(ii) <= Oout[iii] && start(ii)<=Oinn[iii]){ 
              // Split in 2 records - left overlap
              // New record start at end and ends with original end
              OUText +=1;
              Oinn.push_back(end(ii));  
              Oout.push_back(Oout[iii]);
              Oevent.push_back(Oevent[iii]);
              Opnrnum.push_back(Opnrnum[iii]);
              Omergevar.push_back(Omergevar[iii]);
              Oval.push_back("");
              Oout[iii]=end(ii); // Original record ends with interval end  
              Oval[iii]=val[ii]; // and gets the value from the interval
              Oevent[iii]=0;
            } 
            else
            if (end(ii)>=Oout[iii] && Oinn[iii]<=start(ii) && start(ii)<=Oout[iii]){ 
            // split in 2 records - right overlap
            // New record start at start and ends with original end
              OUText +=1;
              Oinn.push_back(start(ii));  
              Oout.push_back(Oout[iii]);
              Oevent.push_back(Oevent[iii]);
              Opnrnum.push_back(Opnrnum[iii]);
              Omergevar.push_back(Omergevar[iii]);
              Oval.push_back(val[ii]);
              Oout[iii]=start(ii); // Original record ends with interval end  
              Oevent[iii]=0;
              Oval[iii]=""; // and gets the value from the interval           
            } else
            if(end(ii)>=Oinn[iii] && end(ii)<=Oout[iii] && start(ii)>=Oinn[iii] 
                  && start(ii)<=Oout[iii]){ 
              //Complete overlap - 3 records
              OUText +=2;
              Oinn.push_back(Oinn[iii]);  // New record start at end and ends with original end
              Oout.push_back(start(ii));
              Oevent.push_back(0);
              Opnrnum.push_back(Opnrnum[iii]);
              Omergevar.push_back(Omergevar[iii]);
              Oval.push_back("");
              Oinn.push_back(end(ii));  // New record start at inn and ends with Tstart(ii) end
              Oout.push_back(Oout[iii]);
              Oevent.push_back(Oevent[iii]);
              Opnrnum.push_back(Opnrnum[iii]);
              Omergevar.push_back(Omergevar[iii]);
              Oval.push_back("");
              Oout[iii]=end(ii); // Original record is middle interval 
              Oinn[iii]=start(ii);
              Oval[iii]=val[ii]; // and gets the value from the interval
              Oevent[iii]=0;  
            }
          }// end iii-loop - base data
        } // End ii-loop - splitting guide
      } // end else-something to split
    } // end consecutive pnr 
  } // End i-for loop
//Rcout << "Create return" << "\n";    
  return (Rcpp::List::create(Rcpp::Named("pnrnum") = Opnrnum,
                            Rcpp::Named("merge")=Omergevar, 
                            Rcpp::Named("inn") = Oinn,
                            Rcpp::Named("out") = Oout,
                            Rcpp::Named("event") = Oevent,
                            Rcpp::Named("val") = Oval));  
}
    


///*** R
//library(data.table)
//# For real use the following vectors will have lengths of up to 500,000,000 records
//pnrnum <- c(1,2,2,3,3,3,4,4,5,6)
//#         1   2   2   3   3   3   4   4   5   6
//inn <- c(100,100,150,100,133,167,100,150,100,100)
//out <- c(200,150,200,133,167,200,150,200,200,200)
//event <- c(1,0,0,0,0,1,0,1,1,1)
//mergevar <- 1:10
//
//#Spnrnum <- c(1,2,2,3,4,4,5,5,5,6) # 
//val <- as.character(1:10)
//#           1   2   2    3   4   4   5   5   5   6
//#start <- c(190, 90,189,132,100,200,110,120,130,90)
//#end <-  c( 210,110,210,166,100,200,115,130,210,210)
//#Spnrnum <- c(5,5,5,5)
//#val <- c("ost","kaffe","brød","forårsrulle")
//#start <- c(20,50,150,180)
//#end <- c(30,150,180,199)
//Spnrnum <- c(1  , 1,  1,  2,  2,  2,  3,  3,  3,  6)
//start <- c(  50,  110,199,125,126,250,100,133,166,98)
//end <-   c(  101, 188,218,125,150,170,133,166,167,250)


//temp <- splitFT(pnrnum,inn,out,event,mergevar,Spnrnum,val,start,end)
//setDT(temp)
//setkey(temp,pnrnum,inn,out)
//temp[]
//*/
