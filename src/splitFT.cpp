#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
// [[Rcpp::export]]
List splitFT(IntegerVector pnrnum, // PNR as sequence number - base data
              NumericVector inn, // Starttimes - base data
              NumericVector out, // Endtimes - base data
              IntegerVector event, // Event at end of interval 0/1 - base data
              IntegerVector mergevar, // Merge variable, multiple records can have same pnr - base data
              IntegerVector Spnrnum, // Sequence number of pnr in split guide
              std::vector<std::string> val, // Value of name to provide to output for interval - split guide
              NumericVector start, // Interval start - split guide
              NumericVector end, // Interval end - split guide
              IntegerVector num, //Covariate number
              int numcov, // Number of covariate to split by
              String default_ // Default value when no value is assigned
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
  std::vector<double> Oinn;  // Starttimes output
  Oinn.reserve(pnrnum.size()*2);
  std::vector<double> Oout; // Endtimes output
  Oout.reserve(pnrnum.size()*2);
  std::vector<int> Oevent; // Event at end 0/1
  Oevent.reserve(pnrnum.size()*2);
  std::vector<std::vector<std::string> > Oval; // Matrix values according to splitting covariates - organised by number
  Oval.resize(numcov); // One vector for each splitparameter
  for(int i=0; i<numcov;i++)Oval[i].reserve(pnrnum.size()*2);
  
  int l1=pnrnum.size(); // length of base data
  int l2=Spnrnum.size(); // length of split guide
  int counter=0; // counts through split guide
  int START=0; //marks start of splitting guide to pnr-range
  int SLUT=0; // End of splitting guide for pnr-range
  int INN=0; // start of pnr-range in output data
  int OUT=-1; // end of pnr-range in output data
  int noSplit=0; // There is a splitting guide to split by for a given id!
  int OUText=0; // Lengthening of out from splitting
  // Loop through all data in base data and split when a sequence of identical pnr-s have
  // been identified.  The base data are added to output vectors sequentially and split
  // prior to adding next pnr
  for(int i=0; i<l1; i++){ // Loop through base data 
    if (i<1 || (i!=0 && pnrnum(i) != pnrnum(i-1))){ // new pnr identified
      noSplit=0; // Splitting guide expected as default
      while (counter<(l2-1) && Spnrnum(counter)<pnrnum(i)) counter+=1;
      //Searching in splitting guide until match found or passed - counter maintains place in splitting guide
      if (Spnrnum(counter)>pnrnum(i) || Spnrnum(counter)<pnrnum(i)) {noSplit=1;
        //passed or reached end without splitting guide
      }
      else {
        if (Spnrnum(counter)==pnrnum(i)){ //Found one, or some
          // start of matching pnrs
          START=counter; // First identified record in splitting guide
          SLUT=counter; // If only 1 is found!
          while(counter<l2-1 && Spnrnum(counter+1)==pnrnum(i)){ //Going through list of matches
            counter+=1; 
            SLUT=counter; // Last record in splitting guide
            if (counter>=l2-1){
              SLUT=l2-1; 
              break; // Reached end;  
            } 
          } // end while pnr equal
        } //end  Spnrnum==pnrnum - identical pnr in base data and splitting guide
      } // end found one - or some
    } // end new pnr - START, SLUT have been identified - INN/OUT yet to be determined
      // Start always by pushing the original record and then add new records - and adjust original record
      // This "push" takes place once per record in base data prior to split or no-split
      //std::cout<< "Before first out"<<" pnrnum(i)="<<pnrnum(i)<<" inn(i)="<<inn(i)<<" out(i)="<<out(i) <<" nosplit="<<noSplit<<"\n";    
      INN=OUT+1;  // Start of next pnr-sequence except for first record
      OUT=INN; // So far just one record
      Opnrnum.push_back(pnrnum(i));
      Omergevar.push_back(mergevar(i));
      Oinn.push_back(inn(i));
      Oout.push_back(out(i));
      Oevent.push_back(event(i));
      for(int k=0; k<numcov; k++) Oval[k].push_back(default_);
      if (noSplit==1){ //Nothing to split - at least potentially
      }
      else{ // Something to split
        for (int ii=START; ii<=SLUT; ii++){ // Outer loop - Start to end of splitting guide records identified - ii refers to splitting guide
          for (int iii=INN; iii<=OUT; iii++){ // inner loop - base data - just one record prior to split - iii refers to base records
            if (start(ii)>Oout[iii] || Oinn[iii]>end(ii)) {
              //std::cout<< "Action: no action" << "\n";
              continue; // no overlap, no action
            }
            else
              if (start(ii)<=Oinn[iii] && end(ii)>=Oout[iii]) {
                //std::cout<<"Action Surrounded"<<"\n";
                // surrounded
                for(int k=0; k<numcov;k++)
                  if(k==num(ii)-1)Oval[k][iii]=val[ii];
                  else Oval[k][iii]=Oval[k][iii];  // previous value
                //std::cout<<"iii="<<iii<<"ii="<<ii<<" num(ii)="<<num(ii)<< " Oval[1,2,3][iii]="<<Oval[1][iii]<<Oval[2][iii]<<Oval[2][iii]<< "\n";
              }
            else
              if (end(ii)>=Oinn[iii] && end(ii) <= Oout[iii] && start(ii)<=Oinn[iii]){
                if(Oinn[iii]==end(ii)){
                  // Only overlap at Oinn=start - no action
                }
              else{
                //std::cout<<"Action left overlap"<<"\n";
                // Split in 2 records - left overlap
                // New record start at end and ends with original end
                OUText +=1; // Extension of length of base records
                Oinn.push_back(end(ii));
                Oout.push_back(Oout[iii]);
                Oevent.push_back(Oevent[iii]);
                Opnrnum.push_back(Opnrnum[iii]);
                Omergevar.push_back(Omergevar[iii]);
                for(int k=0; k<numcov;k++)Oval[k].push_back(Oval[k][iii]);  // previous value
                Oout[iii]=end(ii); // Original record ends with interval end
                Oval[num(ii)-1][iii]=val[ii]; // and gets the value from the interval
                Oevent[iii]=0;
                //std::cout<< "iii=" <<iii<<" ii="<< ii << " Oval[iii]="<< Oval[iii] << "\n";
                }
              }
              else
                if (end(ii)>=Oout[iii] && Oinn[iii]<=start(ii) && start(ii)<=Oout[iii]){
                  if (Oout[iii]==start(ii) && Oevent[iii]==0){
                    // Only overlap at Oout=start and no event - no action
                    //std::cout<<"Action right overlap - marginal at end, no event, no action"<<"\n";
                  }
                  else{
                    //std::cout<<"Action Right overlap"<<"\n";
                    // split in 2 records - right overlap
                    // New record start at start and ends with original end
                    OUText +=1;
                    Oinn.push_back(start(ii));
                    Oout.push_back(Oout[iii]);
                    Oevent.push_back(Oevent[iii]);
                    Opnrnum.push_back(Opnrnum[iii]);
                    Omergevar.push_back(Omergevar[iii]);
                    for(int k=0; k<numcov;k++)
                      if(k==num(ii)-1)Oval[k].push_back(val[ii]);
                      else Oval[k].push_back(Oval[k][iii]);  // previous value
                    Oout[iii]=start(ii); // Original record ends with interval end
                    Oevent[iii]=0;
                    //std::cout<< "iii=" <<iii<<" ii="<< ii << " Oval[iii]="<< Oval[iii] << "\n";
                    }
                  }
                  else
                    if(end(ii)>=Oinn[iii] && end(ii)<=Oout[iii] && start(ii)>=Oinn[iii]
                         && start(ii)<=Oout[iii]){
                      //std::cout<<"Action Complete overlap"<<"\n";
                      //Complete overlap - 3 records
                      OUText +=2;
                      Oinn.push_back(Oinn[iii]);  // New record start at end and ends with original end
                      Oout.push_back(start(ii));
                      Oevent.push_back(0);
                      Opnrnum.push_back(Opnrnum[iii]);
                      Omergevar.push_back(Omergevar[iii]);
                      for(int k=0; k<numcov;k++)Oval[k].push_back(Oval[k][iii]);
                      Oinn.push_back(end(ii));  // New record start at inn and ends with Tstart(ii) end
                      Oout.push_back(Oout[iii]);
                      Oevent.push_back(Oevent[iii]);
                      Opnrnum.push_back(Opnrnum[iii]);
                      Omergevar.push_back(Omergevar[iii]);
                      for(int k=0; k<numcov;k++)Oval[k].push_back(Oval[k][iii]);
                      Oout[iii]=end(ii); // Original record is middle interval
                      Oinn[iii]=start(ii);
                      Oval[num(ii)-1][iii]=val[ii]; // and gets the value from the interval
                      Oevent[iii]=0;
                      //std::cout<< "iii=" <<iii<<" ii="<< ii << " Oval[iii]="<< Oval[iii] << "\n";
                    }
          }// end iii-loop - base data
          OUT=OUT+OUText; // After inner loop number of recrods have extended
          OUText=0; // reset 
        } // End ii-loop - splitting guide
      } // end else-something to split
  } // End i-for loop
  // //Rcout << "Create return" << "\n";  
  return (Rcpp::List::create(Rcpp::Named("pnrnum") = Opnrnum,
                             Rcpp::Named("mergevar")=Omergevar, 
                             Rcpp::Named("inn") = Oinn,
                             Rcpp::Named("out") = Oout,
                             Rcpp::Named("event") = Oevent,
                             Rcpp::Named("val") = Oval));  
}



// /*** R
// library(data.table)
// # For real use the following vectors will have lengths of up to 500,000,000 records
// pnrnum <- c(1,2,2,3,3,3,4,4,5,6)
// #         1   2   2   3   3   3   4   4   5   6
// inn <- as.Date(c(100,100,150,100,133,167,100,150,100,100),origin = '1970-01-01')
// out <- as.Date(c(200,150,200,133,167,200,150,200,200,200),origin = '1970-01-01')
// event <- c(1,0,0,0,0,1,0,1,1,1)
// mergevar <- 1:10
// 
// Spnrnum <- c(1,2,2,3,4,4,5,5,5,6) #
// val <- as.character(c(1,2,2,3,4,4,5,5,5,6))
// #           1   2   2    3   4   4   5   5   5   6
// start <- as.Date(c(110,  120,130,130, 90, 190,90,110,120, 199),origin = '1970-01-01')
// slut <-  as.Date(c(112,  140,150 ,180,100,210,210,190,180,200),origin = '1970-01-01') 
// num <-   c( 1,    2,   3,  1,  2,  3,  1,   2, 3,   1)
// OUT <- splitFT(pnrnum, 
//              inn, 
//              out, # Endtimes - base data
//              event, # Event at end of interval 0/1 - base data
//              mergevar, # Merge variable, multiple records can have same pnr - base data
//              Spnrnum, #Sequence number of pnr in split guide
//              val, # Value of name to provide to output for interval - split guide
//              start, # Interval start - split guide
//              slut, # Interval end - split guide
//              num, #Covariate number
//              3 # Number of covariate to split by
//             ) 
// OUT <- cbind(setDT(OUT[1:5]),setDT(do.call(cbind,OUT[6])))
// setkeyv(OUT,c("pnrnum","mergevar","inn"))
// OUT[]
// */
  
