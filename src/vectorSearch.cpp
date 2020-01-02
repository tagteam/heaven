#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;
// [[Rcpp::export]]
List vectorSearch(std::vector<int> pnrnum,             // Vector of row numbers for searchCols
                  std::vector<std::string> searchCols, // Matrix with pnrnum and colums to search
                  std::vector<std::string> conditions, // Vector of inclusion conditions
                  std::vector<std::string> exclusions, // Vector of exclusion conditions
                  std::vector<std::string> condnames,  // Names of conditions - same length as conditions
                  std::vector<std::string> exclnames,  // Names of exclusions - same length as exclusions
                  int ni,                              // Number of inclusion criteria          
                  int ilength,                         // Number of inclusions in each block
                  int ne,                              // Number of exclusion criteria
                  int elength,                         // Number of exclusions in each block
                  int datarows,                        // Number of rows in searchCols
                  int match                            // 0=start 1=exact 2=end 3=contain
){ 
  // This function searches through searchCols for the the presence of conditions and exclusions
  // Output is produced when there a positive inclusion and no exclusion.
  // The output is a List with 2 columns:
  // - pnrnum - id of original record as integer
  // - condition - name of condition identified in conditions and not in exclusions
  
  // Define output vectors
  std::vector<int> Opnrnum;
  Opnrnum.reserve(datarows);
  std::vector<std::string> Ocondition;  
  Ocondition.reserve(datarows);
  // Define variables
  int innum=0; // Notes place in inclusion criteria
  int exnum=0; // Notes place in exclusion criteria
  int include=0; // Inclusions flag
  int exclude=0; // Exclusion flag
  int stopsearch=0; // Flag to finish search of a single value in searchCols
  // int condnames_length=condnames.size(); // Number of inclusion conditions
  // int exclnames_length=exclnames.size(); // Number of esclusion conditions
  //Rcout <<"Before outler loop"<<"\n";  
  for(int i=0; i<datarows; i++){ // Outer loop through searchCols
    if(searchCols[i].length()==0) continue; // Empty string to compare to
    for(int j=0; j<ni; j++){ // Loop through inclusion criteria blocks
      //Rcout <<"Loop j inclusion criteria blocks j="<<j<<"\n";
      for(int jj=0; jj<ilength;jj++){ // Loop through individual inclusion criteria
        //Rcout<<"Loop jj jj="<<jj;
        innum=j*ilength+jj;
        //Rcout <<" innum="<<innum<<"\n";
        if (conditions[innum].size()==0) break; // end of real inclusion criteria in list
        include=0; // No inclusion found - yet for that i
        exclude=0; // No exclusion found - yet
        stopsearch=0; 
        switch (match){
        case (0):
          if (searchCols[i].size()>=conditions[innum].size() && searchCols[i].find(conditions[innum])==0) include=1;
          break;
        case (1):  
          if (searchCols[i].size()==conditions[innum].size() && searchCols[i].find(conditions[innum])==0 && searchCols[i].size()==conditions[innum].size()) include=1;
          break;
        case (2):
          if (searchCols[i].size()>=conditions[innum].size() && searchCols[i].rfind(conditions[innum])==conditions[i].size()-searchCols[i].size()) include=1;
          break;
        case (3):
          if (searchCols[i].size()>=conditions[innum].size() && searchCols[i].rfind(conditions[innum])<=searchCols[i].size()) include=1;
          break;
        }
        if (include==1 && ne>0){
          include=1; // prepare to include - an inclusion string found
          for(int k=0; k<ne; k++){ // loop though exclusion blocks
            //Rcout<<"Loop k exclusions blocks k="<<k;
            exnum=k*elength; // start of exclusion block
            //Rcout<<" exnum="<<exnum<<" condames[innum]="<<condnames[innum]<<" exclnames[exnum]="<<exclnames[exnum]<<"\n";
            if(condnames[innum]!=exclnames[exnum]) continue; //Exclusion does not match inclusion name
            //Rcout<<" Match, no continue"<<"\n";
            stopsearch=1; // Exclusion criteium matchin inclusion criterium found - stop searching since only one exclusion list for each inclusion
            for(int kk=0; kk<elength; kk++){ // Loop though individual exclusion criteria
              //Rcout<<"Loop kk exclusions blocks kk="<<kk;
              exnum=k*elength+kk;
              if(exclusions[exnum].size()==0) break; // no more real criteria in list
              //Rcout<<" exnum="<<exnum<<"\n";
              switch(match){
              case(0): 
                if (searchCols[i].size()>=exclusions[exnum].size() && searchCols[i].find(exclusions[exnum])==0) exclude=1;
                break;
              case(1):  
                if (searchCols[i].size()==exclusions[exnum].size() && searchCols[i].find(exclusions[exnum])==0 && searchCols[i].size()==exclusions[exnum].size()) exclude=1;
                break;
              case(2):  
                if (searchCols[i].size()>=exclusions[exnum].size() && searchCols[i].rfind(exclusions[exnum])==exclusions[i].size()-searchCols[i].size()) exclude=1;
                break;
              case(3):  
                if (searchCols[i].size()>=exclusions[exnum].size() && searchCols[i].rfind(exclusions[exnum])<=searchCols[i].size())exclude=1; 
                break;
              }
              if (exclude==1) break; 
            }  // end loop through individual exclusion criteria
            if (stopsearch==1 || exclude==1) break;  
          } // end loop through exclusion block
        }// End inclusion match identified
        if (include==1 && exclude==0){
          Opnrnum.push_back(pnrnum[i]);
          Ocondition.push_back(condnames[innum]);
          break;
        }  
      } // End individual inclusion critertia
    } // End inclusion blocks  
  } // end element of search cols
  return (Rcpp::List::create(Rcpp::Named("pnrnum") = Opnrnum,
                             Rcpp::Named("X")=Ocondition));  
}
// /*** R
// library(data.table)
// # For real use the following vectors will have lengths of up to 500,000,000 records
// pnrnum <- c(1,2,3,4)
// pnrnum <-c(pnrnum,pnrnum)
// #         1   2   2   3   3   3   4   4   5   6
// c1 <- c("DI219","DI187","DI309","DI228")
// 
// searchCols <- c1
// 
// co1 <- c("I2")
// co2 <- c("I3")
// conditions <-as.vector(as.matrix(data.table(co1,co2)))
// 
// # ex1 <- c("DI228")
// # exclusions <-as.vector(as.matrix(data.table(ex1)))
// exclusions <- ""
// 
// condnames <- c("co1","co2")
// exclnames <- c("co1")
// 
// clength <- 2
// elength <- 0
// datarows <- 4
// 
// 
// 
// 
// out <- vectorSearch(pnrnum,searchCols,conditions,exclusions,condnames,exclnames,ni=3,ilength=2,ne=1,elength=1,datarows=length(searchCols),match=3)
// 
// out[]
// */

