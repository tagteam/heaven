#include <Rcpp.h>
#include <string>
#include <vector>
#include <regexp>
using namespace Rcpp;
// [[Rcpp::export]]
List matrixSearch(IntegerVector pnrnum,       // Vector of row numbers for searchCols
                  CharacterMatrix searchCols, // Matrix with pnrnum and colums to search
                  CharacterMatrix conditions, // Matrix of inclusion conditions
                  CharacterMatrix exclusions, // Matrix of exclusion conditions
                  CharacterVector condnames,  // Names of conditions
                  CharacterVector exclnames,  // Names of exclusions
                  int clength,                // Max number of inclusions
                  int elength,                // Max number of exclusions
                  int datarows                // Number of rows in searchCols
){ 
  // This function searches through searchCols for the regular
  // expressions in conditions and exclusions. 
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
  int jj=0; // loops through regex inclusion expressions
  int kk=0; // Loops through regex exclusion expressions
  int include=0; // Inclusions flag
  int exclude=0; // Exclusion flag
  int Onum=0; // Tracks rows created in output
  int stopsearch=0; // Flag to finish search of a single value in searchCols
  int condnames_length=condnames.length(); // Number of inclusion conditions
  int exclnames_length=exclnames.length(); // Number of esclusion conditions
  int breakeexclusion=0; // Breaks going through exclusions
  
  for(int i=0; i<datarows; i++){ // Loop through searchCols
    for(int ii=0; ii<clength; ii++){ // Loop through columns in which to search
      include=0; // No inclusion found - yet
      exclude=0; // No exclusion found - yet
      if (searchCols(i,ii)=="" || (Onum>0 &&  Opnrnum(Onum)==pnrnum(i) && Ocondition(Onum)==searchCols(i,ii))) continue; // Missing or already found
      for(int j=0; j<condnames_length; j++){ // Loop through inclusion list
        jj=0;
        while (jj<clength && conditions(j,jj) != ""){
          if (regex_match(searchCols(i,ii),conditions(j,jj))){ // Match found
            include=1;
            for(int k=0,k<exclnames_length; k++){ // Loope through exclusion list
              if(condnames(j)==exclnames(k)){ // Found an exclusion list
                kk=0;
                stopsearch=1; // One found no reason to scan rest of list
                while(kk<elength && exclusions(k,kk) != ""){
                  if (regex_match(searchCols(i,ii),exclusions(k,kk))){ // Exclusion found
                    exclude=1;
                    stopsearch=1;
                  } // Single exclusion look up
                  if(exclude==1 ) break;
                  kk+=1;
                } // list of exclusions loop
              } // Found an exclusion inclusion match
              if (stopsearch==1 || exclude==1) break;
            } // Loope through exclusions list
          } // Found an inclusion match
          if(include==1 || exclude==1) break;
          jj+=1;
        } // Loop through one inclusion list
       } // Loop through list of inclusions
      if (include==1 && exclude==0){
        Onum+=1;
        Opnrnum.pushback(searchCols(i,0));
        Ocondition.pushback(inclusions(i,ii));
      }  
    } // End list of columns to look in
  }
  return (Rcpp::List::create(Rcpp::Named("pnrnum") = Opnrnum,
                             Rcpp::Named("X")=Ocondition);  
}



/*** R
library(data.table)
# For real use the following vectors will have lengths of up to 500,000,000 records
pnrnum <- c(1,2,2,3,3,3,4,4,5,6)
#         1   2   2   3   3   3   4   4   5   6
c1 <- c("1","1","1","2","3","4","1","2","9","9")
c2 <- c("10","1","1","20","30","40","10","20","90","9")
searchCols <- as.matrix(data.table(c1,c2))

co1 <- c("1","")
co2 <- c("3","30")
co3 <- c("4","40")
conditions <-as.matrix(data.table(co1,co2,co3)) 

ex1 <- c("10")
exclusions <-as.matrix(data.table(ex1))

condnames <- c("co1","co2","co3")
exclnames <- c("co1")

clength <- 2
elength <- 1
datarows <- 10

out <- matrixsearch(pnrnum,searchCols,conditions,exclusions,condnames,exclnames,clength,elength,datarows)

out[]
*/
  
