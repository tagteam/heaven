#include <Rcpp.h>
#include <string>
#include <vector>
using namespace Rcpp;


// [[Rcpp::export]]
void  matrixSearch(IntegerVector pnrnum,       // Vector of row numbers for searchCols
                  std::vector<std::string> searchCols, // Matrix with pnrnum and colums to search
                  CharacterMatrix conditions, // Matrix of inclusion conditions
                  CharacterMatrix exclusions, // Matrix of exclusion conditions
                  CharacterVector condnames,  // Names of conditions
                  CharacterVector exclnames,  // Names of exclusions
                  int clength,                // Max number of inclusions
                  int elength,                // Max number of exclusions
                  int datarows                // Number of rows in searchCols
){ 
  for (int i=0; i<searchCols.size();i++){
    int ii=(searchCols[i]=="2");
    std::cout << ii<<" ";
  }

  //std::string var=searchCols(1,2);
  //std::cout << "res= "<<var<<"\n";
 //std::vector<std::string ost>="ostemad";
 //std::cout << "substr(pos)= "<< ost.find("ost") <<"\n";
// std::cout<<"substring "<<searchCols(1,1).substr(pos)<<"\n";
  

} 
  /*** R
  library(data.table)
  # For real use the following vectors will have lengths of up to 500,000,000 records
  pnrnum <- c(1,2,2,3,3,3,4,4,5,6)
  #         1   2   2   3   3   3   4   4   5   6
  c1 <- c("1","1","1","2","3","4","1","2","9","9")
  c2 <- c("10","1","1","20","30","40","10","20","90","9")
  searchCols <- c(c1,c2)
  
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
  
  out <- matrixSearch(pnrnum,searchCols,conditions,exclusions,condnames,exclnames,clength,elength,datarows)
  
  out[]
  */
  