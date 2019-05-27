#include <Rcpp.h>
#include <vector>
#include <string>
#include <random>
#include <algorithm>
using namespace Rcpp;
// [[Rcpp::export]]
List Matcher(int Ncontrols, // Desired number of controls
             int Tcontrols, // Total number of controls
             int Ncases,    // Total number of cases
             int reuseControls,  // Integer logical 0/1
             int expWindow, // Fixed required exposure window between case index and start
             IntegerVector startDate, // Startdate of condition defining exposure window
             IntegerVector controlIndex, // Controls dates for risk-set
             IntegerVector caseIndex, // Cases dates for risk-set
             IntegerVector controls, // controls ID
             IntegerVector cases, // cases ID
             int Ndateterms, // number of dateterm variables - zero=0
             IntegerMatrix datescases, // case dates 
             IntegerMatrix datescontrols, // control dates   
             int NoIndex,
             int seed){ // Ignore index if 1 - match without regard to risk-set - essentially simple match 
  int ii; // while counter - number of selected controls
  int controlCounter=0;// Sequencer through list of controls - controlIndex
  bool IsCoEl;  // preliminary test of selectability
  std::vector<int> selectedControls; // output controls
  selectedControls.reserve(Tcontrols*(Ncases+1)); // avoid repeated copying of vector
  std::vector<int> selectedCases;    // output cases-list
  selectedCases.reserve(Ncontrols*(Ncases+1));
  std::vector<int> haveTried(Tcontrols+1,0); // Have tried and failed - or is-taken if 1
  std::vector<int> usedControls(Tcontrols+1,0);  // Have already been used
  for (int i=0; i<Ncases; i++){ // Loop through each case to identify controls
    if (Ncases<1 || Tcontrols<1) break; // allows for groups to have no controls or no cases
    if (reuseControls==1){ // New shufling for each case
      controlCounter=0;
      seed+=1;
      std::mt19937 eng1(seed);
      std::shuffle(controlIndex.begin(), controlIndex.end(), eng1);
      std::mt19937 eng2 = eng1;
      std::shuffle(controls.begin(), controls.end(), eng2);
    }
    for (int j=0; j<Tcontrols; j++) haveTried[j]=0; //Initialize to "have not tried"
    ii=1; // while counter - number of selected cases for a control
    while (ii<=Ncontrols){
      //Is control eligible - variable
      if (NoIndex==1) IsCoEl=controlCounter<Tcontrols;  // Simply end of controls has not been reached
      else  // comnparison of case index and control index
        IsCoEl=  controlCounter<Tcontrols && (controlIndex[controlCounter]<-2000000000 || caseIndex[i]<-2000000000 || 
          caseIndex[i]<controlIndex[controlCounter]);// End has not been reached && (no dates to compare || case-date prior to controldate) 
      if(IsCoEl && Ndateterms>0) // Additional logic with time varying conditions to be before case date
        for (int k=0; k<Ndateterms; k++){ 
          IsCoEl= IsCoEl && ((datescases(i,k)<-2000000000 && (datescontrols(controlCounter,k)<-2000000000 || datescontrols(controlCounter,k)> caseIndex[i])) || // Both missing or case missing and control greater than caseIndex
            (datescontrols(controlCounter,k)>caseIndex[i] && datescases(i,k)> caseIndex[i]) || // Both beyond caseIndex
            (datescases(i,k)>-2000000000 && datescontrols(controlCounter,k)>-2000000000 && datescases(i,k)<=caseIndex[i] && datescontrols(controlCounter,k)<=caseIndex[i])); //both before caseIndex
        }
        if(IsCoEl && expWindow>0){
          IsCoEl= IsCoEl && ((caseIndex[i]-startDate[controlCounter])>expWindow);
        } 
        // case date missing or controldate missing or contraldate>casedate for all covariates
        if ((reuseControls==0 && usedControls[controlCounter]==0 && haveTried[controlCounter]==0 && IsCoEl) 
              || (reuseControls==1 && haveTried[controlCounter]==0 && IsCoEl )) { //foundOne
          selectedControls.push_back(controls[controlCounter]); //next selected control
          selectedCases.push_back(cases[i]); //case-id for that control
          ii+=1; // next control to be selected
          usedControls[controlCounter] = 1;
          haveTried[controlCounter] = 1;
          controlCounter +=1;
          if (controlCounter==Tcontrols-1) {//Reached end of controls - check for missed oportunities 
            for (int iii=0; iii<=Tcontrols; iii++){
              if ((haveTried[iii]==0 && reuseControls==1) | (usedControls[iii]==0 && haveTried[iii]==0 && reuseControls==0)) { //foundOne
                controlCounter = iii;
                break;
              }   
            }
          }
        }  
        else { // Action for skipped control
          if (controlCounter < Tcontrols) haveTried[controlCounter]=1;
          if (reuseControls==0) {
            for (int iii=controlCounter; iii<=Tcontrols; iii++){
              if ((haveTried[iii]==0 && usedControls[iii]==0) || iii==Tcontrols) { //foundOne or end
                controlCounter = iii;
                break;
              }
            }
            if (controlCounter==Tcontrols){// no find - start from firstUnused
              for (int iii=0; iii<=Tcontrols; iii++){
                if ((haveTried[iii]==0 && usedControls[iii]==0) || iii==Tcontrols) { //foundOne or end
                  controlCounter = iii;
                  break;
                }
              }
            }
          } // End !reuseControl section  
          else { //do reuse controls
            for (int iii=controlCounter; iii<=Tcontrols; iii++){
              if (haveTried[iii]==0){ //found one
                controlCounter = iii;
                break;
              }
            }  
            if (controlCounter==Tcontrols){
              for (int iii=0; iii<=Tcontrols; iii++){
                if (haveTried[iii]==0){ //found one
                  controlCounter = iii;
                  break;
                }
              }
            }
          } // End reuse Control section  
        } // End Skipped control     
        if (controlCounter>=Tcontrols) break; // No available controls for case  
    } // end while
  }// End loop for each case  
  return List::create(Named("selectedCases") = selectedCases,
                      Named("selectedControls") = selectedControls);
}  

// /*** R
// 
// Ncontrols <- 3 # desired number of controls
// Tcontrols <- 20 # total number of controls
// Ncases <- 5 #Total number of cases
// reuseControls <- 1 # 0 do not, 1 do reuse
// controlIndex <- c(rep(0,5),rep(5,5),rep(10,5),rep(15,5)) #control dates
// caseIndex <- c(0,4,9,NA,10) # case dates
// controls <- c(paste0('A',1:5),paste0('B',1:5),paste0('C',1:5),paste0('D',1:5)) #control ids
// controls <- as.integer(factor(controls))
// cases <- LETTERS[1:5] #case ids
// cases <- as.integer(factor(cases))
// NoIndex <- 0
// dcases <-matrix(c(rep(NA,5),8:12,12:16), nrow=5, ncol=3)
// #dcontrols <-matrix(c(seq(2,40,2),seq(3,41,2),seq(4,42,2)), nrow=20, ncol=3)
// dcontrols <- matrix(c(rep(4,60)), nrow=20, ncol=3)
// expWindow=0
// startDate=0
// 
// 
// temp <- Matcher(Ncontrols,Tcontrols,Ncases,reuseControls,expWindow,startDate,
//                         controlIndex, caseIndex,
//                          controls,  cases,3,dcases,dcontrols,NoIndex,2)
//  library(data.table)
//   setDT(temp)[]
// 
// */


