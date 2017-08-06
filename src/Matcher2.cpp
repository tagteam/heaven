
#include <Rcpp.h>
#include <vector>
#include <string>
using namespace Rcpp;


// [[Rcpp::export]]


List Matcher(int Ncontrols, // Desired number of controls
             int Tcontrols, // Total number of controls
             int Ncases,    // Total number of cases
             int reuseControls,  // Integer logical 0/1
             IntegerVector controlIndex, // Controls dates for risk-set
             IntegerVector caseIndex, // Cases dates for risk-set
             std::vector<std::string> controls, // controls ID
             std::vector<std::string> cases, // cases ID
             int NoIndex){ // Ignore index if 1 - match without regard to risk-set - essentially simple match 

  int ii; // while counter - number of selected controls
  int controlCounter=0;// Sequencer through list of controls - controlIndex
  bool IsCoEl;  // preliminary test of selectability
  std::vector<std::string> selectedControls; // output controls
    selectedControls.reserve(Tcontrols*(Ncases+1)); // avoid repeated copying of vector
  std::vector<std::string> selectedCases;    // output cases-list
    selectedCases.reserve(Ncontrols*(Ncases+1));
  std::vector<int> haveTried(Tcontrols+1,0); // Have tried and failed - or is-taken if 1
  std::vector<int> usedControls(Tcontrols+1,0);  // Have already been used
  srand(216); //set seed;
  for (int i=0; i<Ncases; i++){
    if (Ncases<1 || Tcontrols<1) break; // allows for groups to have no controls or no cases
    for (int j=0; j<Tcontrols; j++) haveTried[j]=0; //Initialize to "have not tried"
    ii=1; // while counter - number of selected cases for a control
    while (ii<=Ncontrols){
      //Is control eligible - variable
      if (NoIndex==1) IsCoEl=controlCounter<Tcontrols;  // Simply end of controls has not been reached
      else  
      IsCoEl=  controlCounter<Tcontrols && (controlIndex[controlCounter]==NA || caseIndex[i]==NA || 
               caseIndex[i]<controlIndex[controlCounter]);
       // Above: End has not been reached && (no dates to compare || case-date prior to controldate) 
      if ((usedControls[controlCounter]==0 && IsCoEl) || (haveTried[controlCounter]==0 && IsCoEl)) { //foundOne
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
            // random decision to start with first or continue searching
            int flip=std::rand()%1; if (flip==1) flip=controlCounter;
            for (int iii=flip; iii<=Tcontrols; iii++){
              if ((haveTried[iii]==0 && usedControls[iii]==0) || iii==Tcontrols) { //foundOne or end
                controlCounter = iii;
                break;
              }
            }
            if (controlCounter==Tcontrols && flip !=0){// no find - start from firstUnused
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
//  std::vector<std::string>  out[selectedCases.size()][2];  
//  if (selectedControls.size()>=1)
//    { out(_,1)=selectedCases; 
//      out(_,0)=selectedControls;
//    }
return List::create(Named("selectedCases") = selectedCases,
                    Named("selectedControls") = selectedControls);
}  

/*** R

Ncontrols <- 3 # desired number of controls
Tcontrols <- 20 # total number of controls
Ncases <- 5 #Total number of cases
reuseControls <- 1 # 0 do not, 1 do reuse
controlIndex <- c(rep(0,5),rep(5,5),rep(10,5),rep(15,5)) #control dates
caseIndex <- c(0,4,9,14,10) # case dates
controls <- c(paste0('A',1:5),paste0('B',1:5),paste0('C',1:5),paste0('D',1:5)) #control ids
cases <- LETTERS[1:5] #case ids
NoIndex <- 0

 temp <- Matcher(Ncontrols,Tcontrols,Ncases,reuseControls,  
                        controlIndex, caseIndex,
                         controls,  cases,NoIndex)
 
  setDT(temp)[]


  */
  
 
 
