
#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

//' @title Matcher
//' @author Christian Torp-Pedersen
//' @export
// [[Rcpp::export]]


CharacterMatrix Matcher(int Ncontrols, int Tcontrols, int Ncases, int reuseControls,  
                        IntegerVector controlIndex, IntegerVector caseIndex,
                        CharacterVector controls, CharacterVector cases,int NoIndex){
  // Ncontrols - desired number of controls
  // Tcontrols - totral number of controls
  // Ncases - total number of cases
  // reuseControls  - pseudological 0/1
  // controlIndex - controls id   caseIndex - case id - usedControls - have been used?
  int ii; // while counter
  int controlCounter=0;// Sequencer through list of controls - controlIndex
  bool IsCoEl;  // preliminary test of selectability
  CharacterVector selectedControls; // output controls
  CharacterVector selectedCases;    // output cases-list
  std::vector<int> haveTried(Tcontrols+1,0); // Hve tried and failed or is-taken if 1
  std::vector<int> usedControls(Tcontrols+1,0);
  srand(216); //set seed;
  for (int i=0; i<Ncases; i++){
    if (Ncases<1 || Tcontrols<1) break;
    for (int j=0; j<Tcontrols; j++) haveTried[j]=0; //Initialize
    ii=1;
    while (ii<=Ncontrols){
      //Is control eligible - variable
      if (NoIndex==1) IsCoEl=controlCounter<Tcontrols;
      else  
      IsCoEl=  controlCounter<Tcontrols && (controlIndex[controlCounter]==NA || caseIndex[i]==NA || 
               caseIndex[i]<controlIndex[controlCounter]);
      if ((usedControls[controlCounter]==0 && IsCoEl) || (haveTried[controlCounter]==0 && IsCoEl)) { //foundOne
        selectedControls.push_back(controls[controlCounter]); //next selected control
        selectedCases.push_back(cases[i]); //case-id for that control
        ii+=1; // next control to be selected
        usedControls[controlCounter] = 1;
        haveTried[controlCounter] = 1;
        controlCounter +=1;
        if (controlCounter==Tcontrols-1) {//Reached end of controls
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
  CharacterMatrix out(selectedCases.size(),2);  
  if (selectedControls.size()>=1)
    { out(_,1)=selectedCases; 
      out(_,0)=selectedControls;
    }
  return(out);
}  


  
 
 
