// [[Rcpp::depends(RcppArmadillo)]]
#include <vector>
#include <string>
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::export]]
List Matcher(int Ncontrols, // Desired number of controls
	     int Tcontrols, // Total number of controls
	     int Ncases,    // Total number of cases
	     int expWindow, // Fixed required exposure window between case index and start
	     IntegerVector startDate, // Startdate of condition defining exposure window
	     IntegerVector endFollowUp, // max time for being in at-risk-set
	     IntegerVector caseIndex, // Cases dates for at-risk-set
	     IntegerVector controls, // controls ID
	     IntegerVector cases, // cases ID
	     int Ndateterms, // number of dateterm variables - zero=0
	     IntegerMatrix datescases, // case dates 
	     IntegerMatrix datescontrols // control dates   
	     ){ // Ignore index if 1 - match without regard to risk-set - essentially simple match 
  int ii; // while counter - number of selected controls
  int controlCounter;// Sequencer through list of controls - endFollowUp
  int innerCounter=0;
  arma::vec neworder(Tcontrols); 
  for (int u=0;u<Tcontrols;u++){neworder(u)=u;}
  bool IsCoEl;  // preliminary test of selectability
  std::vector<int> selectedControls; // output controls
  selectedControls.reserve(Tcontrols*(Ncases+1)); // avoid repeated copying of vector
  std::vector<int> selectedCases;    // output cases-list
  selectedCases.reserve(Ncontrols*(Ncases+1));
  int casehasevent=0;
  int controlhasevent=0;
  for (int i=0; i<Ncases; i++){ // Loop through each case to identify controls
    // Rcout << "i: " << i << std::endl;
    // shuffle order in which controls are considered
    ii=1; // while counter - number of selected controls for a case
    innerCounter=0; 
    arma::vec currentorder = arma::shuffle(neworder);
    controlCounter = currentorder(innerCounter);
    while (innerCounter<Tcontrols && ii<=Ncontrols){ 
      controlCounter=currentorder(innerCounter);
      //A control must be at risk at the case's case date
      IsCoEl= (caseIndex[i]<endFollowUp[controlCounter]);
      // Rcout << "caseIndex[i]: " << caseIndex[i] << std::endl;
      // Rcout << "endFollowUp[controlCounter]: " << endFollowUp[controlCounter] << std::endl;      
      // Rcout << "IsCoEl: " << IsCoEl << std::endl;      
      // Time dependent matching variables:
      // event dates of case and control must be on the same side of the case index date
      if(IsCoEl && Ndateterms>0){ // Additional logic with time varying conditions to be before case date
        for (int k=0; k<Ndateterms; k++){
	  controlhasevent = (datescontrols(controlCounter,k)<caseIndex[i]);
	  casehasevent = (datescases(i,k)<caseIndex[i]);
	  if (controlhasevent != casehasevent) IsCoEl=false;
        }
      }
      // Exposure window:
      // a control is only a control if difference between
      // case dato and intro into our study is larger than expWindow
      // Rcout << "IsCoEl: " << IsCoEl << std::endl;      
      if(IsCoEl && expWindow>0){ 
	IsCoEl= IsCoEl && ((caseIndex[i]-startDate[controlCounter])>expWindow);
      }
      if (IsCoEl) { // foundOne
	// Rcout << "-------: "<< std::endl;
	// Rcout << "caseIndex[i]: " << caseIndex[i] << std::endl;
	// Rcout << "endFollowUp[controlCounter]: " << endFollowUp[controlCounter] << std::endl;
	// Rcout << "controls[controlCounter]: " << endFollowUp[controlCounter] << std::endl;      
	// Rcout << "IsCoEl: " << IsCoEl << std::endl;      
	selectedControls.push_back(controls[controlCounter]); //next selected control
	selectedCases.push_back(cases[i]); //case-id for that control
	ii+=1; // next control to be selected
      }
      innerCounter+=1;
    } // end while
  }// End loop for each case  
  return List::create(Named("selectedCases") = selectedCases,
                      Named("selectedControls") = selectedControls);
}


