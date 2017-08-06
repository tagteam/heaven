#' @title MCRiskSetMatch - Risk set matching, multicore
#' 
#' @description
#' Risk set matching - also termed incidence density sampling - matches cases to control in such a way that only 
#' controls with event later than the case are accepted.  The current program is based on exact matching and allows 
#' the user to specify a "greedy" approach where controls are only used once as well as allowing the program to
#' reuse controls and to allow cases to be controls prior to being a case.
#' 
#' This function produces identical results to "RiskSetMatch" but is able to use multiple cores and thus has an
#' option to include a variable to define number of cores (default 10)
#' 
#' 
#' @usage
#' RiskSetMatch(ptid,event,terms,dat,Ncontrols,reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL,
#'         controlIndex=NULL, NoIndex=FALSE, cores=10)
#' 
#' @author Christian Torp-Pedersen
#' 

#'
#' @param ptid - Personal ID variable defining participant
#' @param event - Defines cases/controls MUST be 0/1
#' @param terms - c(.....) the variables that should be matched by
#' @param dat - The single dataset with all information - must be data.table
#' @param Ncontrols - Number of controls soucht for each case
#' @param reuseCases - T/F If T a case can be a control prior to being a case
#' @param reuseControls - T/F If T a control can be reused for several cases
#' @param caseIndex - Date variable defining the date where a case becomes a case. This could typicalle be the
#' date where e.g. a cancer develops and follow up starts - og in a case-control design the date where the event
#' of interest occurs.
#' @param controlIndex - date variable defining the control data that needs to be larger chan caseIndex.  For a cohort
#' design this could typically be the date where a control dies or is censored - and for a case-control design be a date
#' that is prior to a case date for that individual.
#' @param NoIndex - if TRUE caseIndex/controlIndex are ignosed
#' @param cores - Number of cores to use. Defaults to max of available and 10
#' 
#' \bold{Details}
#' The function does exact matching and keep 2 dates apart.
#' 
#' This is a multicore version of a very related function RiskSetMatch that uses a single core. This version is useful
#' for large tasks that may take many hours on a single core.  For smaller sets the single core version is likely to 
#' be faster since the overhead of distributing to cores is large.
#' 
#' Because the matching is exact all matching variables must be integer or character. Make sure that
#' sufficient rounding is done on continuous variables to ensure a decent number of controls for each case.
#' For example it may be difficult to find controls for very high age cases and age should often be rounded 
#' by 2,3 og 5 years - and further aggregating extreme ages.
#' 
#' For many purposes controls should be reused and cases allowed to be controls prior to being cases. The function
#' provides some statistics where You should make sure that reuse of cases/controls is not extreme - which may 
#' cause bias.
#' 
#' With small samples the function is fast.  With extreme number of cases the calculation time can be many hours.
#' With long computations times please use the sister of this program RiskSetMatchMC where multiple cores can be 
#' applied
#' 
#' The function can be used for standard matching without the caseIndex/controlIndex, but other packages
#' such as MatchIt should preferably be used in these cases.
#'
#' @return data.table with cases and controls. A new variable "caseid" links controls to cases. Further the value of caseIndex
#' is added to controls.  Other variables in the original dataset are preserved unchanged. 
#' The final dataset includes all original cases but only the controls that were selected.
#' @export
#'
#' @examples
#' event <- c(rep(0,20),rep(1,5)) 
#' ptid <- 1:25
#' sex <- c(rep("fem",10),rep("mal",10),"fem","fem",rep("mal",3))
#' age <- c(rep(c(70,80),10),70,80,70,70,80)
#' caseIndex <- c(seq(10,28,2),seq(5,25,5))
#' controlIndex <- caseIndex
#' dat <- data.table(ptid,event,sex,age,caseIndex,controlIndex)
#' # Very simple match without reuse - no dates to control for
#' out <- RiskSetMatchMC("ptid","event",c("age","sex"),dat,2,NoIndex=TRUE,cores=2)
#' # Risk set matching without reusing cases/controls - 3 cases get no controls
#' out2 <- RiskSetMatchMC("ptid","event",c("age","sex"),dat,2,caseIndex="caseIndex",controlIndex="controlIndex",cores=2)
#' # Risk set matching with reuse of cases (control prior to case) and reuse of controls
#' out3 <- RiskSetMatchMC("ptid","event",c("age","sex"),dat,2,caseIndex="caseIndex",controlIndex="controlIndex"
#'          ,reuseCases=TRUE,reuseControls=TRUE,cores=2)
RiskSetMatchMC <- function(ptid,event,terms,dat,Ncontrols,reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL,
                         controlIndex=NULL, NoIndex=FALSE,cores=10){
  #ptid - pnr i DST id
  #event 0/1 distinguishes cases from controls
  #terms c(1,2,3) - list of vairables to match by
  #dat - dataset with all variables
  #Ncontrols - number of controls to provide
  #reuseCases - T og F or NULL - can a case be a control prior to being a case?
  #reuseControls - T or F or NULL
  #caseIndex - Integer or date, date where controls must be prior
  #controlIndex - Index date for controls
  require(data.table)
  require(parallel)
  require(heaven)
  options(warn=-1)
  # Check data.table
  if (!is.data.table(dat)) stop("data not data.table")
  # combine matching variables to single term - cterms
  dat[, cterms :=do.call(paste0,.SD),.SDcols=terms] 
  # Select relevant part of table for matching
  if (!NoIndex){
    alldata <- dat[,.SD,.SDcols=c(ptid,caseIndex,controlIndex,event,"cterms") ]
    setnames(alldata,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
  }
  else{
    alldata <- dat[,.SD,.SDcols=c(ptid,event,"cterms") ]
    setnames(alldata,c(".ptid",".event",".cterms"))
  }
  #Checking variables (partially)
  numbers <- table(alldata[,.event])
  cat ("Numbers of controls and cases","\n")
  print(numbers)
  if  ((names(numbers)==c("0","1")) !=c(T,T)) stop (" Event not 0 or 1 ")
  # prepare to split and split
  setkey(alldata,.cterms)
  split.alldata <- split(alldata,by=".cterms")
  #Prepare multicore
  NreuseControls<-as.numeric(reuseControls) # Integerlogic for Rcpp
  if (NoIndex) noindex <- 1L else noindex <- 0L # Noindex for Rcpp
  CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
  print(CLUST)
  clusterExport(CLUST, c("Matcher"))#,"NreuseControls","noindex","reuseCases","NoIndex"),envir=environment())
  clusterEvalQ(CLUST, library(data.table))
  clusterEvalQ(CLUST, library(heaven))
  # Select controls - rbind of each split-member that selects controls
  selected.controls <- do.call(rbind,parallel::parLapply(CLUST,split.alldata,function(controls){
    # Setnames because data.table called from functio
    if (!NoIndex) setnames(controls,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
    else setnames(controls,c(".ptid",".event",".cterms"))
    setkey(controls,.event,.ptid)
    # Define cases in particular match-group
    cases <- controls[.event==1]
    setkey(cases,.ptid)
    # If cases cannot become controls they are removed from controls
    if (!reuseCases) controls <- controls[.event==0]
    #find lengths of controls and cases
    Tcontrols<-dim(controls)[1]
    Ncases<-dim(cases)[1]
    # sort controls by random number - so that they can be selected sequentially
    set.seed(17)
    controls[,random:=runif(.N,1,Ncontrols*10)]
    setkey(controls,random)
    # vectors for Rcpp
    CONTROLS <- controls[,.ptid]
    CASES <- cases[,.ptid]
    if(!NoIndex){
      controlIndex <- controls[,.controlIndex]
      caseIndex <- cases[,.caseIndex]
    } else {
      controlIndex <- 0L
      caseIndex <- 0L
    }
    Output <- heaven::Matcher(Ncontrols, Tcontrols, Ncases, NreuseControls,  
              controlIndex, caseIndex, CONTROLS, CASES,noindex)
    setDT(Output)
    Output
  })) # end function and do.call
  #Close cluster
  if(!is.null(parallelCluster)) {
    parallel::stopCluster(CLUST)
    parallelCluster <- c()
  }
  setnames(selected.controls,c(".ptid","caseid"))
  selected.controls[,.event:=0]
  setkey(alldata,.event)
  cases <- alldata[.event==1]
  cases[,caseid:=.ptid]
  # Create final dataset with cases and controls
  FINAL <- rbind(cases[,.(.ptid,caseid,.event)],selected.controls[,.(.ptid,caseid,.event)])
  setkey(FINAL)
  ## Report matching success
  control<-FINAL[,.N,by=caseid]
  control<-as.vector(control[,N]-1)
  cat("\n","Table of control numbers for cases","\n")
  print(table(control))
  ## Report on reusing controls
  report<-FINAL[.event==0,.N,by=".ptid"]
  report<-with(report,table(N))
  cat("Table of use/reuse controls","\n")
  print(report)
  ## Report of using/reusing cases as controls
  caselist <- cases[,.ptid]
  report <- FINAL[caselist,.N,by=".ptid"]
  report <- with(report,table(N))
  cat("Table of use/reuse of cases\n")
  print(report)
  #output
  setnames(FINAL,".ptid",ptid) #give the proper ptid back
  # Ensure that original ptid is character
  dat[,ptid:=as.character(ptid)]
  FINAL <- merge(FINAL,dat,by=ptid)
  FINAL[,c(".case","cterms"):=NULL] # remove cterms - aggregated terms
  setkeyv(FINAL,c("caseid",event))
  # set caseid for controls
  if (!NoIndex) FINAL[,caseIndex:=caseIndex[.N],by=caseid]
  FINAL 
}