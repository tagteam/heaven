#' @title RiskSetMatch - Risk set matching
#' 
#' @description #' 
#' Risk set matching - also termed incidence density sampling - matches cases to control in such a way that only 
#' controls with event later than the case are accepted.  The current program is based on exact matching and allows 
#' the user to specify a "greedy" approach where controls are only used once as well as allowing the program to
#' reuse controls and to allow cases to be controls prior to being a case.
#' 
#' 
#' @usage
#' RiskSetMatch(ptid,event,terms,dat,Ncontrols,reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL,
#'         controlIndex=NULL, NoIndex=FALSE,cores=1)
#' 
#' @author Christian Torp-Pedersen
#' 
#' @param ptid  Personal ID variable defining participant
#' @param event Defines cases/controls MUST be 0/1
#' @param terms c(.....) the variables that should be matched by
#' @param dat The single dataset with all information - must be data.table
#' @param Ncontrols  Number of controls sought for each case
#' @param reuseCases T/F If T a case can be a control prior to being a case
#' @param reuseControls T/F If T a control can be reused for several cases
#' @param caseIndex Date variable defining the date where a case becomes a case. For a case control study thi
#'        would be the date of event of interest, for a cohort study the date where a case enters an analysis
#' @param controlIndex date variable defining the control data that needs to be larger chan caseIndex. For a
#'        case control study this would be the date where a control has the event of interest or is censored.  For a
#'        cohort study it would be the date where the control disappears from the analysis, e.g. due to death or censoring.
#' @param NoIndex if TRUE caseIndex/controlIndex are ignosed
#' @param cores number of cores to use, default is one
#' 
#' @details 
#' The function does exact matching and keep 2 dates (indices) apart.
#' Because the matching is exact all matching variables must be integer or character. Make sure that
#' sufficient rounding is done on continuous variables to ensure a decent number of controls for each case.
#' For example it may be difficult to find controls for very high age cases and age should often be rounded 
#' by 2,3 og 5 years -- and further aggregating extreme ages.
#' 
#' For case control studies age may be a relevant matching parameter - for most cohort studies year of birth is
#' more relevant since the age of a control varies with time.
#' 
#' For many purposes controls should be reused and cases allowed to be controls prior to being cases. 
#' 
#' With small samples the function is fast.  With extreme number of cases the calculation time can be many hours.
#' With long computations times please use the sister of this program RiskSetMatchMC where multiple cores can be 
#' applied
#' 
#' The function can be used for standard matching without the caseIndex/controlIndex, but other packages
#' such as MatchIt should preferably be used in these cases.
#' 
#' It may appear tempting always to use multiple cores, but this emplies a costly overhead.  With extreme numbers
#' of cases/controls it can save time. When a single core is used a progress shows progress of matching. There is
#' no progress bar with multiple cores
#' 
#' The function matchReport may be used to provide simple summaries of use of cases and controls
#'
#' @return data.table with cases and controls. A new variable "caseid" links controls to cases.  This variable name
#' is fixed and should not be used for other purposes.   Other variables in the original dataset are preserved 
#' unchanged. The final dataset includes all original cases but only the controls that were selected.
#' 
#' @seealso matchReport Matchit
#' 
#' @export
#'
#' @examples
#' require(data.table)
#' event <- c(rep(0,20),rep(1,5)) 
#' ptid <- 1:25
#' sex <- c(rep("fem",10),rep("mal",10),"fem","fem",rep("mal",3))
#' age <- c(rep(c(70,80),10),70,80,70,70,80)
#' caseIndex <- c(seq(10,28,2),seq(5,25,5))
#' controlIndex <- caseIndex
#' library(data.table)
#' dat <- data.table(ptid,event,sex,age,caseIndex,controlIndex)
#' # Very simple match without reuse - no dates to control for
#' out <- RiskSetMatch("ptid","event",c("age","sex"),dat,2,NoIndex=TRUE)
#' # Risk set matching without reusing cases/controls - 3 cases get no controls
#' out2 <- RiskSetMatch("ptid","event",c("age","sex"),dat,2,caseIndex="caseIndex",
#'   controlIndex="controlIndex")
#' # Risk set matching with reuse of cases (control prior to case) and reuse of 
#'   controls
#' out3 <- RiskSetMatch("ptid","event",c("age","sex"),dat,2,caseIndex=
#'   "caseIndex",controlIndex="controlIndex"
#'   ,reuseCases=TRUE,reuseControls=TRUE)
#' # Same with 2 cores          
#' out3 <- RiskSetMatch("ptid","event",c("age","sex"),dat,2,
#'   caseIndex="caseIndex",controlIndex="controlIndex"
#'  ,reuseCases=TRUE,reuseControls=TRUE,cores=2)
RiskSetMatch <- function(ptid,event,terms,dat,Ncontrols,reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL,
                         controlIndex=NULL, NoIndex=FALSE, cores=1){
#browser()  
  #ptid - pnr i DST id
  #event 0/1 distinguishes cases from controls
  #terms c(1,2,3) - list of vairables to match by
  #dat - dataset with all variables
  #Ncontrols - number of controls to provide
  #reuseCases - T og F or NULL - can a case be a control prior to being a case?
  #reuseControls - T or F or NULL
  #caseIndex - Integer or date, date where controls must be prior
  #controlIndex - Index date for controls
  options(warn=-1)
  # copy input data
  datt <- copy(dat)
  if (NoIndex) noindex <- 1L else noindex <- 0L # allows omitting index vectors
  # Check data.table
  if (!is.data.table(dat)) stop("data not data.table")
  # combine matching variables to single term - cterms
  datt[, cterms :=do.call(paste0,.SD),.SDcols=terms] 
  # Select relevant part of table for matching
  if (!NoIndex){
    alldata <- datt[,.SD,.SDcols=c(ptid,caseIndex,controlIndex,event,"cterms") ]
    setnames(alldata,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
  }
  else{
    alldata <- datt[,.SD,.SDcols=c(ptid,event,"cterms") ]
    setnames(alldata,c(".ptid",".event",".cterms"))
  }
  #Checking variables (partially)
  numbers <- table(alldata[,.event])
  if  ((names(numbers)==c("0","1")) !=c(T,T)) stop (" Event not 0 or 1 ")
  # prepare to split and split
  setkey(alldata,.cterms)
  split.alldata <- split(alldata,by=".cterms")
  if (cores<2){
    # Prepare progress bar
    totalprogress <-as.numeric(length(split.alldata)/1000)
    pb <-txtProgressBar(min = 0, max = 1000, initial = 0, char = "=",
                        width = NA, title, label, style = 1, file = "")
    progress <- 0;
    # Select controls - rbind of each split-member that selects controls
    selected.controls <- do.call(rbind,lapply(split.alldata,function(controls){
#browser()    
      # Setnames because data.table called from function
      if (!NoIndex) setnames(controls,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
      else setnames(controls,c(".ptid",".event",".cterms"))
      setkey(controls,.event,.ptid)
      # Define cases in particular match-group
      cases <- controls[.event==1]
      setkey(cases,.ptid)
      # Uf cases cannot becom controls they are removed from controls
      if (!reuseCases) controls <- controls[.event==0]
      #find lengths of controls and cases
      Tcontrols<-dim(controls)[1]
      Ncases<-dim(cases)[1]
      # sort controls by random number - so that they can be selected sequentially
      set.seed(17)
      controls[,random:=runif(.N,1,Ncontrols*10)]
      setkey(controls,random)
      # vectors for Rcpp
      CONTROLS <- as.character(controls[,.ptid])
      CASES <- as.character(cases[,.ptid])
      NreuseControls<-as.numeric(reuseControls) # Integerlogic for Rcpp
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
      progress <<- progress+1/totalprogress
      #Progress bar
        setTxtProgressBar(pb,progress)
        flush(stdout())
      Output
    })) # end function and do.call
  } #end if cores<2
  else
  {
    CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
    print(CLUST)
    parallel::clusterExport(CLUST, c("Matcher"))#,"NreuseControls","noindex","reuseCases","NoIndex"),envir=environment())
    parallel::clusterEvalQ(CLUST, library(data.table))
    parallel::clusterEvalQ(CLUST, library(heaven))
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
      Output <- data.table(heaven::Matcher(Ncontrols, Tcontrols, Ncases, NreuseControls,  
                                           controlIndex, caseIndex, CONTROLS, CASES,noindex))    
      Output
    })) # end function and do.call
    parallel::stopCluster(CLUST)
  }  #end cores>1
#browser()  
  setnames(selected.controls,c("caseid",".ptid"))
  selected.controls[,.event:=0]
  setkey(alldata,.event)
  cases <- alldata[.event==1]
  cases[,caseid:=.ptid]
  # Create final dataset with cases and controls
  FINAL <- rbind(cases[,.(.ptid,caseid,.event)],selected.controls[,.(.ptid,caseid,.event)])
  setkey(FINAL)
  #output
  setnames(FINAL,".ptid",ptid) #give the proper ptid back
  # Ensure that original ptid is character
  datt[,ptid:=as.character(ptid)]
  datt[,event:=NULL]
  FINAL <- merge(FINAL,datt,by=ptid)
  FINAL[,c(".case","cterms"):=NULL] # remove cterms - aggregated terms
  setkeyv(FINAL,c("caseid",".event"))
  #Add relevant caseid to controls
  if (!NoIndex) FINAL[,caseIndex:=caseIndex[.N],by=caseid]
  setnames(FINAL,".event",event)
  FINAL 
}
