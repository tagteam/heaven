#' @title riskSetMatch - Risk set matching
#' 
#' @description 
#' Risk set matching is common term to represent "incidence density sampling" 
#' or "exposure density sampling". In both cases the request is to match by 
#' a series of variables such that the outcome or exposure data of "controls" 
#' are later than the outcome or exposure for cases.
#' 
#' The current program is based on exact matching and allows the user to 
#' specify a "greedy" approach where controls are only used once as well as 
#' allowing the program to reuse controls and to allow cases to be controls 
#' prior to being a case.
#' 
#' In addition to the exact matching the function and also only select controls
#' where time of covariates are missing for both cases and controls, are both
#' before case index, are both after case index - or are missing for case index
#' and after case index for controls. 
#' 
#' To provide necessary speed for large samples the general technique used is 
#' to create a series of risk-groups that have the fixed matching variables 
#' identical (such as birthyear and gender).  The available controls are then 
#' sorted by a random number and selected for consecutive cases if the 
#' "case-date=case index" is later than the corresponding "control-date=
#' control index". The consecutive selection can in theory cause bias and the 
#' result of the function needs careful attention when the number of controls 
#' is small compared to the number of cases.
#' 
#' @usage
#'    riskSetMatch(ptid,event,terms,dat,Ncontrols,oldevent="oldevent"
#'    ,caseid="caseid",reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL 
#'    ,controlIndex=NULL,NoIndex=FALSE,cores=1,dateterms=NULL)
#' @author Christian Torp-Pedersen
#' @param ptid  Personal ID variable defining participant
#' @param event Defining cases/controls MUST be 0/1 - 0 for controls, 1 for case
#' @param terms c(.....) Specifies the variables that should be matched by - 
#' enclosed in ".."
#' @param dat The single dataset with all information - coerced to data.table
#' if data.frame
#' @param Ncontrols  Number of controls sought for each case
#' @param oldevent Holds original value of event - distinguishes cases used as 
#' controls
#' @param caseid Character. Variable holding grouping variable for 
#' cases/controls (=case-ptid)
#' @param reuseCases Logical. If \code{TRUE} a case can be a control prior to 
#' being a case
#' @param reuseControls Logical. If \code{TRUE} a control can be reused for 
#' several cases
#' @param caseIndex Integer/Date. Date variable defining the date where a case 
#' becomes a case. For a case control study this is the date of event of 
#' interest, for a cohort study the date where a case enters an analysis.
#' @param controlIndex Integer/Date. date variable defining the date from which 
#' a controls can no longer be selected.  The controlIndex must be larger than 
#' the caseIndex.  For a case control study this would be the date where a 
#' control has the event of interest or is censored.  For a cohort study it 
#' would be the date where the control disappears from the analysis, e.g. due to 
#' death or censoring.
#' @param NoIndex Logical. If \code{TRUE} caseIndex/controlIndex are ignored
#' @param cores number of cores to use, default is one
#' @param dateterms c(....) A list of variable neames (character) in "dat" 
#' specifying dates of conditions. When a list is specified it is not only
#' checked that the caseIndex is not after controlIndex, but also that for all
#' variables in the list either both control/case dates are missing, both prior
#' to case index, both after case index - or missing for case and with control
#' date after case index.
#' @details 
#' The function does exact matching and keeps 2 dates (indices) apart such that 
#' the date for controls is larger than that for cases. Because the matching 
#' is exact all matching variables must be integer or character. Make sure that
#' sufficient rounding is done on continuous and semicontinuous variables to 
#' ensure a decent number of controls for each case. For example it may be 
#' difficult to find controls for cases of very high age and age should 
#' therefore often be rounded by 2,3 or 5 years - and extreme ages further 
#' aggregated.
#' 
#' For case control studies age may be a relevant matching parameter - for most 
#' cohort studies year of birth is more relevant since the age of a control 
#' varies with time.
#' 
#' Many datasets have comorbidities as time dependent variables. Matching on
#' these requires that the comorbidity date is not (yet) reached for a corres-
#' ponding variables for cases if the case does not have the comorbidity and 
#' similarly that the date has been reached when the case does have that co-
#' morbidity.
#' 
#' For many purposes controls should be reused and cases allowed to be controls 
#' prior to being cases. By default, there is no reuse and this can be adjusted 
#' with "reuseCases" and "reuseControls"
#' 
#' The function can be used for standard matching without the caseIndex/
#' controlIndex (with "NoIndex"), but other packages such as MatchIt are more 
#' likely to be more optimal for these cases.
#' 
#' It may appear tempting always to use multiple cores, but this comes with a 
#' costly overhead because the function machinery has to be distributed to each 
#' defined "worker".  With very large numbers of cases and controls, multiple
#' cores can save substantial amounts of time. When a single core is used a 
#' progress shows progress of matching. There is no progress bar with multiple 
#' cores
#' 
#' The function matchReport may afterwards be used to provide simple summaries 
#' of use of cases and controls
#' @return data.table with cases and controls. After matching, a new variable 
#' "caseid" links controls to cases. Further, a variable "oldevent" holds the 
#' orginal value of "event" - to be used to identify cases functioning
#' as controls prior to being cases.
#' 
#' Variables in the original dataset are preserved. The final dataset includes 
#' all original cases but only the controls that were selected. 
#' 
#' If cases without controls should be removed, this is done by setting the
#' variable removeNoControls to TRUE
#' 
#' @seealso matchReport Matchit
#' 
#' @export
#'
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15)) 
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' case.Index <- c(seq(1,40,1),seq(5,47,3))
#' control.Index <- case.Index
#' diabetes <- seq(2,110,2)
#' heartdis <- seq(110,2,-2)
#' diabetes <- c(rep(1,55))
#' heartdis <- c(rep(100,55))
#' library(data.table)
#' dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,control.Index)
#' # Very simple match without reuse - no dates to control for
#' out <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,NoIndex=TRUE)
#' out[]
#' # Risk set matching without reusing cases/controls - 
#' # Some cases have no controls
#' out2 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="case.Index",
#'   controlIndex="control.Index")
#' out2[]   
#' # Risk set matching with reuse of cases (control prior to case) and reuse of 
#' # controls - more cases get controls
#' out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE)
#' out3[]   
#' # Same with 2 cores
#' out4 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE,cores=2)  
#' out4[]     
#' #Time dependent matching. In addtion to fixed matching parameters there are
#' #two other sets of dates where it is required that if a case has that condi-
#' #tion prior to index, then controls also need to have the condition prior to
#' #the case index to be eligible - and if the control does not have the condi-
#' #tion prior to index then the same is required for the control.
#' out5 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE,cores=1,
#'   dateterms=c("diabetes","heartdis"))  
#' out5[]   
#' 
#' #POSTPROCESSING
#' #It may be convinient to add the number of controls found to each case in or-
#' #der to remove cases without controls or where very few controls have been
#' #found.  This is easily obtained using data.table - with the example above:
#' #out5[,numControls:=.N,by=caseid] # adds a column with the number of controls
#'                                  # for each case-ID     
riskSetMatch <- function(ptid     # Unique patient identifier
                          ,event   # 0=Control, 1=case
                          ,terms   # terms c("n1","n2",...) - list of vairables to match by
                          ,dat     # dataset with all variables
                          ,Ncontrols  # number of controls to provide
                          ,oldevent="oldevent" # To distinguish cases used as controls
                          ,caseid="caseid" # variable to group cases and controls (case-ptid)
                          ,reuseCases=FALSE # T og F or NULL - can a case be a control prior to being a case?
                          ,reuseControls=FALSE # T or F or NULL - can controls be reused?
                          ,caseIndex=NULL      # Integer or date, date where controls must be prior
                          ,controlIndex=NULL   # controlIndex - Index date for controls
                          ,NoIndex=FALSE      # If T ignore index
                          ,cores=1 # Number of cores to use, default 1
                          ,dateterms=NULL # character list of date variables
){ 
  .SD=Internal.ptid=pnrnum=cterms=Internal.event=Internal.cterms=label=Internal.event=pnrnum=
    random=.N=Internal.controlIndex=Internal.caseIndex=random=Internal.controlIndex=Internal.caseIndex=NULL
  #check
  if (!is.character(ptid) | !is.character(event) | (!is.null(caseIndex) & !is.character (caseIndex)) |
      (!is.null(dateterms) & !is.character(dateterms))   |
      (!is.null(controlIndex) & !is.character(controlIndex))) stop (" Variables names must be character")
  setDT(dat) # coerce to data.table if necessary
  if(!is.integer(cores) & !(is.numeric(cores))) stop("cores must be integer, default 1")
  cores <- as.integer(cores)
  # copy input data
  datt <- copy(dat)
  datt[,"oldevent":=.SD,.SDcols=event]  #Remeber status of event before match - when cases can turn out as controls
  if (NoIndex) noindex <- 1L else noindex <- 0L # allows omitting index vectors
  setnames(datt,ptid,"Internal.ptid")
  #Check that patient IDs are unique:
  repetitians <- length(datt[,Internal.ptid])-length(unique(datt[,Internal.ptid]))
  if (repetitians>0) stop(paste(" Error, participant ID not unique. Number of repeats:",repetitians))
  datt[, pnrnum:=as.integer(factor(Internal.ptid))] # numeric sequence of ID
  # combine matching variables to single term - cterms
  datt[, cterms :=do.call(paste0,.SD),.SDcols=terms] 
  # Select relevant part of table for matching
  cols <-c("pnrnum",event,"cterms")
  if(!NoIndex) cols <-c("pnrnum",caseIndex,controlIndex,event,"cterms")
  if(!NoIndex & !is.null(dateterms)) cols <- c("pnrnum",caseIndex,controlIndex,event,"cterms",dateterms)
  alldata <- datt[,.SD,.SDcols=cols]
  # Rename variables
  if(!NoIndex) RcaseIndex <- caseIndex # remember name of caseIndex
  if(NoIndex) setnames(alldata,cols,c("pnrnum","Internal.event","Internal.cterms"))
  else
    if (!NoIndex & is.null(dateterms)) setnames(alldata,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
  else
    if (!NoIndex & !is.null(dateterms)){
      Internal.dateterms <- paste0("V",seq(1,length(dateterms)))
      setnames(alldata,c("pnrnum","Internal.caseIndex","InternalInternal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms)) 
      alldata[,(Internal.dateterms):=lapply(.SD,as.integer),.SDcols=Internal.dateterms]
    }
  #last check
  if (min(as.numeric(unique(alldata[,Internal.event])==c(0,1)))!=1) stop (" Event not 0 or 1 ")
  # prepare to split 
  setkey(alldata,Internal.cterms)
  split.alldata <- split(alldata,by="Internal.cterms") # Now a list aplit by Internal.cterms
  if (cores<2){
    # Prepare progress bar
    totalprogress <-as.numeric(length(split.alldata)/1000)
    pb <-txtProgressBar(min = 0, max = 1000, initial = 0, char = "=",
                        width = NA, title, label, style = 1, file = "")
    progress <- 0;
    # Select controls - rbind of each split-member that selects controls
    selected.controls <- do.call("rbind",lapply(split.alldata,function(controls){ # Function handles each split-group, afterward rbind
      # Setnames because data.table called from function
      if (!NoIndex & is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
      else
        if (!NoIndex & !is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms))
      else
        if (NoIndex) setnames(controls,c("pnrnum","Internal.event","Internal.cterms"))
      setkey(controls,Internal.event,pnrnum)
      # Define cases in selected match-group
      cases <- controls[Internal.event==1]
      setkey(cases,pnrnum)
      # If cases cannot become controls they are removed from controls
      if (!reuseCases) controls <- subset(controls,Internal.event==0)
      #find lengths of controls and cases
      Tcontrols<-dim(controls)[1]
      Ncases<-dim(cases)[1]
      # sort controls by random number - so that they can be selected sequentially
      set.seed(17)
      controls[,random:=runif(.N,1,Ncontrols*10)]
      setkey(controls,random)
      NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
      #Length of dateterm vector
      if(!is.null(dateterms)) Ndateterms=length(dateterms) else Ndateterms <- 0
      if(!NoIndex){
        control.date <- controls[,Internal.controlIndex]
        case.date <- cases[,Internal.caseIndex]
      } else {
        control.date <- 0L
        case.date <- 0L
      }
      #dateterm matrix
      if(!is.null(dateterms)){
        dates.cases <- as.matrix(cases[,.SD,.SDcols=Internal.dateterms])
        dates.controls <- as.matrix(controls[,.SD,.SDcols=Internal.dateterms])
      } 
      else {
        dates.cases <- as.matrix(0)
        dates.controls <- as.matrix(0)
      }
      Output <- .Call('_heaven_Matcher',PACKAGE = 'heaven',Ncontrols,Tcontrols,Ncases,NreuseControls,
                      control.date,case.date,controls[,pnrnum],cases[,pnrnum],
                      Ndateterms,dates.cases,dates.controls,noindex)
      setDT(Output)
      progress <<- progress+1/totalprogress
      #Progress bar
      setTxtProgressBar(pb,progress)
      flush(stdout())
      Output
    })) # end function and do.call
  } #end if cores<2
  else {
    CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
    selected.controls <- do.call(rbind,foreach::foreach(controls=split.alldata,.packages=c("heaven"),.export=c("reuseControls")) %dopar% {
      # Setnames because data.table called from function
      if (!NoIndex & is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
      else
        if (!NoIndex & !is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms))
      else
        if (NoIndex) setnames(controls,c("pnrnum","Internal.event","Internal.cterms"))
      setkey(controls,Internal.event,pnrnum)
      # Define cases in selected match-group
      cases <- controls[Internal.event==1]
      setkey(cases,pnrnum)
      # If cases cannot become controls they are removed from controls
      if (!reuseCases) controls <- subset(controls,Internal.event==0)
      #find lengths of controls and cases
      Tcontrols<-dim(controls)[1]
      Ncases<-dim(cases)[1]
      # sort controls by random number - so that they can be selected sequentially
      set.seed(17)
      controls[,random:=runif(.N,1,Ncontrols*10)]
      setkey(controls,random)
      NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
      #Length of dateterm vector
      if(!is.null(dateterms)) Ndateterms=length(dateterms) else Ndateterms <- 0
      if(!NoIndex){
        control.date <- controls[,Internal.controlIndex]
        case.date <- cases[,Internal.caseIndex]
      } else {
        control.date <- 0L
        case.date <- 0L
      }
      #dateterm matrix
      if(!is.null(dateterms)){
        dates.cases <- as.matrix(cases[,.SD,.SDcols=Internal.dateterms])
        dates.controls <- as.matrix(controls[,.SD,.SDcols=Internal.dateterms])
      } 
      else {
        dates.cases <- as.matrix(0)
        dates.controls <- as.matrix(0)
      }
      Output <- .Call('_heaven_Matcher',PACKAGE = 'heaven',Ncontrols,Tcontrols,Ncases,NreuseControls,
                      control.date,case.date,controls[,pnrnum],cases[,pnrnum],
                      Ndateterms,dates.cases,dates.controls,noindex)
      setDT(Output)
      Output
    })  
    parallel::stopCluster(CLUST)
    setDT(selected.controls)
  }  #end cores>1
  setnames(selected.controls,c(caseid,"pnrnum"))
  selected.controls[,Internal.event:=0]
  setkey(alldata,Internal.event)
  cases <- alldata[Internal.event==1]
  cases[,caseid:=pnrnum]
  # Create final dataset with cases and controls
  FINAL <- rbind(cases[,list(pnrnum,caseid,Internal.event)],selected.controls[,data.table::data.table(pnrnum,caseid,Internal.event)])
  setkey(FINAL)
  #output
  datt[,(event):=NULL]
  FINAL <- merge(FINAL,datt,by="pnrnum")
  FINAL[,c("cterms","pnrnum"):=NULL] # remove cterms - aggregated terms
  setnames(FINAL,"Internal.ptid",ptid)
  setkeyv(FINAL,c(caseid,"Internal.event"))
  #Add relevant caseid to controls
  if (!NoIndex) FINAL[,eval(caseIndex):=.SD[.N],.SDcols=c(caseIndex),by=caseid]
  setnames(FINAL,"Internal.event",event)
  FINAL 
}
