#' @title riskSetMatch - Risk set matching
#' 
#' @description #' 
#' Risk set matching - also termed incidence density sampling - matches cases to control in such a way that only 
#' controls with event later than the case are accepted.  The current program is based on exact matching and allows 
#' the user to specify a "greedy" approach where controls are only used once as well as allowing the program to
#' reuse controls and to allow cases to be controls prior to being a case.
#' 
#' 
#' @usage
#'   riskSetMatch <- function(
#'   ptid     # Unique patient identifier
#' , event   # 0=Control, 1=case
#' , terms   # terms c("n1","n2",...) - list of vairables to match by
#' , dat     # dataset with all variables
#' , Ncontrols  # number of controls to provide
#' , oldevent="oldevent" # To distinguish cases used as controls
#' , caseid="caseid" # variable to group cases and controls (case-ptid)
#' , reuseCases=FALSE # T og F or NULL - can a case be a control prior to being a case?
#' , reuseControls=FALSE # T or F or NULL - can controls be reused?
#' , caseIndex=NULL      # Integer or date, date where controls must be prior
#' , controlIndex=NULL   # controlIndex - Index date for controls
#' ,  NoIndex=FALSE      # If T ignore index
#' ,  cores=1)          # Number of cores to use, default 1
#' 
#' @author Christian Torp-Pedersen
#' 
#' @param ptid  Personal ID variable defining participant
#' @param event Defining cases/controls MUST be 0/1 - 0 for controls, 1 for case
#' @param terms c(.....) Specifies the variables that should be matched by - enclosed in ".."
#' @param dat The single dataset with all information - must be data.table
#' @param Ncontrols  Number of controls sought for each case
#' @param oldevent Holds original value of event - distinguishes cases used as controls
#' @param caseid - variable holding grouping variable for cases/controls (=case-ptid)
#' @param reuseCases T/F If T a case can be a control prior to being a case
#' @param reuseControls T/F If T a control can be reused for several cases
#' @param caseIndex Date variable defining the date where a case becomes a case. For a case control study this
#'        is the date of event of interest, for a cohort study the date where a case enters an analysis
#' @param controlIndex date variable defining the date from which a controls can no longer be selected.  The controlIndex
#'        must be larger than the caseIndex.  For a case control study this would be the date where a control has the 
#'        event of interest or is censored.  For a cohort study it would be the date where the control disappears from 
#'        the analysis, e.g. due to death or censoring.
#' @param NoIndex if TRUE caseIndex/controlIndex are ignored
#' @param cores number of cores to use, default is one
#' 
#' @details 
#' The function does exact matching and keeps 2 dates (indices) apart such that the date for controls is larger than 
#' that for cases. Because the matching is exact all matching variables must be integer or character. Make sure that
#' sufficient rounding is done on continuous and semicontinuous variables to ensure a decent number of controls for 
#' each case. For example it may be difficult to find controls for cases of very high age and age should therefore
#' often be rounded by 2,3 or 5 years - and extreme ages further aggregated.
#' 
#' For case control studies age may be a relevant matching parameter - for most cohort studies year of birth is
#' more relevant since the age of a control varies with time.
#' 
#' For many purposes controls should be reused and cases allowed to be controls prior to being cases. By default,
#' there is no reuse and this can be adjusted with "reuseCases" and "reuseControls"
#' 
#' The function can be used for standard matching without the caseIndex/controlIndex (with "NoIndex"), but other packages
#' such as MatchIt are more likely to be optimal for these cases.
#' 
#' It may appear tempting always to use multiple cores, but this comes with a costly overhead because the function
#' machinery has to be distributed to each defined "worker".  With very large numbers of cases and controls, multiple
#' cores can save substantial amounts of time. When a single core is used a progress shows progress of matching. 
#' There is no progress bar with multiple cores
#' 
#' The function matchReport may afterwards be used to provide simple summaries of use of cases and controls
#'
#' @return data.table with cases and controls. After matching, a new variable "caseid" links controls to cases.
#' Further, a variable "oldevent" holds the orginal value of "event" - to be used to identify cases functioning
#' as controls prior to being cases.
#' Variables in the original dataset are preserved. The final dataset includes all original cases but only the 
#' controls that were selected.
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
#' caseIndex <- c(seq(1,40,1),seq(5,47,3))
#' controlIndex <- caseIndex
#' library(data.table)
#' dat <- data.table(ptid,case,sex,byear,caseIndex,controlIndex)
#' # Very simple match without reuse - no dates to control for
#' out <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,NoIndex=TRUE)
#' # Risk set matching without reusing cases/controls - Some cases have no controls
#' out2 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="caseIndex",
#'   controlIndex="controlIndex")
#' # Risk set matching with reuse of cases (control prior to case) and reuse of 
#'   controls - more cases get controls
#' out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "caseIndex",controlIndex="controlIndex"
#'   ,reuseCases=TRUE,reuseControls=TRUE)
#' # Same with 2 cores
#' out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "caseIndex",controlIndex="controlIndex"
#'   ,reuseCases=TRUE,reuseControls=TRUE,cores=2)          
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
                         , NoIndex=FALSE      # If T ignore index
                         , cores=1){          # Number of cores to use, default 1
    options(warn=-1)
    # copy input data
    datt <- copy(dat)
    datt[,"oldevent":=eval(as.name(event))]
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
    if (!is.character(ptid) | !is.character(event) | (!is.null(caseIndex) & !is.character (caseIndex))
        | (!is.null(controlIndex) & !is.character(controlIndex))) stop (" Variables names must be character")
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
        selected.controls <- do.call("rbind",lapply(split.alldata,function(controls){
            # Setnames because data.table called from function
            if (!NoIndex) setnames(controls,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
            else setnames(controls,c(".ptid",".event",".cterms"))
            setkey(controls,.event,.ptid)
            # Define cases in particular match-group
            cases <- controls[.event==1]
            setkey(cases,.ptid)
            # Uf cases cannot becom controls they are removed from controls
            if (!reuseCases) controls <- subset(controls,.event==0)
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
            NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
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
    else {
        CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
        print(CLUST)
        parallel::clusterExport(CLUST, c("Matcher"))#,"NreuseControls","noindex","reuseCases","NoIndex"),envir=environment())
        # Select controls - rbind of each split-member that selects controls
        selected.controls <- do.call(rbind,foreach::foreach(controls=split.alldata,.packages=c("heaven"),.export=c("reuseControls")) %dopar% {
            # Setnames because data.table called from function
            if (!NoIndex) data.table::setnames(controls,c(".ptid",".caseIndex",".controlIndex",".event",".cterms"))
            else data.table::setnames(controls,c(".ptid",".event",".cterms"))
            data.table::setkey(controls,.event,.ptid)
            # Define cases in particular match-group
            cases <- controls[.event==1]
            data.table::setkey(cases,.ptid)
            # If cases cannot become controls they are removed from controls
            if (!reuseCases) controls <- subset(controls,.event==0)
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
            NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
            if(!NoIndex){
                controlIndex <- controls[,.controlIndex]
                caseIndex <- cases[,.caseIndex]
            } else {
                controlIndex <- 0L
                caseIndex <- 0L
            }
            ## print(list(Ncontrols, Tcontrols, Ncases, NreuseControls, controlIndex, caseIndex, CONTROLS, CASES,noindex))
            Output <- heaven::Matcher(Ncontrols, Tcontrols, Ncases, NreuseControls,  
                                      controlIndex, caseIndex, as.character(CONTROLS), as.character(CASES),noindex)
            setDT(Output)
            Output
        }) # end function and do.call
        parallel::stopCluster(CLUST)
        setDT(selected.controls)
    }  #end cores>1
    setnames(selected.controls,c(caseid,".ptid"))
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
    datt[,(event):=NULL]
    FINAL <- merge(FINAL,datt,by=ptid)
    FINAL[,c(".case","cterms"):=NULL] # remove cterms - aggregated terms
    setkeyv(FINAL,c(caseid,".event"))
    #Add relevant caseid to controls
    if (!NoIndex) FINAL[,caseIndex:=caseIndex[.N],by=caseid]
    setnames(FINAL,".event",event)
    FINAL 
}
