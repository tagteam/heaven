#' @title riskSetMatch - Risk set matching
#'
#' @description
#' Risk set matching or incidence density sampling: Within a cohort,
#' every time a case is diagnosed (has event) one or more controls are selected
#' from other members of the cohort who are at risk at that time, i.e.,
#' a) do not have the diagnosis yet, b) are not dead and c) are not censored.
#' In addition to selecting controls from
#' the risk set of the case, the function does exact matching on the
#' covariates values at the case's diagnosis date. In particular, the function can
#' match on the status of time-dependent comorbidities
#' (status of other events) at the case's index date. 
#' 
#' The resulting dataset can be used to fit a Cox regression model
#' with time-dependent covariates.  
#'
#' To provide necessary speed for large samples the general technique used is
#' work with data.table and to create a series of match groups that have the fixed matching variables
#' identical (such as birthyear and gender).
#'
#' @usage
#'    riskSetMatch(ptid,event,terms,data,n.controls,
#'    ,case.id="case.id",case.index=NULL
#'    ,end.followup=NULL,cores=1,date.terms=NULL,
#'    exposure.window=0,start.date=NULL,seed=17)
#' @author Christian Torp-Pedersen & Thomas Alexander Gerds
#' @param ptid  Personal ID variable defining participant
#' @param event Defining cases/controls MUST be integer 0/1 - 0 for controls, 1 for case
#' @param terms Vector of variable names specifying the variables that should be matched on.
#' @param data The single dataset with all information - coerced to data.table
#' if data.frame
#' @param n.controls Number of controls for each case
#' @param case.id Character. Variable holding grouping variable for
#' cases/controls (=case-ptid)
#' @param case.index Integer or Date. Date variable defining the date where a case
#' becomes a case. For a case control study this is the date of event of
#' interest, for a cohort study the date where a case enters an analysis.
#' @param end.followup Integer or Date. Date variable defining the date from which
#' a controls can no longer be selected. The end.followup must be larger or equal to 
#' the case.index date. The date where the control disappears from the analysis, e.g. due to
#' death, other competing risk, or censoring.
#' @param cores number of cores to use in the calculation.
#' @param date.terms c(....) A list of variable names (character) in "data"
#' specifying dates of conditions. When a list is specified it is not only
#' checked that the case.index is not after end.followup, but also that for all
#' variables in the list either both control/case dates are missing, both prior
#' to case index, both after case index - or missing for case and with control
#' date after case index.
#' @param exposure.window For case/control studies this can be specified to
#' ensure that controls have a minimum exposure window for some condition which
#' starts at the following variable "start.date".  For practical use only cases
#' with a certain exposure window are first selected and then this feature is
#' used to ensure similar exposure for controls.
#' @param start.date Starting date of condition which defines exposure window
#' @param seed - Seed for random shuffling of cases
#' @param progressbar set to \code{FALSE} to avoid progressbar
#' @details
#' The function performs exact matching and hence 
#' all matching variables must be factor variables or character.
#'
#' Make sure that
#' appropriate classification is in place for truly continuous variables, such as age. This is to
#' ensure a sufficient number of controls for each case. For example it may be
#' difficult to find controls for cases of very high and very low ages
#' and extreme ages should therefor further aggregated. 
#'
#' Many datasets have comorbidities as time dependent variables. Matching on
#' these requires that the comorbidity date is not (yet) reached for a corresponding
#' variables for cases if the case does not have the comorbidity and
#' similarly that the date has been reached when the case does have that co-
#' morbidity. 
#'
#' For special cases it may be required that there is a minimum duration of a
#' condition shared by cases and controls. This can be achieved with defining
#' exposure window (same units as the various times, usually days) and
#' start.date, the day the condition of interest starts
#'
#' It may appear tempting always to use multiple cores, but this comes with a
#' time costing overhead because the function machinery has to be distributed
#' to each defined "worker".  With very large numbers of cases and controls,
#' multiple cores can save substantial amounts of time. When a single core is
#' used a progress shows progress of matching. There is no progress bar with
#' multiple cores.
#'
#' The function matchReport may afterwards be used to provide simple summaries
#' of use of cases and controls
#' @return data.table with cases and controls. After matching, a the variable
#' "case.id" identifies sets which include 1 case and x matched controls.
#'
#' Variables in the original dataset are preserved. The final dataset includes
#' all original cases but only the controls that were selected.
#' @seealso matchReport Matchit
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15))
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' case.Index <- c(seq(1,40,1),seq(5,47,3))
#' startDisease <- rep(10,55)
#' control.Index <- case.Index
#' diabetes <- seq(2,110,2)
#' heartdis <- seq(110,2,-2)
#' diabetes <- c(rep(1,55))
#' heartdis <- c(rep(100,55))
#' library(data.table)
#' dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,
#' control.Index,startDisease)
#' # Risk set matching
#' out3 <- riskSetMatch("ptid","case",c("byear","sex"),data=dat,2,case.index=
#'   "case.Index",end.followup="control.Index")
#' out3[]
#' # Same with 2 cores
#' library(parallel)
#' library(foreach)
#' out4 <- riskSetMatch("ptid","case",c("byear","sex"),data=dat,2,case.index=
#'   "case.Index",end.followup="control.Index"
#'   ,cores=2)
#' out4[]
#' #Time dependent matching. In addition to fixed matching parameters there are
#' #two other sets of dates where it is required that if a case has that condi-
#' #tion prior to index, then controls also need to have the condition prior to
#' #the case index to be eligible - and if the control does not have the condi-
#' #tion prior to index then the same is required for the control.
#' out5 <- riskSetMatch("ptid","case",c("byear","sex"),data=dat,2,case.index=
#'   "case.Index",end.followup="control.Index"
#'   ,cores=1,
#'   date.terms=c("diabetes","heartdis"))
#' out5[]
#' # Case control matching with requirement of minimum exposure time in each
#' # group
#' out6 <- riskSetMatch("ptid","case",c("byear","sex"),data=dat,2,case.index=
#'   "case.Index",end.followup="control.Index"
#'   ,cores=1,
#'   exposure.window=15,start.date="startDisease")
#' out6[]
#'
#' #POSTPROCESSING
#' #It may be convenient to add the number of controls found to each case in or-
#' #der to remove cases without controls or where very few controls have been
#' #found.  This is easily obtained using data.table - with the example above:
#' #out5[,numControls:=.N,by=case.id] # adds a column with the number of controls
#'                                  # for each case-ID
#' @export
riskSetMatch <- function(ptid     # Unique patient identifier
                        ,event   # 0=Control, 1=case
                        ,terms   # terms c("n1","n2",...) - list of variables to match by
                        ,data     # dataset with all variables
                        ,n.controls  # number of controls to provide
                        ,case.id="case.id" # variable to group cases and controls (case-ptid)
                        ,case.index=NULL      # Integer or date, date where controls must be prior
                        ,end.followup=NULL   # end.followup - Index date for controls
                        ,cores=1 # Number of cores to use, default 1
                        ,date.terms=NULL # character list of date variables
                        ,exposure.window=0 # Duration of a minimal exposure window for the condition defined by start.date
                        ,start.date=NULL # When relevant starting date of condition where a minimal exposure window is required
                        ,seed=17 # Seed for random sort
                        ,progressbar=TRUE
                         ){
    .SD=cterms=pnrnum=.N=NULL
                                        #check
    vnames <- colnames(data)
    if (length(ptid)!=1 || !is.character(ptid) || match(ptid,vnames,nomatch=0)==0)
        stop ("Argument 'ptid' must be the name of a variable in the dataset.")
    if (length(event)!=1 || !is.character(event) || match(event,vnames,nomatch=0)==0)
        stop ("Argument 'event' must be the name of a variable in the dataset.")
    if (length(case.index)!=1 || !is.character(case.index)  || match(case.index,vnames,nomatch=0)==0)
        stop ("Argument 'case.index' must be the name of a variable in the dataset.")
    if (length(end.followup)!=1 || !is.character(end.followup) || match(end.followup,vnames,nomatch=0)==0)
        stop ("Argument 'end.followup' must be the name of a variable in the dataset.")
    if (!is.null(date.terms)){
        for (dt in date.terms){
            if (length(dt)!=1 || !is.character(dt) || match(dt,vnames,nomatch=0)==0)
                stop ("Argument 'date.terms' must be a vector of names of variables in the dataset.")
        }
    }
    if (exposure.window==0) start.date <- 0 # No exposure window
    if (exposure.window>0 & is.null(start.date)) stop("Error - An exposure window requires a starting date")
    if(!is.integer(cores) & !(is.numeric(cores))) stop("cores must be integer, default 1")
    cores <- as.integer(cores)
                                        # copy input data
    datt <- copy(data)
    setDT(datt)
                                        #Check that patient IDs are unique:
    if ((repetitians <- sum(duplicated(datt[["ptid"]])))>0)
        stop(paste(" Error, participant ID not unique. Number of repeats:",repetitians))
    datt[,pnrnum:=1:.N]
                                        # combine matching variables to single term - cterms
    ## system.time(datt[, cterms :=do.call(paste0,.SD),.SDcols=terms])
    datt[, cterms :=interaction(.SD,drop=TRUE),.SDcols=terms]
                                        # Select relevant part of table for matching - and provide internal names
    cols <-c("pnrnum",event,"cterms",case.index,end.followup)
    if (!is.null(date.terms)){
        cols <- c(cols,date.terms)
    }
    if(exposure.window>0){
        cols <- c(cols,start.date)
    }
    alldata <- datt[,.SD,.SDcols=cols]
    setcolorder(alldata,cols) # ensure correct order of columns
                                        #conversion to integer of all date.terms
    for (v in c(case.index,end.followup,date.terms)){
        set(alldata,j=v,value=as.integer(alldata[[v]]))
    }
                                        # Cases can only be controls before being a case, hence the case index date limits the followup
    alldata[[end.followup]] <- pmin(alldata[[case.index]],alldata[[end.followup]],na.rm=TRUE)
    efup <- alldata[[end.followup]]
    if (any(is.na(efup)))stop("Missing values in end.followup variable. ")
    beyond.maxtime <- max(efup)+1
                                        # replace missing dates with maxtime
    na.replace <- function(v,value){v[is.na(v)]=value;v}
    for (v in c(case.index,end.followup,date.terms)){
        alldata[is.na(get(v)),(v):=beyond.maxtime]
    }
    if(!is.null(date.terms)) {
                                        #Number of date.terms
        Ndate.terms=length(date.terms)
    }else {
        Ndate.terms <- 0
    }
                                        # prepare to split
    setkey(alldata,cterms)
                                        # make sure every case has a date
                                        # FIXME could define event as a non missing caseindex
    ## alldata <- alldata[alldata[[event]]==0 | !is.na(alldata[[case.index]])]
    split.alldata <- split(alldata,by="cterms") # Now a list aplit by cterms
    message("\nMatching terms define ",length(split.alldata)," subsets")
    Nsub <- sapply(split.alldata,NROW)
    progressValues <- cumsum(Nsub)
    if ((Nsmallgroups <- sum(Nsub<n.controls))>0)
        message("Matching terms split the data into ",length(split.alldata)," subsets",
                "\nThere are ",Nsmallgroups," subsets which contain less subjects (case-control-mix)\n",
                "than the number of requested controls for one case.\n")
    if ((Ntinygroups <- sum(Nsub<2))>0)
        message("There are ",Ntinygroups," subsets that have less than two subjects\nIMPORTANT: These are ignored with side effect.\n")
    message("You should reduce",
            "\n  - the number matching variables\n  - the number of values of the matching variables\n  - or both.\n")
    if (cores>1){
        CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
    }else{
        foreach::registerDoSEQ
    }
                                        # Prepare progress bar
    if (progressbar){
        totalprogress <-as.numeric(length(split.alldata)/1000)
        pb <-txtProgressBar(min = 0,
                            max = NROW(alldata),
                            initial = 0,
                            char = "'",
                            width = 20,
                            style = 3,
                            file = "")
        progress <- 0
    }
                                        # Select controls - rbind of each split-member that selects controls
    `%dopar%` <- foreach::`%dopar%`
    ## selected.controls <- do.call(rbind,foreach::foreach(controls=split.alldata,.packages=c("heaven")) %dopar% {
    selected.controls <- do.call(rbind,foreach::foreach(sub=1:length(split.alldata),.packages=c("heaven")) %dopar% {
        controls=split.alldata[[sub]]
                                        # selected.controls <- do.call("rbind",lapply(split.alldata,function(controls){ # Function handles each split-group, afterward rbind
                                        #find lengths of controls and cases
        Tcontrols<-NROW(controls)
                                        # Setnames because data.table called from function
        ## setnames(controls,cols)
        setkeyv(controls,c(event,"pnrnum"))
                                        # Define cases in selected match-group
        isCase <- controls[[event]]==1
        cases <- controls[isCase]
                                        #find lengths of cases
        setkey(cases,pnrnum)
        Ncases<-NROW(cases)
                                        # Remove those where exposre window is not sufficient - only relevant when an exposure window is defined along with a start.date
        if(exposure.window>0){
            cases <- cases[(cases[[case.index]]-cases[[start.date]])>exposure.window]
        }
        if (Ncases==0){return(NULL)}
                                        #dateterm matrix
        if(!is.null(date.terms)){
            dates.cases <- as.matrix(cases[,.SD,.SDcols=date.terms])
            dates.controls <- as.matrix(controls[,.SD,.SDcols=date.terms])
        }
        else {
            dates.cases <- as.matrix(0)
            dates.controls <- as.matrix(0)
        }
        if (exposure.window>0) start.date <- controls[[start.date]]
        else start.date <-0
        set.seed(seed)
        Output <- Matcher(n.controls,
                            Tcontrols,
                            Ncases,
                            exposure.window,
                            start.date,
                            controls[[end.followup]],
                            cases[[case.index]],
                            controls[["pnrnum"]],
                            cases[["pnrnum"]],
                            Ndate.terms,
                            dates.cases,
                            dates.controls)

        setDT(Output)
        if (progressbar){
            progress <- progressValues[sub]
            #Progress bar
            setTxtProgressBar(pb,value=progress)
            flush(stdout())
        }
        Output
    }) # end function and do.call
    if (cores>1){
        parallel::stopCluster(CLUST)
        setDT(selected.controls)
    }  #end cores>1
    setnames(selected.controls, c(case.id, "pnrnum"))
    selected.controls[,event:= 0]
    setkey(alldata, event)
    setnames(alldata,event,"event")
    cases <- alldata[event == 1, .SD,.SDcols=c("pnrnum","event")]
    setnames(alldata,"event",event)
    cases[, `:=`(case.id, pnrnum)]
    setnames(cases,"case.id",case.id)
    FINAL <- rbind(cases, selected.controls)
    setkey(FINAL)
    #output
    datt[,(event):=NULL]
    FINAL <- merge(FINAL,datt,by="pnrnum")
    FINAL[,c("cterms","pnrnum"):=NULL] # remove cterms - aggregated terms
    setkeyv(FINAL,c(case.id,"event"))
    #Add relevant case.id to controls
    FINAL[,eval(case.index):=.SD[.N],.SDcols=c(case.index),by=case.id]
    setnames(FINAL,"event",event)
    cat("\n")
    FINAL[]
}
