## workhorse for incidenceMatch and exposureMatch
riskSetMatch <- function(ptid     # Unique patient identifier
                        ,event   # 0=Control, 1=case
                        ,terms   # terms c("n1","n2",...) - list of variables to match by
                        ,data     # dataset with all variables
                        ,n.controls  # number of controls to provide
                        ,case.index=NULL      # Integer or date, date where controls must be prior
                        ,end.followup=NULL   # end.followup - Index date for controls
                        ,date.terms=NULL # character list of date variables
                        ,duration.terms=NULL # Duration of a minimal exposure window for the condition defined by start.date
                        ,output.count.controls=TRUE # add number of controls
                        ,cores=1 # Number of cores to use, default 1
                        ,seed # Seed for random sort
                        ,progressbar=TRUE
                         ){
    .SD=cterms=tEmP.iD=.N=miscol=case.id=NULL
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
    if(!is.integer(cores) & !(is.numeric(cores))) stop("cores must be integer, default 1")
    cores <- as.integer(cores)
    ## copy input data
    DATA <- copy(data)
    setDT(DATA)
    ##Check that patient IDs are unique:
    if ((repetitians <- sum(duplicated(DATA[["ptid"]])))>0)
        stop(paste(" Error, participant ID not unique. Number of repeats:",repetitians))
    DATA[,tEmP.iD:=1:.N]
    ## combine matching variables to single term - cterms
    miscol <- DATA[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = terms ]
    if (sum(miscol)>0) stop("Data columns corresponding to 'terms' have missing values")
    DATA[,cterms :=interaction(.SD,drop=TRUE),.SDcols=terms]
    ## Select relevant part of table for matching - and provide internal names
    cols <-c("tEmP.iD",event,"cterms",case.index,end.followup)
    if (!is.null(date.terms)){
        cols <- c(cols,date.terms)
    }
    n.duration.terms <- length(duration.terms)
    if (n.duration.terms>0) {
        start.vars <- sapply(duration.terms,function(x){x[["start"]]})
        if (!is.character(start.vars)) stop("All duration terms need to have an element named 'start' which provides the name of the variable that indicates the start of the duration.")
        duration.min <- sapply(duration.terms,function(x){x[["min"]]})
        duration.min <- as.double(duration.min)
        if (any(is.na(duration.min))) stop("Parsing error in argument duration.min which has to be convertable to numeric.")
        cols <- c(cols,start.vars)
    }else{
        start.vars=NULL
    }
    work.data <- DATA[,.SD,.SDcols=cols]
    setcolorder(work.data,cols) # ensure correct order of columns
    ##conversion to integer of all date.terms
    for (v in c(case.index,end.followup,date.terms,start.vars)){
        set(work.data,j=v,value=as.double(work.data[[v]]))
    }
    ## Cases can only be controls before being a case, hence the case index date limits the followup
    work.data[[end.followup]] <- pmin(work.data[[case.index]],work.data[[end.followup]],na.rm=TRUE)
    efup <- work.data[[end.followup]]
    if (any(is.na(efup)))stop("Missing values in end.followup variable. ")
    beyond.maxtime <- max(efup)+1
    ## replace missing dates with maxtime
    ## na.replace <- function(v,value){v[is.na(v)]=value;v}
    for (v in c(case.index,end.followup,date.terms)){
        work.data[is.na(get(v)),(v):=beyond.maxtime]
    }
    if(!is.null(date.terms)) {
        ##Number of date.terms
        n.date.terms=length(date.terms)
    }else {
        n.date.terms <- 0
    }
    ## prepare to split
    setkey(work.data,cterms)
    ## make sure every case has a date
    ## FIXME could define event as a non missing caseindex
    ## work.data <- work.data[work.data[[event]]==0 | !is.na(work.data[[case.index]])]
    split.work.data <- split(work.data,by="cterms") # Now a list aplit by cterms
    message("\nMatching terms define ",length(split.work.data)," subsets")
    Nsub <- sapply(split.work.data,NROW)
    progressValues <- cumsum(Nsub)
    if ((Nsmallgroups <- sum(Nsub<n.controls))>0)
        message("Matching terms split the data into ",length(split.work.data)," subsets",
                "\nThere are ",Nsmallgroups," subsets which contain less subjects (case-control-mix)\n",
                "than the number of requested controls for one case.\n")
    if ((Ntinygroups <- sum(Nsub<2))>0){
        message("There are ",Ntinygroups," subsets that have less than two subjects\nIMPORTANT: These are ignored with side effect.\n")
        message("You should reduce",
                "\n  - the number matching variables\n  - the number of values of the matching variables\n  - or both.\n")
    }
    if (cores>1){
        CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
    }else{
        foreach::registerDoSEQ
    }
    ## Prepare progress bar
    if (progressbar){
        totalprogress <-as.numeric(length(split.work.data)/1000)
        pb <-txtProgressBar(min = 0,
                            max = NROW(work.data),
                            initial = 0,
                            char = "'",
                            width = 20,
                            style = 3,
                            file = "")
        progress <- 0
    }
                                        # Select controls - rbind of each split-member that selects controls
    `%dopar%` <- foreach::`%dopar%`
    ## selected.controls <- do.call(rbind,foreach::foreach(controls=split.work.data,.packages=c("heaven")) %dopar% {
    selected.controls <- do.call(rbind,foreach::foreach(sub=1:length(split.work.data),.packages=c("heaven")) %dopar% {
        controls=split.work.data[[sub]]
        ## find lengths of controls and cases
        Tcontrols<-NROW(controls)
        ## Setnames because data.table called from function
        setkeyv(controls,c(event,"tEmP.iD"))
        ## Define cases in selected match-group
        isCase <- controls[[event]]==1
        cases <- controls[isCase]
        ##find lengths of cases
        setkey(cases,tEmP.iD)
        Ncases<-NROW(cases)
        if (Ncases==0){return(NULL)}
        ## dateterm matrix
        if(n.date.terms>0){
            dates.cases <- as.matrix(cases[,.SD,.SDcols=date.terms])
            dates.controls <- as.matrix(controls[,.SD,.SDcols=date.terms])
        } else {
            dates.cases <- as.matrix(0)
            dates.controls <- as.matrix(0)
        }
        ## durationterm matrix
        if(n.duration.terms>0){
            duration.start.cases <- as.matrix(cases[,.SD,.SDcols=start.vars])
            duration.start.controls <- as.matrix(controls[,.SD,.SDcols=start.vars])
        } else {
            duration.start.cases <- as.matrix(0)
            duration.start.controls <- as.matrix(0)
            duration.min <- double(1)
        }
        if (!missing(seed)) set.seed(seed)
        Output <- Matcher(n.controls,
                          Tcontrols,
                          Ncases,
                          as.double(controls[[end.followup]]),
                          as.double(cases[[case.index]]),
                          controls[["tEmP.iD"]],
                          cases[["tEmP.iD"]],
                          n.date.terms,
                          dates.cases,
                          dates.controls,
                          n.duration.terms,
                          duration.start.cases,
                          duration.start.controls,
                          duration.min)
        setDT(Output)
        if (progressbar){
            progress <- progressValues[sub]
            ##Progress bar
            setTxtProgressBar(pb,value=progress)
            flush(stdout())
        }
        Output
    }) # end function and do.call
    if (cores>1){
        parallel::stopCluster(CLUST)
        setDT(selected.controls)
    }  #end cores>1
    if (progressbar) cat("\n") 
    setnames(selected.controls, c("case.id", "tEmP.iD"))
    ## prepare cases for rbind
    has.case <- work.data[[event]] == 1
    cases <- work.data[has.case,data.table::data.table(case.id=tEmP.iD,tEmP.iD=tEmP.iD,event=1 )]
    FINAL <- rbind(cases, cbind(selected.controls,event=0))
    setnames(FINAL,"event",event)
    ## merge with data
    DATA[,c("cterms"):=NULL] # remove cterms
    DATA[,(event):=NULL]
    FINAL <- merge(FINAL,DATA,by="tEmP.iD")
    FINAL[,tEmP.iD:=NULL]
    setkeyv(FINAL,c("case.id",event))
    ## Add case.index to controls
    FINAL[,eval(case.index):=.SD[.N],.SDcols=c(case.index),by=case.id]
    if (output.count.controls){
        FINAL[,n.controls:=(.N-1),by=case.id]
    }
    ## FINAL[,case.id:=NULL]   
    attr(FINAL,"id") <- ptid
    attr(FINAL,"event") <- event
    class(FINAL) <- c("riskSetMatch",class(FINAL))
    FINAL[]
}






