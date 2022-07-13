##' Processing function, to perform calculations according to the data and variables specified in preprocessing object.
##'
##' @title Medicin macro to estimate prescription lengths and averages
##' @param drugs A named list of drugs. Each element of this list should be a list
##' with the following elements:
##' \itemize{
##'   \item{atc}{A vector of ATC codes which should match the components of the drug exactly.}
##'   \item{maxdepot} The maximum total dose that a single patient can possibly stack.
##'   \item{period} A vector of dates to limit the period in which to estimate the daily dose.
##'   \item{prescriptionwindow} Default is 2.
##'   \item{doses}
##'    A named list with the elements \code{value}, \code{min}, \code{max} and \code{def}.
##'    Here \code{value} is a vector of strengths of one
##'    unit (e.g., pill) of the drug. The vector should have one such strength for
##'    each of the different packages that occur in the data.
##' \code{min} is a vector of the same length as \code{value} where each element is the
##' assumed minimum total dosis that a patient can consume on one day. For example, if the
##' value is 50 mg and the pills of this strength can be cut (in halves) the minimum
##' total dosis is 25 mg.
##' \code{max} is a vector of the same length as \code{value} where each element is the
##' assumed maximum total dosis that a patient can consume on one day. For example, if the
##' value is 50 mg and one can at most consume 4 pills a day the maximum
##' total dosis is 200 mg.
##' \code{def} is a vector of the same length as \code{value} where each element is the
##' assumed default dosis that an average patient would consume on one day. For example,
##' if the value is 50 mg and usually a patient would consume 2 pills the default is 100 mg.
##' }
##' (see examples).
##' @param drugdb data.table with (subset of) medical drugs registry
##' @param admdb data.table with (subset of) hospital admission registry. The data.table should be
##' prepared such that it contains only overnight hospital stay (i.e., pattype=2) and non-overlapping
##' hospital stay periods, i.e., as obtained with \code{getAdmLimits} (SAS-AKA: code-from-hell).
##' @param datesdb data.table with individual periods or time points of interest.
##' Only used when the \code{type} argument is different from \code{"dynamic"}.
##' Should contain one start and end date for each individual when \code{type} equal to \code{"period"} 
##' and just one date for each individual when \code{type} equal to \code{"cross-sectional"} 
##' (see examples).
##' @param type string, specifying which analysis to perform.
##' Can be \code{"dynamic"}, \code{"period"}, or \code{"cross-sectional"}. Default is \code{"dynamic"}.
##' (see examples).
##' @param drugdb.datevar name of the date variable in \code{drugdb}. Default is \code{"eksd"}.
##' @param admdb.datevars names of the date variables in \code{admdb}. Default is \code{c("inddto","uddto")}.
##' @param datesdb.datevars name(s) of the date variables in \code{datesdb}. 
##' Should be a single string when \code{type}=\code{"cross-sectional"} and a vector of two string when
##' \code{type}=\code{"period"}. Default is \code{"time"} in the first case and \code{c("start", "end")} in the second.
##' @param method Which method to use to estimate the daily dose. Can be \code{"default.doses"} or
##' \code{"number.of.days"}. Default is \code{"default.doses"}.
##' See documentation for more details.
##' Only used when the \code{type} is different from \code{"dynamic"}.
##' @param cap.values Boolean, specifying whether or not to restrict the estimated daily dose to lie
##' within the specified minimum and maximum dose. Default is \code{TRUE}
##' Only used when the \code{type} is different from \code{"dynamic"}.
##' @param window
##' Only used when the \code{type} = \code{"cross-sectional"}.
##' Specify how many days back from the given time point we should go to find purchases to estimate the daily dose.
##' @param pre.window
##' Only used when the \code{type} = \code{"period"}.
##' Specify how many days back from the individual end periods we should go to find purchases to estimate the
##' daily dose for the first part of the periods. 
##' @param id Name of variable to identify individuals. Default is
##'     \code{"pnr"}.
##' @param strength.var Name of variable to identify strength. Default
##'     is \code{"strnum"}.
##' @param packsize.var Name of variable to identify size of
##'     packages. Default is \code{"packsize"}.
##' @param atc.var Name of variable to identify drugs. Default is \code{"atc"}.
##' @param apk.var Name of variable to identify number of
##'     packages. Default is \code{"apk"}.
##' @param splitting Split the data into 10 chunks to estimate
##'     remaining time (and possibly speed up computation).
##' @param verbose bla bla?
##' @author Helene Charlotte Rytgaard, Anders Munch and Thomas Alexander Gerds based
##'     on the famous SAS macro by Christian Tobias Torp-Pedersen
##' @examples
##' set.seed(05021992)
##' library(data.table)
##' N=20
##' packs = list("R03AK11"=list(c(750,75),c(500,200),c(400,200)),
##'              "R03AL03"=list(c(750,75),c(500,200),c(400,200)),
##'              "C01CA01"=list(c(200,100),c(750,30)))
##' lmdb=simPrescriptionData(N,packages=packs)
##' lpr=simAdmissionData(N)
##' lpr <- getAdmLimits(lpr,collapse=TRUE)
##' drug1 = list(atc=c("R03AK11","R03AL03","R03AC02","R03AC04","R03AC19",
##'                     "R03AL02","R03AA01","R03AC18","R03AL01"),
##'              maxdepot=4000,
##'              period=as.Date(c("1995-01-01", "2012-12-31")),
##'              prescriptionwindow=2,
##'              doses=list(value=c(750,500,400,200,75),
##'                         min = c(250,200,200,100,25),
##'                         max = c(1000,600,800,600,100),
##'                         def = c(750,500,400,200,75)))
##' drug2=list(atc=c("C01CA01","C01AA05"),
##'            maxdepot=4000,
##'            period=as.Date(c("1995-01-01", "2012-12-31")),
##'            prescriptionwindow=2,
##'            doses=list(value=c(200, 400, 500,750),
##'                       min = c(100, 200, 250,750),
##'                       max = c(300, 800, 1000,750),
##'                       def = c(200, 400, 500,750)))
##' x=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr)
##' x$drug1
##'
##' ## Examples with other types
##' ## NB: Calculations with maxdepot>0 is not implemented yet for type different from "dynamic".
##' ## Not specifying maxdepot is the same at setting maxdepot=0.
##' library(data.table)
##' drug1 = list(atc=c("R03AK11","R03AL03","R03AC02","R03AC04","R03AC19",
##'                     "R03AL02","R03AA01","R03AC18","R03AL01"),
##'              doses=list(value=c(750,500,400,200,75),
##'                         min = c(250,200,200,100,25),
##'                         max = c(1000,600,800,600,100),
##'                         def = c(750,500,400,200,75)))
##' drug2=list(atc=c("C01CA01","C01AA05"),
##'            doses=list(value=c(200, 400, 500,750),
##'                       min = c(100, 200, 250,750),
##'                       max = c(300, 800, 1000,750),
##'                       def = c(200, 400, 500,750)))
##' ## Note that we define the periods separately, as they are now individual:
##' ind.periods <- data.table(pnr=1:N,
##'                           start=rep(as.Date("1996-01-01"), N)+floor(rnorm(N,sd=100)),
##'                           end=rep(as.Date("1998-05-30"), N)+floor(rnorm(N,sd=100)))
##' z=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr,
##'                type="period",datesdb=ind.periods)
##' z$drug1
##' ## Using different method:
##' z=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr,
##'                type="period",datesdb=ind.periods, method="number.of.days")
##' z$drug1
##'
##' ## Using type = "cross-sectional" to check if individuals are exposed at a given time
##' ## Note that we now only give one timepoint but then also need to supply the window argument
##' ind.periods2 <- copy(ind.periods)
##' ind.periods2[,time:=start][,c("start","end"):=NULL]
##' y=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr,
##'                type="cross-sectional",datesdb=ind.periods2, window=365.25)
##' y$drug1
##' ## Changing the window size of course affects our estimates:
##' y=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr,
##'                type="cross-sectional",datesdb=ind.periods2, window=10)
##' y$drug1
##' ## Above, we renamed the date variables before plugging it in, but we could also have used the
##' ## datesdb.datevars argument:
##' y=medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr,
##'                type="cross-sectional",datesdb=ind.periods, window=365.25,
##'                datesdb.datevars="start")
##' y$drug1
##' @export
medicinMacro <- function(drugs,
                         drugdb,
                         admdb,
                         datesdb,
                         type="dynamic",
                         drugdb.datevar="eksd",
                         admdb.datevars=c("first.indate","last.outdate"),
                         datesdb.datevars,
                         method,
                         cap.values,
                         window,
                         pre.window,
                         id = "pnr",
                         strength.var = "strnum",
                         packsize.var="packsize",
                         atc.var="atc",
                         apk.var="apk",
                         splitting = FALSE,verbose=FALSE){
    atc=eksd=inddto=uddto=tmp.index=.N=pnr=B=E=exposure.days=lastday=firstday=pnr.db=NULL
    ## Check id variables
    id.character.drug <- typeof(drugdb[[id]])=="character"
    if (NROW(admdb)>0){
        id.character.adm <- typeof(admdb[[id]])=="character"
        if (id.character.adm != id.character.drug)
            stop("The id variable ",id," should either both be character or both be numeric in input data sets: drugdb and admdb.")
    }
    ## Check date variables

    ## Check pattype
    
    ## Check type argument:
    if(!(type %in% c("dynamic", "period", "cross-sectional")))
        stop("Choose either type = \"dynamic\", \"period\" or \"cross-sectional\".")
    ## Check datesdb is present and correctly setup for type other than "dynamic":
    if(type!="dynamic"){
        if(missing(datesdb))
            stop(paste("datesdb needs to be provided when using type =", type))
        datesdb.work <- copy(datesdb)
        ## Change names of the datesdb
        if (id!="pnr") setnames(datesdb.work,id,"pnr")
        ## Set default values for relevant arguments
        if(missing(method)) method <- "default.doses"
        if(missing(cap.values)) cap.values <- TRUE
        if (type=="period"){
            if (missing(datesdb.datevars))
                datesdb.datevars <- c("start", "end")
            else
                if(length(datesdb.datevars)!=2)
                    stop("When using type=\"period\", there should be two date variable names given in datesdb.datevars")
            if(any(!(datesdb.datevars %in% names(datesdb))))
                stop(paste0("Variables \"", datesdb.datevars[1], "\" and/or \"", datesdb.datevars[2], "\" not found in datesdb"))
            setnames(datesdb.work,datesdb.datevars[1],"start")
            setnames(datesdb.work,datesdb.datevars[2],"end")
            if(missing(pre.window)) pre.window <- 365.25
            ## Warn against what arguments are not used
            if(!(missing(window)))
                warning(paste0("The argument window is ignored when type=\"period\"."))
        }
        if (type=="cross-sectional"){
            if (missing(datesdb.datevars))
                datesdb.datevars <- "time"
            else
                if(length(datesdb.datevars)!=1)
                    stop("When using type=\"period\", there should be one date variable name given in datesdb.datevars")
            if(any(!(datesdb.datevars %in% names(datesdb))))
                stop(paste0("Variables \"", datesdb.datevars, "\" not found in datesdb"))
            setnames(datesdb.work,datesdb.datevars,"time")
            if(missing(window)) stop("window needs to be given when type = \"cross-sectional\".")
            ## Warn against what arguments are not used
            if(!(missing(pre.window)))
                warning(paste0("The argument pre.window is ignored when type=\"cross-sectional\"."))
        }        
    }else{
        ## Warn against what arguments are not used
        if(!(missing(datesdb) & missing(method)  & missing(cap.values) & missing(window) & missing(pre.window)))
            warning(paste0("The arguments datesdb, method, cap.values, window, and pre.window are ignored when type=\"dynamical\"."))
    }
    ## Set the right structure for processed object
    processed <- structure(list(),class = "medicinmacro")
    if (missing(drugs) || is.null(drugs)) stop("Sorry, no drugs have been specified.")
    if (missing(drugdb) || is.null(drugdb)) stop("No drug purchase data provided")
    if (NROW(admdb)>0){
        if (any(dups <- duplicated(admdb[,c(id,admdb.datevars),with=FALSE]))){
            stop(paste0("Admission data argument 'admdb' has not been prepared correctly:\nThere are duplicated (and/or overlapping) admission periods in at least one person."))
        }
        ptype <- grep("pattype",names(admdb),ignore.case=TRUE,value=TRUE)
        if ((length(ptype)>0) && any(admdb[[ptype[[1]]]]!=0)){
            warning("Admission data argument 'admdb' contains admissions that are not overnight hospital admissions, i.e., pattype!=0.")
        }
        admdb.work <-  copy(admdb)
        if (id!="pnr") setnames(admdb.work,id,"pnr")
        if (any(admdb.datevars!=c("inddto","uddto"))) {
            setnames(admdb.work,admdb.datevars[1],"inddto")
            setnames(admdb.work,admdb.datevars[2],"uddto")
        }
    }
    for (drugname in names(drugs)){
        ## treatfun <- function(drugname) {
        drugdb.work <-  copy(drugdb)
        if(NROW(admdb)>0)        
            admdb.work.drug <- copy(admdb.work)
        else
            admdb.work.drug = NULL
        if (id!="pnr") setnames(drugdb.work,id,"pnr")
        if (strength.var!="strnum") setnames(drugdb.work,strength.var,"strnum")
        if (packsize.var!="packsize") setnames(drugdb.work,packsize.var,"packsize")
        if (apk.var!="apk") setnames(drugdb.work,apk.var,"apk")
        if (atc.var!="atc") setnames(drugdb.work,atc.var,"atc")
        if (drugdb.datevar!="eksd") setnames(drugdb.work,drugdb.datevar,"eksd")
        j            <- (1:length(drugs))[names(drugs) == drugname]
        atcs         <- unlist(drugs[[j]]$atc)
        doses        <- drugs[[j]]$doses
        maxdepot     <- drugs[[j]]$maxdepot
        drugdb.work   <- drugdb.work[atc %in% atcs, ]
        # Quick fix to change pnr to integer if needed (assuming the id-val names are "pnr" for both dt)
        if(id.character.drug){
            db = data.table(pnr.db=unique(drugdb.work$pnr))
            db[,tmp.index:=1:.N]
            drugdb.work = merge(drugdb.work,db,by.x="pnr",by.y="pnr.db", all.x=TRUE)[,pnr:=tmp.index][,tmp.index:=NULL][]
            if(NROW(admdb)>0)
                admdb.work.drug = merge(admdb.work.drug,db,by.x="pnr",by.y="pnr.db")[,pnr:=tmp.index][,tmp.index:=NULL][]
            ## now continuing with numeric id (changing back in the end)
        }
        ## Check if there is sufficient drug info for the data:
        dosesmissing <- !(unique(drugdb.work[[strength.var]]) %in% doses$value)
        if (any(dosesmissing)) {
            stop(paste0(paste0("Missing doses (min,max,def) for ",drugname,": ",
                               paste(unique(drugdb.work[[strength.var]])[dosesmissing],collapse=", ")),"\n"))
        }
        switch(type,
               "cross-sectional"={
                   if(is.null(maxdepot))
                       maxdepot <- 0
                   if(maxdepot!=0)
                       warning(paste("\nMethod for calculating with maxdepot > 0 for type different from \"dynamic\" is not implemented yet, so returns NULL for", drugname))
                   else
                       processed[[drugname]] <- mm1(drugs=drugs[[drugname]],
                                                    drugdb=drugdb.work,
                                                    admdb=admdb.work.drug,
                                                    time.points=datesdb.work,
                                                    method=method,
                                                    stash=maxdepot,
                                                    cap.values=cap.values,
                                                    window=window,
                                                    verbose=verbose)
               },
               "period"={
                   if(is.null(maxdepot))
                       maxdepot <- 0
                   if(maxdepot!=0)
                       warning(paste("\nMethod for calculating with maxdepot > 0 for type different from \"dynamic\" is not implemented yet, so returns NULL for", drugname))
                   else
                       processed[[drugname]] <- mm2(drugs=drugs[[drugname]],
                                                    drugdb=drugdb.work,
                                                    admdb=admdb.work.drug,
                                                    periods=datesdb.work,
                                                    method=method,
                                                    stash=maxdepot,
                                                    cap.values=cap.values,
                                                    pre.window=pre.window,
                                                    verbose=verbose)
               },
               "dynamic"={
                   ## Run some extra tests for this method 
                   period       <- drugs[[j]]$period    
                   prescriptionwindow <- drugs[[j]]$prescriptionwindow
                   ## Restrict data to period for the drug
                   drugdb.work   <- drugdb.work[eksd <= period[2] & eksd >= period[1], ]
                   ##--- unique id's
                   idunique <- unique(drugdb.work[["pnr"]])
                   if (length(idunique)==0) { ## Check if there is any data to work on
                       warning(paste0("No individual purchased ",paste0(atcs,collapse=", ")," in this period"))
                       processed[[drugname]] <- NULL
                   }else{
                       if (NROW(admdb)>0){
                           admdb.work.j   <- admdb.work.drug[inddto<= period[2] & uddto >= period[1], ]
                       }else{
                           admdb.work.j <- data.table(pnr=numeric(0),inddto=numeric(0),uddto=numeric(0))
                       }
                       if (length(prescriptionwindow) == 0) prescriptionwindow = 2
                       if (length(maxdepot) == 0) stop("Argument max depot missing\n")
                       if (length(period) == 0) stop("Argument period missing\n")
                       ## convert dates to numeric: number of days since 1995-01-01
                       drugdb.work[,eksd:=as.numeric(eksd-as.Date("1995-01-01"))]
                       if (NROW(admdb.work.j)>0){
                           admdb.work.j[,inddto:=as.numeric(inddto-as.Date("1995-01-01"))]
                           admdb.work.j[,uddto:=as.numeric(uddto-as.Date("1995-01-01"))]
                       }
                       ## Start calculation with the innerMedicinMacro (cpp-function)
                       if(length(idunique)>10000 & splitting==TRUE){
                           stop("Split implementation pending right now")
                           ind.split <- cut(as.numeric(factor(idunique)), 10, labels =FALSE)
                           out.list <- vector("list", 10)
                           est.time <- 0
                           for (split in 1:10){
                               split.id <- idunique[ind.split==split]
                               setkeyv(drugdb.work,"pnr")
                               out.list[[split]] <- rbindlist(innerMedicinMacro(dat=drugdb.work[split.id],
                                                                                admdat=admdb.work.j,
                                                                                doses=doses,
                                                                                idunique=split.id,
                                                                                prescriptionwindow=prescriptionwindow,
                                                                                maxdepot=maxdepot,verbose=verbose))
                           }
                           out <- rbindlist(out.list)
                       }else{
                           ## Setup index data first
                           setorder(drugdb.work, "pnr")
                           drugdb.work[,index:=1:.N]
                           index.drug <- drugdb.work[, .(id.start=index[1], id.end=index[.N]), by=pnr]
                           admdb.work.j <- admdb.work.j[pnr %in% idunique] ## NB: Check how long this takes...!
                           setorder(admdb.work.j, "pnr")
                           admdb.work.j[,index:=1:.N]
                           index.adm <- admdb.work.j[, .(id.start.adm=index[1], id.end.adm=index[.N]), by=pnr]
                           index <- merge(index.drug, index.adm, by="pnr", all.x=1)
                           index[is.na(id.start.adm), ":="(id.start.adm=0, id.end.adm=-1)] ## Hack to make cpp generate empty vector instead of 0 scalar
                           for (j in c("id.start", "id.end", "id.start.adm", "id.end.adm")) set(index, j=j, value=index[[j]] -1) ## Convert to C++ indexing
                           out <- rbindlist(innerMedicinMacro(dat=drugdb.work,
                                                              admdat=admdb.work.j,
                                                              doses=doses,
                                                              idunique=as.numeric(idunique),
                                                              index=index,
                                                              prescriptionwindow=prescriptionwindow,
                                                              maxdepot=maxdepot,verbose=verbose))
                           setnames(out,"X","dose")
                           out[,B:=as.Date("1995-01-01")+B] 
                           out[,E:=as.Date("1995-01-01")+E]
                           setnames(out,"B","firstday")
                           setnames(out,"E","lastday")
                           out[,exposure.days:=lastday-firstday]
                           setkey(out,pnr,firstday)
                       }
                       ## Revert pnr type change
                       if(id.character.drug)
                           out = merge(out,db,by.x="pnr",by.y="tmp.index",all.x=TRUE)[,pnr:=pnr.db][,pnr.db:=NULL][]
                       processed[[drugname]] <- out[]
                   }
               })
    }
    processed
}

