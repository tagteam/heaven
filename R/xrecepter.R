##' wrapper for medicin macro
##'
##' wrapper for medicin macro
##' @title wrapper for medicin macro
##' @param drugdb drugdb data
##' @param adm admission data
##' @param atc atc kode specifying drugdb of interest
##' @param window number of purchases that enter the calculations
##' @param value is a vector of strengths of one
##'    unit (e.g., pill) of the drug. The vector should have one such strength for
##'    each of the different packages that occur in the data.
##' @param min is a vector of the same length as \code{value} where each element is the
##' assumed minimum total dosis that a patient can consume on one day. For example, if the
##' value is 50 mg and the pills of this strength can be cut (in halves) the minimum
##' total dosis is 25 mg.
##' @param max is a vector of the same length as \code{value} where each element is the
##' assumed maximum total dosis that a patient can consume on one day. For example, if the
##' value is 50 mg and one can at most consume 4 pills a day the maximum
##' total dosis is 200 mg.
##' @param def is a vector of the same length as \code{value} where each element is the
##' assumed default dosis that an average patient would consume on one day. For example,
##' if the value is 50 mg and usually a patient would consume 2 pills the default is 100 mg.
##' @param maxdepot Maximum number of units that a patient can stash
##' @param period Vector of two dates that SAS can eat: c("'01jan1995'd","'31dec2015'd").
##' Can also be numeric values: number of days since 1960-01-01.
##' @param wd working directory
##' @param sas.program path to sas program
##' @param sas.switches On linux this defaults to {""} on any other
##'     system to \code{"-batch -nosplash -noenhancededitor -sysin"}
##' @param sas.runner How sas is invoked. On linux this defaults to
##'     \code{"system"} on any other system to \code{"shell"}.
##' @param macro path to sas macro
##' @param save.tmp if TRUE do not delete the temporary SAS files 
##' @param name working name of output file (only relevant when save.tmp is set) 
##' @param verbose like bla bla?
##' @param server name of remote (unix like) machine 
##' @param user user name on remote (unix like) machine 
##' @param remote if TRUE run on a remote (unix like) machine
##' @param remote.home home folder on remote (unix like) machine
##' @examples
##' library(heaven)
##' \dontrun{
##'  library(heaven)
##' library(data.table)
##' set.seed(05021992)
##' N=18
##' packs = list("R03AK11"=list(c(10,1)))
##' lmdb=simPrescriptionData(N,packages=packs,max.packages=1)
##' lpr=simAdmissionData(N)
##' ## very simple data 
##' lmdb <- lmdb[c(1,4,18)]
##' R03 = list(atc=c("R03AK11"),
##'           maxdepot=100,
##'           period=as.Date(c("1995-01-01", "2012-12-31")),
##'            prescriptionwindow=2,
##'           doses=list(value=c(5,10),
##'                      min = c(.5,.5),
##'                      max = c(2,2),
##'                      def = c(1,1)))
##' xrecepter(drugdb=lmdb,adm=lpr,window=5,remote=FALSE,wd="~/tmp",save.tmp=0L,
##'           value=rep(R03$doses$value,2),min=rep(R03$doses$min,2),max=rep(R03$doses$max,2),
##'           def=rep(R03$doses$def,2),maxdepot=100,period=c(12784,20089),atc="R03AK11",
##'           name="blaupill",sas.program="/usr/local/bin/sas",
##'           verbose=FALSE)
##' } 
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
xrecepter <- function(drugdb,
                      adm,
                      atc,
                      window=5,
                      value,
                      min,
                      max,
                      def,
                      maxdepot,
                      period=c("'01jan1995'd","'31dec2015'd"),
                      name="adrug",
                      user,
                      wd,
                      sas.program="/usr/local/bin/sas ",
                      sas.switches,
                      sas.runner,
                      macro="~/research/SoftWare/heaven/sas/medicin_macro_version_31.sas",
                      save.tmp=FALSE,
                      verbose=TRUE,
                      server="doob",
                      remote=FALSE,
                      remote.home){
    if (remote & missing(remote.home)) stop("Need directory remote.home to run SAS on remote server")
    if (missing(value) || length(value)!=4) stop("argument value should be a vector of length 4")
    if (missing(min) || length(min)!=4) stop("argument min should be a vector of length 4 (minimal dosages)")
    if (missing(max) || length(max)!=4) stop("argument max should be a vector of length 4 (maximal dosages)")
    if (missing(def) || length(def)!=4) stop("argument def should be a vector of length 4 (default dosages)")
    .SD = NULL
    olddir <- getwd()
    if (missing(wd) || length(wd) == 0){
        wd <- getwd()
    }else{
        setwd(wd)
    }
    tmpname <- basename(tempfile(pattern="heaven_tempSASfiles"))
    tmpdir = paste0(wd,"/",tmpname)
    # cleaning up old temporary directories
    olddirectories <- list.files(wd,pattern="heaven_tempSASfiles[a-z0-9]+")
    for (old in olddirectories){
        if (verbose)
            message("Cleaning up temporary directories from previous calls.")
        unlink(old,recursive=TRUE,force=TRUE)
    }
    if (verbose) message("Writing temporary SAS files (log, lst, input data, output data) to directory\n",tmpdir)
    if (file.exists(tmpdir)) {
        stop(paste("file.exists:",tmpdir))
    }else{
        try.val <- try(dir.create(tmpdir))
        if (class(try.val)[[1]]=="try-error")
            stop("Cannot create temporary directory.\nYou probably do not have permission to write to the directory: \n ",
                 tmpdir, "\nTry to specify another directory with the argument \"wd\" or change the working directory.",
                 sep = "")
    }
    setwd(tmpdir)
    on.exit({
        setwd(olddir)
        if (!save.tmp) {
            unlink(tmpdir,recursive=TRUE,force=TRUE)
            if (remote){
                system(paste0(
                    paste0("ssh -Y ",
                           user,
                           "@",
                           server,
                           " 'rm -rf ",sub("~",remote.home,tmpdir),"'")))
            }
        }else{
            cat("\nTemporary directory:\n ",tmpdir,"\ncan now be inspected -- and should be removed manually afterwards!\n")
        }
    })
    if (.Platform$OS.type == "unix") {
        if (missing(sas.program)) {
            sas.program <- "sas"
        }
        if (missing(sas.switches)) {
            sas.switches <- ""
        }
        if (missing(sas.runner)) {
            sas.runner <- "system"
        }
    }
    else {
        if (missing(sas.program)) {
            sas.program <- "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe"
        }
        if (missing(sas.switches)) {
            sas.switches <- "-batch -nosplash -noenhancededitor -sysin "
        }
        if (missing(sas.runner)) {
            sas.runner <- "shell"
        }
    }
    proFile=paste0(tmpdir,"/med-macro-call.sas")
    drugFile=paste0(tmpdir,"/med-macro-drug.csv")
    admFile=paste0(tmpdir,"/med-macro-adm.csv")
    outFile=paste0(tmpdir,"/med-macro-out.csv")
    logfile=paste0(tmpdir,"/med-macro.log")
    lstfile=paste0(tmpdir,"/med-macro.lst")
    ## Prog <- path.expand(proFile)
    ## Out <- path.expand(outFile)
    Prog <- proFile
    ## Out = outFile
    Out <- paste0(tmpdir,"/",name,"_alt.sas7bdat")
    ## if(file.exists(Out)) file.remove(Out)
    data.table::fwrite(drugdb,sep=";",file=drugFile)
    data.table::fwrite(adm,sep=";",file=admFile)
    cat("libname mm '",tmpdir,"';\n",file=Prog,append=FALSE,sep="")
    cat("%include \'",macro,"\';\n\n",file=Prog,append=TRUE,sep="")
    cat("proc import out=drugdb
            datafile= \'",drugFile,"\'
            dbms=csv replace;
     delimiter=';';
     getnames=yes;
     datarow=2;
     run;\n",file=Prog,append=TRUE,sep="")
    cat("proc import out=admdata
            datafile=\'",admFile,"\'
            dbms=csv replace;
     delimiter=';';
     getnames=yes;
     datarow=2;
     run;\n",file=Prog,append=TRUE,sep="")
    cat("\n
    Options mprint;\n
    proc contents data=admdata;
     run;\n",file=Prog,append=TRUE,sep="")
    cat("\nproc contents data=drugdb;
     run;\n",file=Prog,append=TRUE,sep="")
    cat("proc transpose data=admdata out=indato prefix=inddto;
    by pnr; var inddto;
    run;
    proc transpose data=admdata out=uddato prefix=uddto;
      by pnr; var uddto;
    run;",file=Prog,append=TRUE,sep="")
    cat("\n proc summary data = admdata nway;
    by pnr; var inddto; 
    output out = nadm n=max_indl; 
   run;",file=Prog,append=TRUE,sep="")
    # cat("\n proc print data = nadm; run;",file=Prog,append=TRUE,sep="")
    cat("\ndata datoer;
    merge indato uddato nadm (keep = pnr max_indl);
    by pnr;
    run;\n",file=Prog,append=TRUE,sep="")
    cat("%x_recepter(drugdb,
                  datoer,\n",
        name,",\n",
        atc,",\n",
        window,",\n",
        paste0(value,collapse=","),",\n",
        paste0(min,collapse=","),",\n",
        paste0(max,collapse=","),",\n",
        paste0(def,collapse=","),",\n",
        maxdepot,",\n",
        period[1],",\n",
        period[2],",\n","
        1,
        0,
        test,
        1)
    \n",file=Prog,append=TRUE,sep="")
    ## cat("\nproc datasets; run;\n",file=Prog,append=TRUE,sep="")
    cat("\ndata mm.",name,"_alt; set ",name,"_alt;run;\n",sep="",file=Prog,append=TRUE)
    ## cat("proc export 
    ## data=",name,"_alt
    ## dbms=csv\n",file=Prog,append=TRUE,sep="")
    ## cat(paste("OUTFILE=\"",Out,"\" replace;\n",sep=""),file=Prog,append=TRUE)
    ## cat("run;\n",file=Prog,append=TRUE,sep="")
    ## secure copy to server
    if (remote){
        system(paste0("scp -r ", tmpdir," ",user,"@",server,":",tmpdir))
        progstring <- paste0("ssh -Y ",
                             user,
                             "@",
                             server,
                             " ",
                             sas.program,
                             " -log ",
                             sub("~",remote.home,logfile),
                             " -print ",
                             sub("~",remote.home,lstfile),
                             " ",
                             sub("~",remote.home,Prog))
        print(progstring)
        status <- system(progstring)
        ## secure copy from server
        system(paste0("scp -r ",user,"@",server,":",tmpdir," ", wd))
    }else{
        if (.Platform$OS.type == "unix")
            fprog <- paste0(sas.program, " ", sas.switches, " ",
                            Prog)
        else
            fprog <- paste0("\"\"", sas.program, "\" ", sas.switches,
                            "\"", Prog, "\"\"")
        runnit <- try(do.call(sas.runner, list(fprog)), silent = FALSE)
        if (class(runnit)[1] == "try-error")
            warning(paste("Running sas on", fprog, "yielded the error shown above."))
    }
    if (file.exists(Out)){
        if (remote){
            out <- sas7bdat::read.sas7bdat(paste0(tmpdir,"/",name,"_alt.sas7bdat"))
            data.table::setDT(out)
            out[,startdag:=as.Date(startdag,origin=as.Date("1960-01-01"))]
            out[,slutdag:=as.Date(slutdag,origin=as.Date("1960-01-01"))]
        }else{
            out <- importSAS(filename=paste0(tmpdir,"/",name,"_alt.sas7bdat"),
                             date.vars=c("startdag","slutdag"),
                             verbose=verbose,show.sas.code=verbose,
                             sas.program=sas.program,sas.switches=sas.switches,sas.runner=sas.runner)
        }
        setnames(out,"startdag","firstday")
        setnames(out,"slutdag","lastday")
        setnames(out,"dosis","dose")
        out[,exposure.days:=lastday-firstday]
        setkey(out,pnr,firstday)
        return(out[])
    } else invisible(NULL)
}
