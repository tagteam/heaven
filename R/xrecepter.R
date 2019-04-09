##' wrapper for medicin macro
##'
##' wrapper for medicin macro
##' @title wrapper for medicin macro
##' @param drug drug data  
##' @param adm admission data
##' @param atc atc kode specifying drug of interest
##' @param name name of drug
##' @param sas.program path to sas program
##' @param sas.switches On linux this defaults to {""} on any other system to \code{"-batch -nosplash -noenhancededitor -sysin"}
##' @param sas.runner How sas is invoked. On linux this defaults to \code{"system"} on any other system to \code{"shell"}.
##' @param macro path to sas macro
##' @param proFile path to pro
##' @param drugFile path to drug
##' @param admFile path to adm
##' @param outFile path to out
##' @param logfile path to log
##' @examples
##' library(heaven)
##' \dontrun{
##' ppi=read.table("~/research/SoftWare/heaven/data/PPI.csv",sep=";",header=TRUE)
##' adm=read.table("~/research/SoftWare/heaven/data/admData.csv",sep=";",header=TRUE)
##' u=medicinMacro(drug=ppi,adm=adm,atc="A02BC02",name="omeprazol",npre=5,dose=c(10, 20, 40, 40),min=c(10, 20, 40, 40),max=c(20, 40, 60, 80),def=c(10, 20, 40, 40),maxdepot=4000,period=c("'01jan1997'd","'31dec2012'd"),sas="/usr/local/bin/sas",server="doob",user="grb615")
##' v=medicinMacro(drug=ppi,adm=adm,atc="A02BC02",name="omeprazol",npre=5,dose=c(10, 20, 40, 40),min=c(10, 20, 40, 40),max=c(20, 40, 60, 80),def=c(20, 40, 60, 10),maxdepot=4000,period=c("'01jan1997'd","'31dec2012'd"),sas="/usr/local/bin/sas",server="doob",user="grb615")
##' set.seed(05021992)
##' N=100
##' da=heaven:::simPrescriptionData(N)
##' a=heaven:::simAdmissionData(N)
##' system.time(w <- medicinMacro(drug=da,adm=a,atc="A07",
##' name="drug1",npre=5,dose=c(200, 400, 500, 500),
##' min=c(100, 200, 250, 250),max=c(300, 800, 1000, 1000),def=c(200, 400,500,500),maxdepot=4000,period=c("'01jan1997'd","'31dec2012'd"),
##' sas.program="/usr/local/bin/sas",server="doob",user="grb615"))
##'
##' library(heaven)
##' d=dpp()
##' drugdb(d) <- (da)
##' admdb(d) <- (a)
##'
##' drug(d, "drug1") <- atc("A07")
##' drug(d, "drug1") <- pack(c(200, 400, 500),
##'                          min = c(100, 200, 250),
##'                          max = c(300, 800, 1000),
##'                          def = c(200, 400, 500))
##' maxdepot(d) <- 4000
##' pwindow(d) <- 2
##' } 
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
xrecepter <- function(drug,
                      adm,
                      atc,
                      npre,
                      dose,
                      min,
                      max,
                      def,
                      maxdepot,
                      period,
                      name,
                      user,
                      server="doob",
                      wd,
                      sas.program="/usr/local/bin/sas ",
                      sas.switches,
                      sas.runner,
                      macro="~/research/SoftWare/heaven/sas/medicin_macro_version_31.sas"){

    .SD = NULL
    olddir <- getwd()
    if (length(wd) == 0){
        wd <- getwd()
    }
    # cleaning up old temporary directories
    olddirs <- list.files(wd,pattern="heaven_tempSASfiles[a-z0-9]+")
    for (old in olddirs){
        message("Cleaning up temporary directories from previous calls.")
        unlink(old,recursive=TRUE,force=TRUE)
    }
    tmpname <- basename(tempfile(pattern="heaven_tempSASfiles"))
    tmpdir = paste0(wd,"/",tmpname)
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
    proFile="med-macro-call.sas"
    drugFile="med-macro-drug.csv"
    admFile="med-macro-adm.csv"
    outFile="med-macro-out.csv"
    logfile="med-macro.log"
    lstfile="med-macro.lst"
    Prog <- path.expand(proFile)
    Out <- path.expand(outFile)
    if(file.exists(Out)) file.remove(Out)
    data.table::fwrite(drug,sep=";",file=drugFile)
    data.table::fwrite(adm,sep=";",file=admFile)
    cat("libname mm '/tmp/';\n",file=Prog,append=FALSE,sep="")
    cat("%include \'",macro,"\';\n\n",file=Prog,append=TRUE,sep="")
    cat("proc import out=drug
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
    cat("%x_recepter(drug,
                  datoer,\n",
        name,",\n",
        atc,",\n",
        npre,",\n",
        paste0(dose,collapse=","),",\n",
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
    run;\n",file=Prog,append=TRUE,sep="")
    cat("\nproc datasets; run;\n",file=Prog,append=TRUE,sep="")
    cat("proc export 
    data=",name,"_alt
    dbms=csv\n",file=Prog,append=TRUE,sep="")
    cat(paste("OUTFILE=\"",Out,"\" replace;\n",sep=""),file=Prog,append=TRUE)
    cat("run;\n",file=Prog,append=TRUE,sep="")
    ## remove old out file on server
    system(paste0("ssh ",user,"@",server," 'rm ",Out,"'"),intern=TRUE)
    ## secure copy to server
    system(paste0("scp ", drugFile," ",user,"@",server,":",drugFile))
    system(paste0("scp ", admFile," ",user,"@",server,":",admFile))
    progstring <- paste0("ssh -Y ",user,"@",server," ",sas.program," -log ",logfile," -print ",lstfile," ",Prog)
    print(progstring)
    system(paste0("scp ", Prog," ",user,"@",server,":",Prog))
    status <- system(progstring)
    isLOG <- system(paste0("scp ",user,"@",server,":",logfile," ",logfile),intern=TRUE)
    ## isLST <- system(paste0("scp ",user,"@",server,":",lstfile," ",lstfile),intern=TRUE)
    isOUT <- system(paste0("scp ",user,"@",server,":",Out," ",Out),intern=TRUE)
    if (length(attr(isOUT,"status")))
        system(paste0("cat ",logfile))
    else 
        message(paste0("    Logfile: ",logfile,"\nOutput file: ",lstfile))
    out <- read.csv(Out)
    out
}
