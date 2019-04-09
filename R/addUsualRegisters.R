### addUsualRegisters.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr  9 2019 (15:17) 
## Version: 
## Last-Updated: Apr  9 2019 (15:51) 
##           By: Thomas Alexander Gerds
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Add usual registers to dpp object
##'
##' Add usual registers to dpp object
##' @title Add usual registers to dpp object
##' @param x dpp object 
##' @param pop If character this is the filename (full path) of the pop registry. If \code{TRUE} we search for pop.sas7bdat in result of \code{listRawdata(full.names=TRUE)}.
##' @param dod If character this is the filename (full path) of the dod registry. If \code{TRUE} we search for dod.sas7bdat in result of \code{listRawdata(full.names=TRUE)}.
##' @param lmdb If character this is the filename (full path) of the lmdb registry. If \code{TRUE} we search for lmdb.sas7bdat in result of \code{listRawdata(full.names=TRUE)}.
##' @param lpr If character this is the filename (full path) of the lpr registry. If \code{TRUE} we search for lpr.sas7bdat in result of \code{listRawdata(full.names=TRUE)}.
##' @param opr If character this is the filename (full path) of the opr registry. If \code{TRUE} we search for opr.sas7bdat in result of \code{listRawdata(full.names=TRUE)}.
##' @param ... not yet used
##' @return the updated object
##' @seealso dpp
##' @examples
##' # data preprocessing object
##' \dontrun{
##' x <- dpp()
##' # locate registries
##' listRawdata(full.names=TRUE)
##' # add selected registries
##' x <- addUsualRegisters(x)
##' }
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
addUsualRegisters <- function(x,
                              pop=TRUE,
                              dod=TRUE,
                              lmdb=TRUE,
                              lpr=TRUE,
                              opr=TRUE, ...){
    files <- listRawdata(full.names=TRUE)
    if(length(pop)>0  || (is.logical(pop)&& pop==FALSE)){
        if (is.character(pop))
            pop.file <- pop
        else
            pop.file <- grep("pop.sas7bdat",files,value=TRUE)
        if (length(pop.file)>0 && file.exists(pop.file)){
            message("checking in registry 'pop' at: ",pop.file)
            addData(x,name="pop",variables=c("sex","date"="fdato")) <- pop.file
        }else{
            warning("Cannot find file for pop: ",pop.file," does not exists")
        }
    }
    if(length(dod)>0  || (is.logical(dod)&& dod==FALSE)){
        if (is.character(dod))
            dod.file <- dod
        else
            dod.file <- grep("doede.sas7bdat",files,value=TRUE)
        if (length(dod.file)> 0 && file.exists(dod.file)){
            message("checking in registry 'dod' at: ",dod.file)
            addData(x,name="dod",variables=c("date"="doddato")) <- dod.file
        }else{
            warning("Cannot find file for dod: ",dod.file," does not exists")
        }
    }
    if(length(lmdb)>0  || (is.logical(lmdb)&& lmdb==FALSE)){
        if (is.character(lmdb))
            lmdb.file <- lmdb
        else
            lmdb.file <- grep("lmdb.sas7bdat",files,value=TRUE)
        if (length(lmdb.file)>0 && file.exists(lmdb.file)){
                        message("checking in registry 'lmdb' at: ",lmdb.file)
            addData(x,name="lmdb",variables=c("atc","date"="eksd")) <- lmdb.file
        }else{
            warning("Cannot find file for lmdb: ",lmdb.file," does not exists")
        }
    }
    if(length(lpr)>0 || (is.logical(lpr)&& lpr==FALSE)){
        if (is.character(lpr))
            lpr.file <- lpr
        else
            lpr.file <- grep("diag_indl.sas7bdat",files,value=TRUE)
        if (length(lpr.file)>0 && file.exists(lpr.file)){
            message("checking in registry 'lpr' at: ",lpr.file)
            addData(x,name="lpr",variables=c("diag","diagtype","date"="inddto","date"="uddto","pattype")) <- lpr.file
        }else{
            warning("Cannot find file for lpr: ",lpr.file," does not exists")
        }
    }
    if(length(opr)>0 || (is.logical(opr)&& opr==FALSE)){
        if (is.character(opr))
            opr.file <- opr
        else
            opr.file <- grep("opr.sas7bdat",files,value=TRUE)
        if (length(opr.file)>0 &&  file.exists(opr.file)){
            message("checking in registry 'opr' at: ",opr.file)
            addData(x,name="opr",variables=c("opr","tilopr","date"="odto","date"="inddto","date"="uddto","pattype")) <- opr.file
        }else{
            warning("Cannot find file for opr: ",opr.file," does not exists")
        }
    }
    x
}


######################################################################
### addUsualRegisters.R ends here
