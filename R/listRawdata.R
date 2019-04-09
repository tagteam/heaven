### listRawdata.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr  9 2019 (15:23) 
## Version: 
## Last-Updated: Apr  9 2019 (15:48) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' List the contents of a directory which contains sas data files
##'
##' List the contents of a directory which contains sas data files
##' @title List sas data files
##' @param dir directory which contains the sas files 
##' @param pattern passed to \code{list.files}
##' @param full.names passed to \code{list.files}
##' @param ... passed to list.files
##' @return list with file names
##' @seealso \code{scanRawdata}, \code{addUsualRegisters} 
##' @examples
##' \dontrun{listRawdata("x:/Data/Rawdata_Hurtig/706818/")}
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
listRawdata <- function(dir,
                        pattern="\\.sas7bdat$",
                        full.names=FALSE,
                        ...){
    if (missing(dir)){
        dir <- guessRawdataDirectory()
    }
    if (file.exists(dir))
        list.files(dir,pattern=pattern,full.names=full.names,...)
    else{
        stop("Argument dir is missing and my best guess does not exist:\n",dir,".\nPlease specify directory or set working directory to workdata.")
    }
}


######################################################################
### listRawdata.R ends here
