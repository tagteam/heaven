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
##' @return A character vector of matching SAS file names, including their
##' paths when \code{full.names = TRUE}.
##' @seealso \code{scanRawdata}, \code{addUsualRegisters} 
##' @examples
##' rawdir <- tempfile("heaven-rawdata-")
##' dir.create(rawdir)
##' file.create(file.path(rawdir, "pop.sas7bdat"))
##' listRawdata(rawdir)
##' unlink(rawdir, recursive = TRUE)
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
