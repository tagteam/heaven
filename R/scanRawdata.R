### scanRawdata.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr  9 2019 (15:25) 
## Version: 
## Last-Updated: Apr  9 2019 (15:48) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#' Scan the contents of a directory which contains sas data files
#'
#' Scan the contents of a directory which contains sas data files
#' @title Scan sas data files
#' @param dir directory which contains the sas files 
#' @param which vector of expressions to match the file names
#' @param verbose Logical. Print file contents and pass verbosity to importSAS.
#' @return A named list of data.tables describing the variables in each
#' matching SAS file. Each table is returned by \code{importSAS(content = TRUE)};
#' list names are the SAS file names. An empty list is returned if no files match.
#' @seealso \code{listRawdata}
#' @examples
#' \dontrun{
#' # Requires SAS and access to the registry files at the supplied path.
#' scanRawdata("x:/Data/Rawdata_Hurtig/706818/",which=c("pop","lmdb"))
#' }
#' @export 
#' @author Thomas A. Gerds <tag@@biostat.ku.dk>
scanRawdata <- function(dir,which=c("pop","doede","diag","lmdb","opr"),verbose=FALSE){
    sasfiles <- listRawdata(dir=dir,full.names=TRUE)
    out <- vector(length(sasfiles),mode="list")
    names(out) <- sapply(sasfiles,function(ff){
        tmp <- strsplit(ff,"/")[[1]]
        tmp[[length(tmp)]]
    })
    for (i in seq_along(sasfiles)){
        sf <- sasfiles[[i]]
        name.sf <- names(out)[[i]]
        if (length(which)==0 || any(unlist(sapply(which,grep,name.sf)))){
            csf <- importSAS(sf,content=TRUE,verbose=verbose)
            if (verbose) print(csf)
            out[[i]] <- csf
        }
    }
    out[sapply(out,length)>0]
}


######################################################################
### scanRawdata.R ends here
