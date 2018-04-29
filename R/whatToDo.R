### whatToDo.R --- 
##----------------------------------------------------------------------
## Author: Paul Blanche
## Created: Feb 23 2018 (14:45) 
## Version: 
## Last-Updated: Apr 29 2018 (10:20) 
##           By: Thomas Alexander Gerds
##     Update #: 21
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:
#' decide between data splitting/combine and bootstrap
#' 
##' @title What to do: split and combine or bootstrap?
##' @param FUN  Name of function to call.
##' @param B Number of bootstrap replications.
##' @param K Number of splits.
##' @param mydata Data.
##' @param myargs Common arguments of the function for both Bootstrap and split method.
##' @param myargsBoot Specific arguments of the function for the Bootstrap method.
##' @param myargsPart Specific arguments of the function for the Bootstrap method.
##' @param timeunit Time units (to print the results).
##' @param myseed Random seed (for reproducibility).
##' @return nothing
##' @author Paul Blanche

whatToDo <- function(FUN,
                     B=10,
                     K=10,
                     mydata,
                     myargs,        
                     myargsBoot,    
                     myargsPart,
                     timeunit="mins",
                     myseed=29021986){
    ## browser()
    set.seed(myseed)
    myn <- nrow(mydata)
    ## dim(dpart)
    ## dim(dboot)
    ## length(unique(dboot$X6))    
    ## {{{ 1 bootstrap
    indicesBoot <- sample(x=1:myn,size=myn,replace=TRUE)
    dboot <- mydata[indicesBoot,]
    tboot0 <- Sys.time()
    res1boot <- do.call(what=FUN,c(data=list(dboot),myargs,myargsBoot))
    tboot1 <- Sys.time()
    compTimeBoot <- as.numeric(difftime(tboot1,tboot0,units=timeunit))
    ## }}}
    ## {{{ Estim on 1 part
    indicesPart <- sample(x=1:myn,size=ceiling(myn/K),replace=FALSE)
    dpart <- mydata[indicesPart,]
    tpart0 <- Sys.time()
    res1part <- do.call(what=FUN,c(data=list(dpart),myargs,myargsPart))    
    tpart1 <- Sys.time()
    compTimePart <- as.numeric(difftime(tpart1,tpart0,units=timeunit))
    ## }}}
    ## {{{ What to do?
    ToDO <- ifelse(compTimePart*K<compTimeBoot*B,"Split the data!","Bootstrap!")
    ## }}}
    ## {{{ OLD Print (now in the print function)
    ## cat("\nSample size is",myn,".\n")
    ## cat("\nComputation on one Boostrap sample took",compTimeBoot,timeunit,".\n")
    ## cat("With",B," replications it should take about",compTimeBoot*B,timeunit,".\n")
    ## cat("\nComputation on one subsample (n=",ceiling(myn/K),") took",compTimePart,timeunit,".\n")
    ## cat("In total, on all ",K,
    ## "subsamples it should take about",compTimePart*K,timeunit,".\n")
    ## cat("\nTo do:",ToDO,"\n")    
    ## }}}
    out <- list(timeBoot=compTimeBoot,
                timeSplit=compTimePart,
                resBoot=res1boot,
                resSplit=res1part,
                indices=list(Boot=indicesBoot,
                             Part=indicesPart),
                myn=myn,
                K=K,
                B=B,
                timeunit=timeunit,
                todo=ToDO
                )
    class(out) <- "BootorSplit"
    out
}


##----------------------------------------------------------------------
### whatToDo.R ends here
