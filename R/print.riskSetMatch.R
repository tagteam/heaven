### print.riskSetMatch.R --- 
                                        #----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov 17 2019 (17:34) 
## Version: 
## Last-Updated: Nov 17 2019 (17:40) 
##           By: Thomas Alexander Gerds
##     Update #: 2
                                        #----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
                                        #----------------------------------------------------------------------
## 
### Code:
print.riskSetMatch <- function(x,...){
    .SD=.N=N=caseid=NULL
    id=attr(x,"id")
    event=attr(x,"event")
    cat("\n","------------------------------------------------------------------","\n","Matching success")
    ## Matching success:
    if (!("n.controls" %in% names(x)))
        n.controls <- as.vector(x[,(.N-1),by="case.id"])
    else
        n.controls <- x[["n.controls"]]
    cat("\n", "Line 1, number of controls found - Line 2, number of occurrences:\n")
    print(table(n.controls))
    cat("\n","------------------------------------------------------------------")
    ##Reuse of controls
    n.reuse<-as.vector(x[x[[event]]==0,.N,by=id])[["N"]]
    cat("\n","Reuse/use of controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
    print(table(n.reuse))
    cat("\n","------------------------------------------------------------------\n")
}


## summary.riskSetMatch <- function(object,...){
  # din matchreport kode her
## }


######################################################################
### print.riskSetMatch.R ends here
