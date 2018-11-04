### print-whattoDo.r --- 
##----------------------------------------------------------------------
## Author: Paul Blanche
## Created: Feb 23 2018 (14:51) 
## Version: 
## Last-Updated: Nov  4 2018 (08:34) 
##           By: Thomas Alexander Gerds
##     Update #: 9
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

print.whattodo <- function(x,...){
    cat("\nSample size is",x$myn,".\n")
    cat("\nComputation on one Boostrap sample took",x$timeBoot,x$timeunit,".\n")
    cat("With",x$B," replications it should take about",x$timeBoot*x$B,
        x$timeunit,".\n")
    cat("\nComputation on one subsample (n=",ceiling(x$myn/x$K),
        ") took",x$timeSplit,x$timeunit,".\n")
    cat("In total, on all ",x$K,
        "subsamples it should take about",x$timeSplit*x$K,x$timeunit,".\n")
    cat("\nTo do:",x$todo,"\n")    
}

##----------------------------------------------------------------------
### print-whattoDo.r ends here
