### getAdmLimits.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43) 
## Version: 
## last-updated: Aug  4 2016 (20:07) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

getAdmLimits <- function(dt){
    stopifnot(is.data.table(dt))
    setkey(dt,pnr,inddto)
    out <- 
    
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here
