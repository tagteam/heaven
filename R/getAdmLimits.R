### getAdmLimits.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43) 
## Version: 
## last-updated: Aug  4 2016 (20:32) 
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

getAdmLimits <- function(dt){
    stopifnot(is.data.table(dt))
    setkey(dt,pnr,inddto)
    dt[,prev.uddto:= c(NA,uddto)]
    dt[,prev.inddto:= c(NA,inddto)]
    
    dt$first.inddto<-ifelse(dt$prev.uddto>=dt$inddto,dt$prev.inddto,dt$inddto)
    dt$last.uddto<-pmax(dt$uddto,dt$prev.uddto)
    dt$out<-as.character(paste(dt$pnr, dt$first.inddto, sep=""))
    dt<-dt[order(dt$pnr,-dt$last.uddto),]
    dt<-subset(dt,!duplicated(out))
    dt<-dt[order(dt$pnr,dt$first.inddto),]
    dt$inddto<-dt$first.inddto
    dt$uddto<-dt$last.uddto
    dt$uddto<-as.Date(dt$uddto,origin="1970-01-01")
    dt$inddto<-as.Date(dt$inddto,origin="1970-01-01")
    dt[,c('prev.uddto','prev.inddto','first.inddto','last.inddto','last.uddto','out')]<-list(NULL,NULL,NULL,NULL,NULL,NULL)
    as.data.frame(dt)
    
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here
