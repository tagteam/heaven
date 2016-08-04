### cfh.R --- 
#----------------------------------------------------------------------
## author: Jannik Pallisgaard 
## created: Jul  31 2016 (22:48) 
## Version: 
## last-updated: Aug  4 2016 (20:01) 
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#code from hell----
##' R-version of the sas code from hell.
##'
##' Ignores pattype 
##' @title Jannik's first R-version of the sas code from hell 
##' @param dt data frame or data table which provides variables: pnr, recno, inddto, uddto, pattype
##' @return 
##' @seealso 
##' @examples
##' data(samplepop)
##' cfh(samplepop)
##' @export 
##' @author Jannik Pallisgaard 
cfh <- function(dt){
    dt$uddto<-as.numeric(dt$uddto)
    dt$inddto<-as.numeric(dt$inddto)
    dt$prev.uddto <- with(dt, ave(uddto, pnr, FUN=function(pnr) head(c(0,pnr),-1)))
    dt$prev.inddto <- with(dt, ave(inddto, pnr, FUN=function(pnr) head(c(0,pnr),-1)))
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
### cfh.R ends here
