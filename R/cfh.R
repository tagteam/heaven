### cfh.R Version 2--- 
#----------------------------------------------------------------------
## author: Jannik Pallisgaard 
## created: Jul  31 2016 (22:48) 
## Version: 
## last-updated: Aug  4 2016 (21:53) 
##           By: Thomas Alexander Gerds
##     Update #: 9
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
##' @param dt data.frame or data.table which provides variables: pnr, recno, inddto, uddto, pattype
##' @return data.frame where all intermediate admission records are removed 
##' @examples
##' data(samplepop)
##' cfh(samplepop)
##' @export 
##' @author Jannik Pallisgaard 

#code from hell----
cfh<-function(diag){
  diag$end<-as.numeric(diag$end)
  diag$start<-as.numeric(diag$start)
  diag<-diag[order(diag$pid,diag$start),]
  diag$prev.end <- with(diag, ave(end, pid, FUN=function(pid) head(c(0,pid),-1)))
  diag$prev.start <- with(diag, ave(start, pid, FUN=function(pid) head(c(0,pid),-1)))
  diag$first.start<-ifelse(diag$prev.end>=diag$start,diag$prev.start,diag$start)
  diag$last.end<-pmax(diag$end,diag$prev.end)
  diag$out<-as.character(paste(diag$pid, diag$first.start, sep=""))
  diag<-diag[order(diag$pid,-diag$last.end),]
  diag<-subset(diag,!duplicated(out))
  diag<-diag[order(diag$pid,diag$first.start),]
  diag$start<-diag$first.start
  diag$end<-diag$last.end
  diag[,c('prev.end','prev.start','first.start','last.end','out')]<-list(NULL,NULL,NULL,NULL,NULL)
  diag<-data.table(diag)
}

setkey(pop)

a<-Sys.time()
for(i in 1:100000000){
  old.pop.num<-nrow(pop)
  pop<-cfh(pop)
  new.pop.num<-nrow(pop)
  if (old.pop.num==new.pop.num){
    break
  }
}
b<-Sys.time()
round(b-a,1)
#----------------------------------------------------------------------
### cfh.R ends here
