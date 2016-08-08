### cfh.R --- 
#----------------------------------------------------------------------
## author: Jannik Pallisgaard 
## created: Jul  31 2016 (22:48) 
## Version: 
## last-updated: Aug  8 2016 (10:38) 
##           By: Jannik Pallisgaard
##     Update #: 10
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
  #date changed to numberic
  diag$start<-as.numeric(diag$start)
  #date changed to numberic
  diag<-diag[order(diag$pid,diag$start),]
  #orders by first inddate for each id
  diag$prev.end <- with(diag, ave(end, pid, FUN=function(pid) head(c(0,pid),-1))) 
  #copies end date from line above to line below by id and creates new variable prev.end
  diag$prev.start <- with(diag, ave(start, pid, FUN=function(pid) head(c(0,pid),-1)))
  #copies start date from line above to line below by id and creates new variable prev.start
  diag$first.start<-ifelse(diag$prev.end>=diag$start,diag$prev.start,diag$start)
  #If prev.end date is larger og equal to start date then change first.start date to prev start date
  diag$last.end<-pmax(diag$end,diag$prev.end)
  # creates last end date by last date of end date or prev end date 
  diag$out<-as.character(paste(diag$pid, diag$first.start, sep=""))
  #creates an identifier to removed if to rows are overlapping
  diag<-diag[order(diag$pid,-diag$last.end),]
  # orders the last end date by id
  diag<-subset(diag,!duplicated(out))
  # removes the first identifier 
  diag<-diag[order(diag$pid,diag$first.start),]
  # orders the first start date by id
  diag$start<-diag$first.start
  # first start is the new start date
  diag$end<-diag$last.end
  # last end date is end date
  diag[,c('prev.end','prev.start','first.start','last.end','out')]<-list(NULL,NULL,NULL,NULL,NULL)
  # removes needless variables
  diag<-data.table(diag)
  # changes diag to diag.table
}

setkey(pop)

a<-Sys.time()
for(i in 1:100000000){
  #runs the function between one and infinity
  old.pop.num<-nrow(pop)
  #counts the number of rows in the data.table before running function
  pop<-cfh(pop)
  #running function
  new.pop.num<-nrow(pop)
  #counts the number of rows in date.table after running the function
  if (old.pop.num==new.pop.num){
    break
  }
  #stops function when there is nu difference between the rows before and after running the function
}
b<-Sys.time()
round(b-a,1)
#----------------------------------------------------------------------
### cfh.R ends here
