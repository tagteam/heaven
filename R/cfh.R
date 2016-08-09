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
  diag$uddto<-as.numeric(diag$uddto)
  #date changed to numberic
  diag$inddto<-as.numeric(diag$inddto)
  #date changed to numberic
  diag<-diag[order(pnr,inddto),]
  #orders by first inddate for each id
  diag$prev.uddto <- with(diag, ave(uddto, pnr, FUN=function(pnr) head(c(0,pnr),-1))) 
  #copies uddto date from line above to line below by id and creates new variable prev.uddto
  diag$prev.inddto <- with(diag, ave(inddto, pnr, FUN=function(pnr) head(c(0,pnr),-1)))
  #copies inddto date from line above to line below by id and creates new variable prev.inddto
  diag$first.inddto<-ifelse(diag$prev.uddto>=diag$inddto,diag$prev.inddto,diag$inddto)
  #If prev.uddto date is larger og equal to inddto date then change first.inddto date to prev inddto date
  diag$last.uddto<-pmax(diag$uddto,diag$prev.uddto)
  # creates last uddto date by last date of uddto date or prev uddto date 
  diag$out<-as.character(paste(diag$pnr, diag$first.inddto, sep=""))
  #creates an identifier to removed if to rows are overlapping
  diag<-diag[order(pnr,-last.uddto),]
  # orders the last uddto date by id
  diag<-subset(diag,!duplicated(out))
  # removes the first identifier 
  diag<-diag[order(pnr,first.inddto),]
  # orders the first inddto date by id
  diag$inddto<-diag$first.inddto
  # first inddto is the new inddto date
  diag$uddto<-diag$last.uddto
  # last uddto date is uddto date
  diag[,c('prev.uddto','prev.inddto','first.inddto','last.uddto','out')]<-list(NULL,NULL,NULL,NULL,NULL)
  # removes needless variables
  diag<-data.table(diag)
  # changes diag to diag.table
}


library(data.table)
diag<-data.table(sample.diag)

a<-Sys.time()
for(i in 1:100000000){
  #runs the function between one and infinity
  old.pop.num<-nrow(diag)
  #counts the number of rows in the data.table before running function
  diag<-cfh(diag)
  #running function
  new.pop.num<-nrow(diag)
  #counts the number of rows in date.table after running the function
  if (old.pop.num==new.pop.num){
    break
  }
  #stops function when there is no difference between the number of rows before and after running the function
}
b<-Sys.time()
round(b-a,1)
#----------------------------------------------------------------------
### cfh.R ends here
