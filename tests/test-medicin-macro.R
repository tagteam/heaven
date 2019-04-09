### test-medicin-macro.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Mar 11 2019 (12:57) 
## Version: 
## Last-Updated: Apr  9 2019 (16:36) 
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
if (FALSE){
    library(data.table)
    testmed <- data.table(pnr=17,apk=1L,atc="C03",eksd=as.Date(c("2010-03-18","2010-05-25","2010-08-10","2010-10-05","2010-11-24","2011-01-13","2011-03-07","2011-05-03","2011-06-06","2011-08-02","2011-09-27","2012-02-27","2012-04-23","2012-08-06","2012-11-05","2013-01-15","2013-05-14","2013-08-05","2013-10-28","2014-02-04")),
                          packsize=100,strnum=40)

    testlpr <- lpr[pnr<7]
    drug1=list(atc=c("C03"),
               maxdepot=4000,
               period=as.Date(c("2000-01-01", "2016-12-31")),
               prescriptionwindow=2,
               doses=list(value=c(40, 400, 500,750),
                          min = c(20, 200, 250,750),
                          max = c(160, 800, 1000,750),
                          def = c(40, 400, 500,750)))

    xx <- sapply(1:100,function(b){
        a <- medicinMacro(drugs=list("drug1"=drug1),drugdb=testmed,admdb=testlpr)
        b <- medicinMacro(drugs=list("drug1"=drug1),drugdb=testmed,admdb=testlpr)
        print(all.equal(a,b))
    })
    sum(xx)
}

######################################################################
### test-medicin-macro.R ends here
