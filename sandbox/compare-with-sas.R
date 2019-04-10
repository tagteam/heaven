### compare-with-sas.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Nov 29 2016 (15:36) 
## Version: 
## last-updated: Apr 10 2019 (08:28) 
##           By: Thomas Alexander Gerds
##     Update #: 11
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

admData <- data.frame(inddto = sapply(c("2004-01-20", "2004-01-22", "2006-06-20", "2006-06-23", "2010-01-21",
                                       "2010-01-14", "2010-01-26", "2010-07-05", "2010-10-21", "2011-07-14",
                                       "2011-12-01", "2011-12-14", "2011-12-25", "2011-12-29"), as.Date),
                      uddto  = sapply(c("2004-01-20", "2004-01-23", "2006-06-20", "2006-06-23", "2010-01-20",
                                       "2010-01-26", "2010-02-10", "2010-07-05", "2010-10-29", "2011-07-16",
                                       "2011-12-14", "2011-12-25", "2011-12-29", "2012-01-16"), as.Date))

admData$pnr <- rep(1, dim(admData)[1])
str(admData)
admData$inddto <- as.Date(admData$inddto, origin="1970-01-01")
admData$uddto <- as.Date(admData$uddto, origin="1970-01-01")

library(heaven)
PPI <- simPrescriptionData(n=1)
head(PPI)
PPI$eksd <- as.Date(PPI$eksd, format="%d/%m/%Y")
d <- dpp()
drugdb(d) <- PPI
d$drugdb
admdb(d) <- admData
str(d$admdb)

drug(d, "omeprazol") <- atc("A02BC02")
drug(d, "omeprazol") <- pack(c(10, 20, 40, 40),
                             min = c(10, 20, 40, 40),
                             max = c(20, 40, 60, 80),
                             def = c(10, 20, 40, 40))
period(d) <- sapply(c("1997-01-01", "2012-12-31"), as.Date)
d

maxdepot(d) <- 4000
d$maxdepot
plot(d)

#process(d, out_data = FALSE)
(out <- process(d, keep_data = TRUE))
plot(out)
plot(out, trace=TRUE)



source("~/research/SoftWare/UseSas/R/medicinMacro.R")


library(heaven)
library(data.table)
set.seed(05021992)
N=1
packs = list("R03AK11"=list(c(10,1)))
lmdb=simPrescriptionData(N,packages=packs,max.packages=1)
## very simple data 
lmdb <- lmdb[c(1,4,18)]
R03 = list(atc=c("R03AK11"),
           maxdepot=100,
           period=as.Date(c("1995-01-01", "2012-12-31")),
           prescriptionwindow=2,
           doses=list(value=c(10,33),min = c(.5,.5),max = c(2,2),def = c(1,1)))
set.seed(8)
packs

ppi=simPrescriptionData(8)
adm=simAdmissionData(8)
a=xrecepter(drug=ppi,
            adm=adm,
            npre=5,
            dose=c(200, 400, 500, 500),
            min=c(100, 200, 250, 250),max=c(300, 800, 1000, 100),def=c(20, 75,100,100),
            atc="A06AB06",
            name="omeprazol",
            sas.program="/usr/local/bin/sas",
            server="doob",
            user="grb615")
adm$inddto <- as.Date(adm$inddto)
adm$uddto <- as.Date(adm$uddto)
ppi$eksd <- as.Date(ppi$eksd,format="%d/%m/%Y")
library(heaven)
d=dpp()
drugdb(d) <- ppi
admdb(d) <- adm
plot(d)
drug(d, "omeprazol") <- atc("A02BC02")
drug(d, "omeprazol") <- pack(c(10, 20, 40), 
                              min = c(10, 20,40), 
                              max = c(20, 40,60), 
                             def = c(10, 20,40))
period(d) <- as.Date(c("1997-01-01", "2012-12-31"))



