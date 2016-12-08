### compare-with-sas.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Nov 29 2016 (15:36) 
## Version: 
## last-updated: Nov 29 2016 (17:34) 
##           By: Thomas Alexander Gerds
##     Update #: 8
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
PPI <- read.table("~/research/SoftWare/heaven/data/samplePPIData.csv", header=TRUE, sep=";")
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



#----------------------------------------------------------------------
### admData.R ends here


