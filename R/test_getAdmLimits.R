library(data.table)
diag<-data.table(sample.diag)
diag<-getAdmLimits(diag)
diag<-diag[order(pnr,inddto),] #does nothing since it is sorted in the funtion
View(diag)

library(data.table)
#run data/samplediaggenerator.r
diag<-data.table(sample.diag2)
#run R/getAdmLimits2.r
diag<-getAdmLimits2(diag)
View(diag)

##Problems solved ##
#regbnum 34 are duplicates 
#regnum 29 is a hospital stay within other hospital stays

##Problem not solved ##
#regnum 35 is a hospital stay for 20000 days
#regnum 37 is a hospital stay with a negative duration

#All above should be solved for getAdmLimits2