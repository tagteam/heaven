library(data.table)
diag<-data.table(sample.diag)
diag<-getAdmLimits(diag)
diag<-diag[order(pnr,inddto),]
View(diag)


##Problems not solved ##
# does not remove duplicates
#regnum 35 is a hospital stay for 20000 days
#regnum 37 is a hospital stay with a negative duration

library(data.table)
diag<-data.table(sample.diag)
diag<-getAdmLimits2(diag)
diag<-diag[order(pnr,inddto),]
View(diag)

##Problems not solved ##
#does not remove duplicates
#regnum 35 is a hospital stay for 20000 days
#regnum 37 is a hospital stay with a negative duration