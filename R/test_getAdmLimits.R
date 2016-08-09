library(data.table)
diag<-data.table(sample.diag)
diag<-getAdmLimits(diag)
diag<-diag[order(pnr,inddto),]
View(diag)

##Problems solved ##
#regbnum 34 are duplicates 
#regnum 29 is a hospital stay within other hospital stays

##Problem not solved ##
#regnum 35 is a hospital stay for 20000 days
#regnum 37 is a hospital stay with a negative duration

