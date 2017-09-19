############################
### Testing getAdmLimits ###
############################

library(data.table)
diag<-data.table(sample.diag)
diag<-getAdmLimits(diag)
diag<-diag[order(pnr,inddto),] #does nothing since it is sorted in the funtion
View(diag)



##Problems not solved ##
# does not remove duplicates
#regnum 35 is a hospital stay for 20000 days
#regnum 37 is a hospital stay with a negative duration



#############################
### Testing getAdmLimits2 ###
#############################

#note: Does not collapse, but adds to all observations.
library(data.table)
#run data/samplediaggenerator.r
diag<-as.data.table(sample.diag)
#run R/getAdmLimits2.R
diag<-getAdmLimits2(diag)
View(diag)

str(diag)

##Problems not solved ##
#regnum 35 is a hospital stay for 20000 days 
    #is this a problem? it is like this in the sample. Should it detect dates that are in the future?