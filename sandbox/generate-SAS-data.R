### generate-SAS-data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  1 2018 (18:50) 
## Version: 
## Last-Updated: Nov  1 2018 (20:10) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(heaven)
library(data.table)
set.seed(8)
lmdb <- simPrescriptionData(9837)
set.seed(9)
lpr <- simAdmissionData(1338)
fwrite(lpr,file="~/research/SoftWare/heaven/sandbox/lpr.csv")
fwrite(lmdb,file="~/research/SoftWare/heaven/sandbox/lmdb.csv")

system("sas ~/research/SoftWare/heaven/sandbox/generate-SAS-data.sas")

######################################################################
### generate-SAS-data.R ends here
