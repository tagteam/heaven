library(heaven)
library(data.table)
set.seed(05021992)
N=180
packs = list("R03AK11"=list(c(10,5)))
source("~/research/SoftWare/heaven/R/simPrescriptionData.R")
lmdb=simPrescriptionData(N,packages=packs,max.packages=1)
## table(lmdb$strnum)
lpr=simAdmissionData(N)
## very simple data 
## lmdb <- lmdb[c(1,4,18)]
R03 = list(atc=c("R03AK11"),
           maxdepot=10000,
           period=as.Date(c("1995-01-01", "2012-12-31")),
           prescriptionwindow=5,
           doses=list(value=c(5,10),
                      min = c(1,1),
                      max = c(2,2),
                      def = c(1,1)))

## efter man har bygget pakken en gang via
if (0L){
setwd("~/research/SoftWare/heaven/")
devtools::document()
devtools::build()
devtools::install(quick=TRUE)
}

# kan man bar koere de foelgende to linier (kraever dog at man re-starter R)
# men man behoever ikke vente paa install() hver gang

setwd("~/research/SoftWare/heaven/")
inner1 <- Rcpp::sourceCpp("src/innerMedicinMacro.cpp")
source("R/medicinMacro.R")
## lmdb <- lmdb[,pnr:=c(1,3,1)][,strnum:=5]

lmdb1 <- lmdb[pnr==1][1:6]
## lmdb1 <- lmdb[pnr==1][1:5]
## lmdb1[2,eksd:=as.Date("1995-01-01")+15]
a <- medicinMacro(drugs=list("R03"=R03),drugdb=lmdb1,admdb=lpr)
B <- a$R03[dose>0]
b <- xrecepter(drugdb=lmdb1,adm=lpr,window=5,remote=TRUE,remote.home="/home/ifsv/grb615",wd="~/tmp/",save.tmp=FALSE,value=rep(R03$doses$value,2),min=rep(R03$doses$min,2),max=rep(R03$doses$max,2),def=rep(R03$doses$def,2),maxdepot=100,period=c(12784,20089),atc="R03AK11",name="blaupill",sas.program="/usr/local/bin/sas",server="doob",user="grb615")
a$R03[dose>0]
b

## Tasks:
##-----------------------------------------

## 2. resultatet skal give mening baade med og uden collapse

medicinMacro(drugs=list("R03"=R03),drugdb=lmdb1,admdb=lpr,collapse=0L)

## source("~/research/SoftWare/heaven/R/xrecepter.R")
xrecepter(drugdb=lmdb,
          adm=lpr,
          window=5,
          remote=FALSE,
          wd="~/tmp",
          save.tmp=0L,
          value=rep(R03$doses$value,2),
          min=rep(R03$doses$min,2),
          max=rep(R03$doses$max,2),
          def=rep(R03$doses$def,2),
          maxdepot=100,
          period=c(12784,20089),
          atc="R03AK11",
          name="blaupill",
          sas.program="/usr/local/bin/sas",
          verbose=FALSE)

## remote
xrecepter(drugdb=lmdb,adm=lpr,npre=5,remote=TRUE,remote.home="/home/ifsv/grb615",wd="~/tmp/",save.tmp=TRUE,dose=rep(R03$doses$value,2),min=rep(R03$doses$min,2),max=rep(R03$doses$max,2),def=rep(R03$doses$def,2),maxdepot=100,period=c(12784,20089),atc="R03AK11",name="blaupill",sas.program="/usr/local/bin/sas",server="doob",user="grb615")


y <- medicinMacro(drugs=list("R03"=R03),drugdb=lmdb[c(1,1,2,3)],admdb=NULL,collapse=0L)
y

## 3. resultatet skal igen ligne SAS resultat.

# der er en ny fil: xrecepter.R som skal kunne koere SAS macro i baggrund og hente resultaterne
# det skal den kunne paa DST og ogsaa herhjemme (borel, gauss, etc). dvs., du skal kopere noget fra
# importSAS ...

