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

## Anders: efter du har bygget pakken en gang via
if (0L){
setwd("~/research/SoftWare/heaven/")
devtools::document()
devtools::build()
devtools::install(quick=TRUE)
}

# kan du bar koere de foelgende to linier (kraever dog at man re-starter R)
# men du behoever ikke vente paa install() hver gang
setwd("~/research/SoftWare/heaven/")
inner1 <- Rcpp::sourceCpp("src/innerMedicinMacro.cpp")
source("R/medicinMacro.R")

## Tasks:
##-----------------------------------------

## 2. resultatet skal give mening baade med og uden collapse
medicinMacro(drugs=list("R03"=R03),drugdb=lmdb[,strnum:=33][,apk:=2][1,],admdb=NULL,collapse=1L)

xrecepter(drugs=list("R03"=R03),drugdb=lmdb[,strnum:=33][,apk:=2][1,],admdb=NULL,collapse=1L)


y <- medicinMacro(drugs=list("R03"=R03),drugdb=lmdb[c(1,1,2,3)],admdb=NULL,collapse=0L)
y

## 3. resultatet skal igen ligne SAS resultat.

# der er en ny fil: xrecepter.R som skal kunne koere SAS macro i baggrund og hente resultaterne
# det skal den kunne paa DST og ogsaa herhjemme (borel, gauss, etc). dvs., du skal kopere noget fra
# importSAS ...

