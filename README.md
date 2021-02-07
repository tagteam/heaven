To install the package using the devtools package use the following commands 
(remember to have the package folder as working directory):

OPTIONAL Rcpp::compileAttributes()
OPTIONAL pkgbuild::compile_dll()
devtools::document()
build()
install()


To build/include the vignettes we need a workaround because of our non-standard orgmode vignettes:

tools::buildVignettes()
pkgbuild::build()

Now it should be possible to see the vignettes, e.g. by using
browseVignettes('heaven') or vignette('user-heaven').


Standard mail and set of commands for Statistics Denmark to provide a new version of heaven:

Kære Henrik og Carsten

Vi har en ny version af heaven (vedhæftet). I må meget gerne

1. slette den nuværende mappe:
"v:/data/alle/heaven"

og erstatte den med den vedhæftete fil - som skal udpakkes

2. for at installere den nye version skal følgende kommandoer køres

library(devtools)
library(tools)
library(pkgbuild)
setwd("v:/data/alle/heaven")
document()
pkgbuild::build()
#tools::buildVignettes(package='heaven')
install(build_vignettes=TRUE,force=TRUE)
library(heaven)

på forhånd mange tak for hjælpen !

Christian & Thomas