To install the package using the devtools package use the following commands 
(remember to have the package folder as working directory):

OPTIONAL Rcpp::compileAttributes()
OPTIONAL pkgbuild::compile_dll()
devtools::document()
build()
install()


To build/include the vignettes we need a workaround because of our
non-standard orgmode vignettes:

tools::buildVignettes()
pkgbuild::build()

Now it should be possible to see the vignettes, e.g. by using
browseVignettes('heaven') or vignette('user-heaven').
