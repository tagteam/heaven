To install the package using the devtools package use the following commands 
(remember to have the package folder as working directory):

build_vignettes()   
build()    
install(build_vignettes = TRUE)     

build_vignettes = TRUE makes the vignettes work after installation (build_vignettes = FALSE is the default for some reason).  Now it should be possible to see the vignettes, e.g. by using browseVignettes('heaven') or vignette('user-heaven').
