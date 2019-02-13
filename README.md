To install the package using the devtools package use the following commands 
(remember to have the package folder as working directory):

build_vignettes()                                               ## Creates html files and the folder 'doc'
build()                                                         ## Creates .tar.gz file
install(build_vignettes = TRUE)                                 ## build vignettes = FALSE is the default for some reason

Now it should be possible to see the vignettes, e.g. by using browseVignettes('heaven') or vignette('user-heaven').
