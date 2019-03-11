.onLoad <- function(libname, pkgname){
    ## tools::vignetteEngine("knitr", weave = vweave, tangle = vtangle,
                          ## pattern = "[.]Rmd$", package = "knitr")
    if (interactive()) {
        v = utils::packageVersion("heaven")
        packageStartupMessage(paste0("\nLOL: This is heaven! Version ", v))
        packageStartupMessage(paste0("A good place to get started is vignette('user-heaven')."))
    }
}
