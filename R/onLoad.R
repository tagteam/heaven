.onLoad <- function(libname, pkgname){
    if (interactive()) {
        v = utils::packageVersion("heaven")
        packageStartupMessage(paste0("\nLOL: This is heaven! Version ", v))
        packageStartupMessage(paste0("A good place to get started is vignette('user-heaven')."))
    }
}
