### createProject.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr 11 2019 (09:51) 
## Version: 
## Last-Updated: Apr 11 2019 (10:43) 
##           By: Thomas Alexander Gerds
##     Update #: 17
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' create new empty folder for project organisation 
##'
##' ordnung muss sein
##' @title ordnung muss sein
##' @param directory Full path of the new project. E.g., "v:/data/workdata/706818/BlaBlaBla/study"
##' @examples
##' createProject("v:/data/workdata/706818/BlaBlaBla/study")
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
createProject <- function(directory){
    if (missing(directory)) directory <- file.choose()
    if (dir.exists(directory)) stop("Directory ",directory," exists already.\nYou may want to choose another name and afterwards merge with the old project!?")
    else {
        dd <- try(xx <- dir.create(directory))
        if (xx==FALSE || class(dd)[1]=="try-error")
            stop("Cannot create directory ",directory,"\n\nMaybe the mother directory does not exist yet?\nOr, maybe you do not have write permissions?\n\n")
        else{
            readme <- paste0(directory,"/","README.txt")
            file.create(readme)
            cat("Created: ",as.character(Sys.Date()),
                "\nAuthor(s): ",as.character(Sys.info()[["login"]]),
                "\n-----------------------------------------------------------",
                "\nFile structure:\n",
                "  - To create a new R script file copy the file template-R-code.R\n",
                "  - To create a new SAS script file copy the file template-SAS-code.R",
                "\n-----------------------------------------------------------",
                "\nDo not POLUTE the environment:",
                "\n - put temporary objects into temp (i.e., files that can be deleted without loss)",
                "\n - put test code into sandbox.",
                "\n-----------------------------------------------------------\n",
                "\nDescription:\n\nThis project is about ..."
               ,file=readme,append=TRUE,sep="")
            dir.create(paste0(directory,"/","R-code"))
            temp.R <- paste0(directory,"/","R-code/template-R-code.R")
            file.create(temp.R)
            cat("# Created: ",as.character(Sys.Date()),
                "\n# Author(s): ",as.character(Sys.info()[["login"]]),
                "\n# Input files: (list data sources and other scripts which deliver data for this file)",
                "\n# Output files: (list processed data files and other output such as figures and tables)",
                "\n# Description:\n\n# This script is about ..."
               ,file=temp.R,append=TRUE,sep="")
            dir.create(paste0(directory,"/","sas-code"))
            temp.SAS <- paste0(directory,"/","sas-code/template-SAS-code.sas")
            file.create(temp.SAS)
            cat("/*\nCreated: ",as.character(Sys.Date()),
                "\nAuthor(s): ",as.character(Sys.info()[["login"]]),
                "\nInput files: (list data sources and other scripts which deliver data for this file)",
                "\nOutput files: (list processed data files and other output such as figures and tables)",
                "\nDescription:\n\nThis script is about ...\n*/"
               ,file=temp.SAS,append=TRUE,sep="")
            dir.create(paste0(directory,"/","sandbox"))
            dir.create(paste0(directory,"/","temp"))
            dir.create(paste0(directory,"/","tables"))
            dir.create(paste0(directory,"/","figures"))
            dir.create(paste0(directory,"/","data"))
            message("Empty project created. Please read ",readme)
        }
    }
}


######################################################################
### createProject.R ends here
