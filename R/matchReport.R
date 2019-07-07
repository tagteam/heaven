#' @title matchReport - report of matching from riskSetMatch
#' 
#' @description  
#' The function provides very simple tables of the success of finding controls and
#' the reuse of cases and controls. Designed to deal with the results of function
#' riskSetMatch
#' @usage
#' matchReport(dat, id, case, caseid,oldcase="oldevent") 
#' @param data result of exposureMatch or incidenceMatch
#' @author Christian Torp-Pedersen & Thomas Alexander Gerds
#' @details 
#' This function can be helpful to define matching options.
#' If there is excessive reuse of controls or many
#' cases do not find controls it may be desirable to do
#' further rounding of matching variables.
#' @return Three tables - Number of controls for cases, use/reuse of controls
#' @seealso riskSetMatch
#' @export
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15)) 
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' caseIndex <- c(seq(1,40,1),seq(5,47,3))
#' controlIndex <- caseIndex
#' library(data.table)
#' dat <- data.table(ptid,case,sex,byear,caseIndex,controlIndex)
#' # Very simple match without reuse - no dates to control for
#' dataout <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="caseIndex",
#'   controlIndex="controlIndex",reuseCases=TRUE,reuseControls=TRUE)
#' matchReport(dataout,"ptid","case","caseid")   
matchReport <- function(data){
    .SD=.N=N=caseid=NULL
    vnames <- names(data)[1:3]
    id=vnames[1]
    case=vnames[2]
    datt <- data[,.SD,.SDcols=c(id,case,"case.id")] # select relevant data
    setnames(datt,c("id","case","caseid"))
    cat("\n","------------------------------------------------------------------","\n","Matching success")
    ## Matching success:
    controls <- datt[,.N,by=caseid]
    controls <- as.vector(controls[,N]-1)
    controls <- table(controls)
    cat("\n", "Line 1, number of controls found - Line 2, number of occurrences")
    print(controls)
    cat("\n","------------------------------------------------------------------")
                                        #Reuse of controls
    controls<-datt[case==0,.N,by=id]
    controls<-with(controls,table(N))
    cat("\n","Reuse/use of controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
    print(controls)
    cat("\n","------------------------------------------------------------------")
}

 
 
