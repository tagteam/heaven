#' @title matchReport - report of matching from riskSetMatch
#' 
#' @description  
#' The function provides very simple tables of the success of finding controls and
#' the reuse of cases and controls. Designed to deal with the results of function
#' riskSetMatch
#' @usage
#' matchReport(dat, id, case, caseid,oldcase="oldevent") 
#' @param dat - data.table with result from riskSetMatch
#' @param id - variable with participant id
#' @param case - 0=control, 1=case
#' @param caseid - variable defining the groups of matching cases/controls
#' @param oldcase - Variable holding case/control=0/1 prior to matching. Distinguishes
#' cases reused as controls
#' @author Christian Torp-Pedersen
#' @details 
#' This function can be helpful to define matching options.  If there is excessive reuse of controls or many
#' cases do not find controls it may be desirable to do further rounding of matching variables.
#' @return Three small tables - Number of controls for cases, use/reuse of controls, use/reuse of cases
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
matchReport <- function(dat, # Datatable with matches
                        id, # Participant identification
                        case, # 0=control, 1=case
                        caseid, # grouping variable
                        oldcase="oldevent" # remember whether a case used as control was originally a case
                        ){
 datt <- dat[,.SD,.SDcols=c(id,case,caseid,oldcase)] # select relevant data
 setnames(datt,c("id","case","caseid","oldcase"))
 cat("\n","------------------------------------------------------------------","\n","Matching success")
 #Matching success:
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
 #Reuse of cases
 cat("\n","Use and reuse of cases as controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
 cases <- datt[oldcase==1,.N,by=id]
 cases <- with(cases,table(N))
 print(cases)
 cat("\n","------------------------------------------------------------------")
}

 
 
