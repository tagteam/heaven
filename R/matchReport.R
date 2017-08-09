#' @title matchReport - report of matching from riskSetMatch
#' 
#' @description  
#' The function provides very simple tables of the success of finding controls and
#' the reuse of cases and controls
#' 
#' 
#' @usage
#' matchReport(dat, id, case, caseid) 
#' @param dat - data.table with result from riskSetMatch
#' @param id - variable with participant id
#' @param case - 0=control, 1=case
#' @param caseid - variable defining the groups of matching cases/controls
#' 
#' @author Christian Torp-Pedersen
#' 
#' 
#' @details 
#' This function can be helpful to define matching options.  If there is excessive reuse of controls or many
#' cases do not find controls it may be desirable to do further rounding of matching variables.
#'
#' @return Three small tables - Number of controls for cases, use/reuse of controls, use/reuse of cases
#' 
#' @seealso riskSetMatch
#' 
#' @export
#'
#' @examples
#' require(data.table)
#' event <- c(rep(0,50),rep(1,5)) 
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",25),rep("mal",25),"fem","fem",rep("mal",3))
#' age <- c(rep(c(70,80),25),70,80,70,70,80)
#' caseIndex <- c(seq(1,49,1),seq(5,45,10))
#' controlIndex <- caseIndex
#' library(data.table)
#' dat <- data.table(ptid,event,sex,age,caseIndex,controlIndex)
#' out2 <- RiskSetMatch("ptid","event",c("age","sex"),dat,2,caseIndex="caseIndex",
#'   controlIndex="controlIndex")
#' matchReport(out2,"ptid","event","caseid")   
matchReport <- function(dat, # Datatable with matches
                        id, # Participant identification
                        case, # 0=control, 1=case
                        caseid # grouping variable
                        ){
#browser()  
 require(data.table)
 datt <- dat[,.SD,.SDcols=c(id,case,caseid)] # select relevant data
 datt[,.SD,.SDcols=c(id,case,caseid)] # select relevant data
 cat("\n","------------------------------------------------------------------","\n","Matching success")
 #Matching success:
 controls <- datt[,.N,by=caseid]
 controls <- as.vector(controls[,N]-1)
 controls <- table(controls)
 cat("\n", "Line 1, number of controls found - Line 2, number of occurrences")
 print(controls)
 cat("\n","------------------------------------------------------------------")
 #Reuse of controls
 controls<-datt[event==0,.N,by=id]
 controls<-with(controls,table(N))
 cat("\n","Reuse/use of controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
 print(controls)
 cat("\n","------------------------------------------------------------------")
 #Reuse of cases
 cat("\n","Use and reuse of cases as controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
 cases <- datt[event==1,.N,by=id]
 cases <- with(cases,table(N))
 print(cases)
 cat("\n","------------------------------------------------------------------")
}

 
 