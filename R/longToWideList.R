#' @title longToWideList
#' @description
#' This function is designed for the RTMLE package where time dependent 
#' covariates are reqiored to be a list of data.tables. The input is a
#' dataframe og data.table with an ID variable and columns for each covariate.
#' The output is a named list of wide data.tables as required by RTMLE.
#' 
#' @usage
#' longToWideList(data,ID,variables)
#' @author Christian Torp-Pedersen
#' @param data is a dataframe or data.table with the input
#' @param ID is the variable defining the individual
#' @param invars vector of column names for id/entry/exit - in that 
#' order, example: c("id","start","end")
#' @param variables is a vector of variable names to be managed
#' @return
#' The function returns a named list of data.tables.  Each element corresponds
#' to a variable is has the original variable name. Each data.table has the 
#' following columns:  The ID variable and then a sequences of variables 
#' condstructed by 1) the variable name 2) "_" and 3) numbers from zero to as
#' many periods were planned for the RTMLE analysis.
#' @export
#' @details 
#' The program checks that input variables correct type.  It is assumed that the
#' input has an ID column and then a columne for each relevant variable in the
#' "variables" vector.  For each individual it is assumed that there are an 
#' equal number of records corresponding to the time intervals in the RTMLE
#' analysis.
#' 
#' It is important that the data table is sorted such that blocks of rows 
#' represent all data from an individual in the correct sequence of time slices
#' for the RTMLE analysis.
#' @seealso RTMLE
#' @examples
#' library(data.table)
#' dat <- data.table(pnr=c(as.character(rep(1:3,3))),
#'                     A=c(0,1), B=c(1,0))
#' #setkey(dat,"pnr")
#' out <- longToWideList(dat,"pnr",c("A","B"))                
#' @export
longToWideList <- function(data,ID,variables){
  setDT(data)
  out <- melt(data,ID,variables)
  out[,num:=(0:(.N-1)),by=c(ID,"variable")]
  out[,variable2:=paste0(variable,"_",num)]
  out <- split(out,by="variable")
  out <- lapply(out,function(x)dcast(x,as.formula(paste0(ID,"~variable2")),value.var = "value"))
  out[]
}
