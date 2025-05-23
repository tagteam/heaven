#' @title splitFromTo
#' @description 
#' splitFromTo is devised to split a series of records in multiple records based
#' on entry/exit dates provided from another splitting guide dataset. The function
#' can handle multiple different events with each type of event characterized by
#' the variable "value".
#' 
#' The function is useful for analysis of data where temporary events occur.
#' These may be drugs treatment, pregnancy etc.  For a situation of pregnancy
#' for example it may be useful for analysis to split a person into three 
#' records representing time before, during and after pregnancy.
#' 
#' The input is first "base data" id/in/out (and other variables) that
#' may already have been split by other functions.  The other data is a sequence 
#' of id/from/to/value/name - that may represent multiple conditions with 
#' from/to and with "name" to distinguish.
#' 
#' The function further implements a simplification where only a single "name"
#' and no "value" is provided.
#' @usage
#' splitFromTo(indat,splitdat,invars,splitvars,default="0",datacheck=TRUE)
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end and other data - possibly 
#' already split
#' @param splitdat - Data with splitting guide - id/from/to/value/name 
#' @param invars - vector of column names for id/entry/exit - in that 
#' order, 
#' example: c("id","start","end")
#' @param splitvars - vector of column names containing dates to split by.
#' example: c("id","start","end","value","name") - must be in that order!
#' @param default - Value given to intervals not given a value by the function.
#' @param datacheck - This function can crash or produce incorrect results if
#' input data have overlapping intervals or negative intervals in any of the two
#' input datasets.  Thic is checked and error produced by datacheck. Can be 
#' set to FALSE if data are checked otherwise
#' @return
#' The function returns a new data table where records have been split according 
#' to the splitting guide dataset. Variables unrelated to the splitting are 
#' left unchanged.
#' @details 
#' The input to this function are two data.tables and two lists of the critical 
#' variables.  The BASE data it the data to be split.
#' This data must have a variable to identify participants and start/end times. 
#'
#' The other table (SPLITTINGUIDE) contains possibly multiple records for each 
#' participants with id/from/to/value/name - or just id/from/to
#' 
#' The program checks that intervals are not negative and that intervals for 
#' each "name" and each individual do not overlap.  Violation stops the program 
#' with error. Overlaps may occur in real situations, but the user needs to make 
#' decisions regarding this prior to using this function.
#' 
#' It is required that the splitting guide contains at least one record.  
#' Missing data for key variables are not allowed and will cause errors.
#' 
#' This function is a wrapper to the lexisFromTo function with the change that 
#' "events" are not considered.
#' @seealso splitFromTo
#' @examples
#' library(data.table)
#' dat <- data.table(id=c("A","A","B","B","C","D"),
#'                  start=as.Date(c(0,100,0,100,200,400),origin='1970-01-01'),
#'                  end=as.Date(c(100,200,100,200,300,500),origin='1970-01-01'))
#' split <- data.table (id=c("A","A","A","A","B","B","B","D","D"),
#'                     start=as.Date(c(0,50,25,150,110,150,400,300,500),origin='1970-01-01'),
#'                     end= as.Date(c(25,75,150,151,120,250,500,400,500),origin='1970-01-01'),
#'                     value=c(1,2,3,4,1,2,3,6,7),
#'                     name=c("d1","d1","d2","d2","d1","d1","d2","d3","d4"))
#' #Show the dataset:
#' dat[]
#' split[]                   
#' temp <- splitFromTo(dat # inddato with id/in/out/event
#'                    ,split # Data with id and dates
#'                    ,c("id","start","end") #names of id/in/out/event - in that order
#'                   ,c("id","start","end","value","name")) #Names var date-vars to split by
#' temp[]                   
#' # Short splittingguide with only id/start/end
#' split2 <- data.table (id=c("A","A","B","D"),
#'                     start=as.Date(c(0,50,110,150),origin='1970-01-01'),
#'                     end= as.Date(c(25,75,120,250),origin='1970-01-01'))
#'                     
#' temp2 <- splitFromTo(dat # inddato with id/in/out/event
#'                    ,split2 # Data with id and dates
#'                    ,c("id","start","end") #names of id/in/out - in that order
#'                   ,c("id","start","end")) #Names var date-vars to split by   
#' temp2[]                                   
#' @export
splitFromTo <- function(indat # inddato with id/in/out/event - and possibly other variables
                       ,splitdat # Data with from/to/Value
                       ,invars #names of id/in/out/event - in that order
                       ,splitvars #Names in splitdat with pnr/from/to/value/name
                       ,default="0"
                       ,datacheck=TRUE
                        ){
  #browser()
  dummyvariable_ <- NULL
  setDT(indat)
  if(length(splitvars)==3){
    splitvars <- c(splitvars,"dummyvariable_value","dummyvariable_name")
    splitdat[,':='(dummyvariable_value=1,dummyvariable_name="dummyvariable_name")]
  }  
  indat[,dummyvariable_:=1]
  dat <- lexisFromTo(indat,splitdat,c(invars,"dummyvariable_"),splitvars,
                     default,datacheck)
  if("dummyvariable_name" %in% splitvars){
    indat[,dummyvariable_:=NULL]
    splitdat[,c("dummyvariable_value","dummyvariable_name"):=NULL]
    dat[,c("dummyvariable_","dummyvariable_name"):=NULL]
  } else {
    indat[,dummyvariable_:=NULL]
    dat[,dummyvariable_:=NULL]
  }  
  dat[]
}
