#' @title importDREAM
#' @description
#' The DREAM dataset is a SAS dataset where all in the Danish population that
#' have received public funding are recorded. A number of variables are presen-
#' ted. For a detailed description of DREAM refer to www.dreammodel.dk/
#' The current function can output 2 datasets from the DREAM database 
#' \itemize{
#' \item{support}{A long version data.table providing a record per week for all 
#' registered individual with start/end/supportcode and support description}
#' \item{branche}{A long version data.table providing a record per month for
#' type of profession with start/end/work-code and description}
#' }
#' The description provided must be merged from a text file with matching code
#' and description.  These text-files can be found in statistics Denmark in the
#' area holding standard Statistics Denmark formats: "//srvfsenas1/formater/
#' SAS formater i Danmarks Statistik/txt_filer". Multiple formats are available
#' for more or less crude grouping.
#' @usage
#' importDREAM(dreamData,explData=NULL,type="support",pnr="PNR")
#' @author Christian Torp-Pedersen
#' @param dreamData Name of dataset holding DREAM data. Typically product of
#' the importSAS function.  In most cases it will be useful to add a set.hook
#' to the importSAS function - set.hook="keep PNR branch:" for type of work
#' and set.hook="keep=PNR y_:" for support
#' @param explData Name of data holding relation between codes and explanatory
#' text. Typically read from "//srvfsenas1/formater/SAS formater i Danmarks 
#' Statistik/txt_filer" using "read.csv2".  The data is expected to hold two
#' variables: the code and the explanation - in that order.  The variable
#' holding the code MUST be named "branche" or "support" as appropriate.
#' @param type - "support" extracts weekly public support and "branche" supplies
#' montly type of work. Other names cause error.
#' @param pnr - name of person identifier in DREAM dataset, typically "PNR"
#' @return
#' the function returns a data.table with the following variables:
#' \itemize{
#' \item pnr - person identificer from call
#' \item start - start of each period
#' \item end - end of each period
#' \item code - coding of period
#' \item explanation - text to eplain coding if a text dataset was specified
#' }
#' @export
#' @details 
#' DREAM data are organized in a special way.  From the start of recording there
#' is a weekly recording of receipt of support from the state. These are in se-
#' parate variables.  Then there is a montly recording of type of work also in
#' separate variables. Other data are in DREAM, but not extracted by current 
#' function which either selects public support or type of work.
#' The result is either occupation (branche) or support-type with start and
#' end dates and in long format.
#' @seealso importSAS
#' @examples
#' # The following is a minute version of DREAM
#' library(data.table)
#' microDREAM <-data.table(PNR=c(1,2),branche_2008_01=c("","3"),
#'   branche_2008_02=c("1","3"), branche_2008_03=c("1",""),
#'   y_9201=c("","1"),y_9202=c("","2"),y_9203=c("","2"))
#' # Explanation of codes for "branche" 
#' branche <- data.table(branche=c("1","2","3"),
#'             text=c("school","gardening","suage"))   
#' support <- data.table(support=c("1","2"),text=c("education","sick")) 
#' temp <- importDREAM(microDREAM,branche,type="branche",pnr="PNR")   
#' temp[]
#' temp2 <- importDREAM(microDREAM,support,type="support",pnr="PNR")
#' temp2[]         
#' @export
importDREAM <- function(dreamData,explData=NULL,type="support",pnr="PNR")
{
  setDT(dreamData)
  if (!is.null(explData)){
    setDT(explData)
    if (!type %in% names(explData)) stop('One column in explData must be type in order to correctly merge')
  } 
  if (!type %in% c("support","branche")) stop('type must be "support" or "branche"')
  if (type=="branche"){
    cols<-c(pnr,grep("branch",names(dreamData),value=TRUE))
    DREAM <- dreamData[,.SD,.SDcols=cols]
    changeCols <- cols[-1] # other than pnr
    DREAM <- DREAM[,(changeCols):=lapply(.SD,as.character),.SDcols=changeCols] # made all branche as character
    DREAM <- melt(DREAM,measure=patterns("branch"),variable.name="date",value.name="branche") #transpose
    #Define start of periods
    DREAM[,start:=as.Date(paste0(gsub("branche_","",as.character(date)),"_01"),format="%Y_%m_%d")] 
    setkeyv(DREAM,c("PNR","start")) #sort
    DREAM[,num_b:=as.numeric(branche)] #Numeric version of branche - requirement for diff
    DREAM[is.na(num_b),num_b:=-1] #diff must have integers, not NA
    DREAM[,lastdate:=seq(start[.N],by="1 months",length=2)[2],by="PNR"] # Last date each PNR + 1 month
    DREAM[,diff:=c(999,diff(num_b,lag=1))] # difference between columns with NA for first
    DREAM <- DREAM[diff!=0] # remove repetitions - leaves 1 record for each period of branche
    DREAM[,end:=shift(start,type="lead")] # provides end dates
    DREAM[is.na(end),end:=lastdate] #provides end date for last record of each PNR
    DREAM[,c("date","num_b","diff","lastdate"):=NULL] # remove temporary columns
  }
  else{
    cols<-c(pnr,grep("y_",names(dreamData),value=TRUE))
    DREAM <- dreamData[,.SD,.SDcols=cols]
    changeCols <- cols[-1] # other than pnr
    DREAM <- DREAM[,(changeCols):=lapply(.SD,as.character),.SDcols=changeCols] # made all support as character
    DREAM <- melt(DREAM,measure=patterns("y_"),variable.name="date",value.name="support") #transpose
    #Define start of periods
    DREAM[,':='(year=as.integer(substr(date,3,4)),week=as.integer(substr(date,5,6)))]
    DREAM[,char_week:=ifelse(week<10,as.character(paste0('0',week)),as.character(week))]
    DREAM[,year:=ifelse(year<90,year+2000L,year+1900L)]
    DREAM[,start:=ISOweek::ISOweek2date(paste0(year,"-W",char_week,"-1"))] 
    setkeyv(DREAM,c("PNR","start")) #sort
    DREAM[,num_b:=as.numeric(support)] #Numeric version of support - requirement for diff
    DREAM[is.na(num_b),num_b:=-1] #diff must have integers, not NA
    DREAM[,lastdate:=start[.N]+7,by="PNR"] # Last date each PNR + 1 week
    DREAM[,diff:=c(999,diff(num_b,lag=1))] # difference between columns with NA for first
    DREAM <- DREAM[diff!=0] # remove repetitions - leaves 1 record for each period of support
    DREAM[,end:=shift(start,type="lead")] # provides end dates
    DREAM[is.na(end),end:=lastdate] #provides end date for last record of each PNR
    DREAM[,c("date","num_b","diff","lastdate","year","week","char_week"):=NULL] # remove temporary columns
  }
  if (!is.null(explData)){
    #merge
    DREAM <- merge(DREAM,explData,by=type,all.x=TRUE)
    setkeyv(DREAM,c(pnr,"start"))
  }
  DREAM
}


