#' @title importDREAM
#' @description
#' The DREAM dataset is a SAS dataset where all in the Danish population that
#' have received public support are recorded. A number of variables are 
#' presented. For a detailed description of DREAM refer to www.dreammodel.dk
#' 
#' The current function can output 2 datasets from the DREAM database 
#' \itemize{
#' \item{support}{  A long version data.table providing a sequential list of
#' time periods and the codes for public support.  Description of codes can be
#' identified a number of places and for those working on Statistics Denmark
#' there are SAS format codes available}
#' \item{branche}{  A long version data.table providing a record per month for
#' type of profession with start/end/work-code. SAS formats are also available
#' for these codes}
#' }
#' @usage  
#' importDREAM(dreamData,type="support",pnr="pnr",explData=NULL,)
#' @author Christian Torp-Pedersen
#' @param dreamData Name of dataset holding DREAM data. Typically product of
#' the importSAS function.  In most cases it will be useful to add a set.hook
#' to the importSAS function - set.hook="keep PNR branch:" for type of work
#' and set.hook="keep=PNR y_:" for support
#' @param type - "support" extracts weekly public support and "branche" supplies
#' montly type of work. Other names cause error.
#' @param pnr - name of person identifier in DREAM dataset, typically "PNR"
#' @param explData Name of data holding relation between codes and explanatory
#' text. Typically read from "//srvfsenas1/formater/SAS formater i Danmarks 
#' Statistik/txt_filer" using "read.csv2".  The data is expected to hold two
#' variables: the code and the explanation - in that order.  The variable
#' holding the code MUST be named "branche" or "support" as appropriate.
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
#' temp <- importDREAM(microDREAM,type="branche",pnr="PNR",explData=branche)   
#' temp[]
#' temp2 <- importDREAM(microDREAM,type="support",pnr="PNR",explData=support)
#' temp2[]         
#' @export
importDREAM <- function (dreamData, type = "support", pnr = "pnr",explData = NULL) 
{
  .SD <- melt <- patterns <- start <- num_b <- branche <- lastdate <- .N <- end <- char_week <- week <- year <- support <- na.omit <- NULL
  setDT(dreamData)
  if (!is.null(explData)) {
    setDT(explData)
    if (!type %in% names(explData)) 
      stop("One column in explData must be 'type' in order to correctly merge")
  }
  if (!type %in% c("support", "branche")) 
    stop("type must be \"support\" or \"branche\"")
  if (type == "branche") {
    cols <- c(pnr, grep("branch", names(dreamData), value = TRUE))
    DREAM <- dreamData[, .SD, .SDcols = cols]
    changeCols <- cols[-1]
    DREAM <- DREAM[, `:=`((changeCols), lapply(.SD, as.character)), .SDcols = changeCols]
    DREAM <- melt(DREAM, measure = patterns("branch"), variable.name = "date", value.name = "branche")
    DREAM[, `:=`(start, as.Date(paste0(gsub("branche_", "", as.character(date)), "_01"), format = "%Y_%m_%d"))]
    setkeyv(DREAM, c(pnr, "start"))
    DREAM[,rl:=rleid(branche)]
    DREAM <- DREAM[,.(start=start[1],end=start[.N]+30),by=c(pnr,"rl","branche")]
  }
  else {
    cols <- c(pnr, grep("y_", names(dreamData), value = TRUE))
    DREAM <- dreamData[, .SD, .SDcols = cols]
    changeCols <- cols[-1]
    DREAM <- DREAM[, `:=`((changeCols), lapply(.SD, as.character)), .SDcols = changeCols]
    DREAM <- melt(DREAM, measure = patterns("y_"), variable.name = "date", value.name = "support")
    DREAM[, `:=`(year = as.integer(substr(date, 3, 4)), week = as.integer(substr(date, 5, 6)))]
    #DREAM[, `:=`(char_week, fifelse(week < 10, as.character(paste0("0", week)), as.character(week)))]
    DREAM[, `:=`(year, fifelse(year < 90, year + 2000L, year + 1900L))]
    #DREAM[, `:=`(start, ISOweek::ISOweek2date(paste0(year, "-W", char_week, "-1")))]
    setkeyv(DREAM, c(pnr,"year","week"))
    #setkeyv(DREAM, c(pnr, "start"))
    DREAM[,rl:=rleid(support),by=pnr]
    #DREAM <- DREAM[,.(start=start[1],end=start[.N]+7),by=c(pnr,"rl","support")]
    DREAM <- DREAM[,.(startweek=week[1],endweek=week[.N],
                      startyear=year[1],endyear=year[.N]),by=c(pnr,"rl","support")]
    DREAM[, `:=`(char_week_start, fifelse(startweek < 10, as.character(paste0("0", startweek)), as.character(startweek)))]
    DREAM[, `:=`(char_week_end, fifelse(endweek < 10, as.character(paste0("0", endweek)), as.character(endweek)))]
    DREAM[,':='(start=ISOweek::ISOweek2date(paste0(startyear, "-W", char_week_start, "-1")),
                end=ISOweek::ISOweek2date(paste0(endyear, "-W", char_week_end, "-1"))+7)]
    DREAM <- DREAM[,.SD,.SDcols=c(pnr,"support","start","end","rl")]
  }
  if (!is.null(explData)) {
    DREAM <- merge(DREAM, explData, by = type, all.x = TRUE)
    setkeyv(DREAM, c(pnr, "start"))
  }
  DREAM[,rl:=NULL]
  DREAM
}
