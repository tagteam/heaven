#' @title charlsonindex - define charlson index at a selected time
#' @description
#' This function uses 19 disease categories and their weights to establish
#' charlson index at a particular time indicated by a variable
#' 
#' The suggested list of Charlson definitions comes partly from 
#' Quann (Am J Epidemiol.2011 173:676-82), partly from DMCG.dk benchmarking 
#' consortium and with further minor adjustments provided by Peter Ascanius 
#' Jacobsen.  It is unlikely that any international list of definitions will 
#' apply for a Danish Population and the listing used should be either cited or
#' provided in a supplement for publication.
#' @usage charlsonIndex(data,ptid='pnr',vars,data.date,charlson.date,look.back=5
#' ,ccodes=charlson.codes)
#' @author Christian Torp-Pedersen 
#' @param data - A dataframe/table with disease codes and dates
#' @param ptid - Variable defining individual
#' @param vars - variables where disease codes should be searched for
#' @param data.date - variable in data with date of disease
#' @param charlson.date - variable defining index date for determination of 
#' charlson index
#' @param look.back - numnber of years to look back from charlson.date for 
#' diseases
#' @param ccodes - named list of charlson codes. Default uses list 
#' supplied by heaven
#' @details
#' The charlson weights and selection of disease codes are from DCMG.dk
#' @return A list with 2 data.tables - index holding charlson index and elements 
#' holding presence of each category
#' @examples
#' require(data.table)
#' set.seed(211)
#' adm <- simAdmissionData(10000)
#' adm[,charlson.date:=as.Date("2017-01-01")]
#' ci <- charlsonIndex(adm,ptid='pnr',vars='diag',data.date='inddto',
#'   charlson.date="charlson.date")
#' @export
charlsonIndex <- function(data,ptid='pnr',vars,data.date,charlson.date,look.back=5,ccodes=charlson.codes){
  weight=component=dcast=NULL
  if(!"data.frame" %in% class(data)) stop('data not data.fram or data.table')
  if(!class(vars)=='character') stop ('search variables not character')
  if(!class(ptid)=='character') stop ('ptid not character')
  if(!class(data.date)=='character') stop ('data.date not character')
  if(!class(charlson.date)=='character') stop ('charlson.date not character')
  if(!class(look.back)=='numeric') stop('look.back not numeric')
  #Select relevant span in data
  datt <- copy(data)
  setDT(datt)
  setnames(datt,c(ptid,data.date,charlson.date),c("ptid","data.date","charlson.date"))
  time=look.back
  charlson.weights <- data.table(
    X=c("myocardial.infarction","heart.failure","peripheral.vascular.disease",
    "cerebrovascular.disease","dementia","chronic.pulmonary.disease",
    "rheumatic.disease","peptic.ulcer.disease","mild.liver.disease",
    "severe.liver.disease","diabetes.without.complications","diabetes.with.complications",
    "hemiplegia.paraplegia","renal.disease","any.malignancy",
    "metastatic.solid.tumor","AIDS.HIV","leukemia",
    "lymphoma"),
    weight=c(1,1,1,1,1,1,1,1,1,3,1,2,2,2,6,6,6,2,2)
  )
  datt <- datt[data.date>=charlson.date-time*365.25 & data.date<=charlson.date]
  search.vars <- vars
  codes <- ccodes
  datt <- findCondition(datt,"ptid",search.vars,keep="ptid",codes,match='start')
  setkeyv(datt,c("ptid","X"))
  datt <- unique(datt) # Only one of each
  index <- merge(datt,charlson.weights,by="X")
  index <- index[,list(charlson.index=sum(weight)),by=ptid]
  elements <- merge(datt,charlson.weights,by='X')
  elements[,component:=1]
  elements <- dcast(elements,ptid~X,value.var="component")
  elements[is.na(elements)] <- 0
  charlson.index <- list(index,elements)
  charlson.index
}
