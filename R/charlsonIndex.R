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
#' provided in a supplement for publication.  The data included in this package
#' "charlson.codes" includes a selection of ICD8 and ICD10 codes.  ICD9 has
#' never been used in Denmark.
#' 
#' If there are repeats of patient id with different index date (data.date), 
#' then Charlson index is calcultated independently for each of these dates.
#' 
#' Note: Only cases that have at least one of the relevant diseases will be 
#' given values with this function.  The remaining should manually be provided
#' zeroes.
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
#' 
#' Note: The function only calculates a charlson index for those observations
#' where the index is at least "1".  Cases with zero appears as NA and need to
#' be converted to zero after the function.
#' @return A list with 2 data.tables - index holding charlson index and elements 
#' holding presence of each category
#' @examples
#' require(data.table)
#' set.seed(211)
#' adm <- simAdmissionData(10)
#' adm[,charlson.date:=as.Date("2017-01-01")]
#' adm2 <- adm[1:5]
#' adm2[,charlson.date:=as.Date("2015-01-01")]
#' adm <- rbind(adm,adm2)
#' ci <- charlsonIndex(adm,ptid='pnr',vars='diag',data.date='inddto',
#'   charlson.date="charlson.date")
#' @export
charlsonIndex <- function(data,ptid='pnr',vars,data.date,charlson.date,look.back=5,ccodes=charlson.codes){
  weight=component=dcast=X=severe.liver.disease=mild.liver.disease=diabetes.with.complications=diabetes.without.complications=
    metastatic.solid.tumor=any.malignancy=melt=value=NULL
  if(!"data.frame" %in% class(data)) stop('data not dat.fram or data.table')
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
    weight=c(1,1,1,1,1,1,1,1,1,3,1,2,2,2,2,6,6,2,2)
  )
  datt <- datt[data.date>=charlson.date-time*365.25 & data.date<=charlson.date]
  search.vars <- vars
  codes <- ccodes
  datt <- findCondition(datt,search.vars,keep=c("ptid","charlson.date"),codes,match='start')
  setkeyv(datt,c("ptid","X"))
  datt <- unique(datt) # Only one of each
  # Elements:
  elements <- copy(datt)
  elements[,component:=1]
  elements <- dcast(elements,ptid+charlson.date~X,value.var="component")  
  for (cond in charlson.weights[,X]){ 
    elements[!exists(cond),(cond):=0]
  }
  elements[is.na(elements)] <- 0
  # Remove double points for severe/mild
  elements[severe.liver.disease==1,mild.liver.disease:=0]
  elements[diabetes.with.complications==1,diabetes.without.complications:=0]
  elements[metastatic.solid.tumor==1,any.malignancy:=0]
  # Index
  index <- melt(elements,id.vars=c("ptid","charlson.date"),measure.vars=charlson.weights[,X],variable.name="X",value.name="value",variable.factor=FALSE)
  index <- merge(index,charlson.weights,by="X",all=TRUE)
  index[,weight:=weight*value]
  index <- index[,list(charlson.index=sum(weight)),by=c("ptid","charlson.date")]
  #Output
  setnames(elements,c("ptid","charlson.date"),c(ptid,charlson.date))
  setnames(index,c("ptid","charlson.date"),c(ptid,charlson.date))
  charlson.index <- list(index,elements)
  charlson.index
}
