#' @title Hypertension during follow up
#'
#' @param data Data set with drugs indicated by atc codes and date of drugs, e.g. lmdb 
#' @param dateBL Baseline date "YYYY-MM-DD"
#' @param dateFU Follow up date "YYYY-MM-DD"
#' @param pnr Variable with ID for each subject/group. Default name: pnr 
#' @param atc Variable with atc codes. Must be type character. Default name: atc 
#' @param eksd Variable with dates. Must be type Date or numeric. Default name: eksd
#'
#' @return A variable indicating wheter there is hypertension during follow up. The variable hypertension during follow up is one if 
#' the person has received two or more types of anti-hypertensive medications within 90 days in two quarters in a row. 
#' Devision in quaters (90 days) from the baseline date.
#' The codes for hypertension are currently defined in the function.
#' Output: A data set with pnr and hypertension variable
#' 
#' @export
#'
#' @examples
#' library(data.table)
#' dat<-data.table(pnr=1:50, atc=rep(c("C09CA04", "C04AD03", "C08DB01", "C07FB02", "C01DA02"), each=10), eksd=c("2002-01-23" ,"2001-02-15", "2001-03-23", "2001-03-01"))
#' dat$eksd<-as.Date(dat$eksd)
#' dat
#' baseline<-"2001-03-30"
#' #Create hypertension variable
#' ht<-hypertensionBaseline(dat,baseline)
#' ht
#' #With custom names
#' dat2<-data.table(MyID=1:50, MyAtcCodes=rep(c("C09CA04", "C04AD03", "C08DB01", "C07FB02", "C01DA02"), each=10), MyDates=c("2002-01-23" ,"2001-02-15", "2001-03-23", "2001-03-01"))
#' dat2$MyDates<-as.Date(dat2$MyDates)
#' dat2
#' ht2<-hypertensionBaseline(dat2, baseline, pnr='MyID', atc='MyAtcCodes', eksd='MyDates')
#' ht2
#' #Several rows per pnr
#' dat3<-data.table(pnr=1:15, atc=rep(c("C09CA04", "C04AD03", "C08DB01", "C07FB02", "C01DA02", "C09BB"), each=10), eksd=c("2002-01-23" ,"2001-02-15", "2001-03-23", "2001-03-01"))
#' dat3$eksd<-as.Date(dat3$eksd)
#' dat3
#' ht3<-hypertensionBaseline(dat3,baseline)
#' ht3
#' @author Helle Højmark Eriksen <helle.e@@rn.dk>
hypertensionFU<- function(data,dateBL,dateFU,pnr='pnr',atc='atc',eksd='eksd'){
  
  options(warn=1)
  if(is.null(data)){
    warning("Argument 'data' is missing")
    return(NA)
  }
  if(is.null(dateBL)){
    warning("Argument 'dateBL' is missing")
    return(NA)
  }
  if(regexpr("-", dateBL, fixed=T)[1]!=5){
    warning("Argument 'dateBL' must have the format YYYY-MM-DD")
    return(NA)
  }
  if(!is.null(dateBL)&&is.na(as.Date(dateBL))){
    warning("Argument 'dateBL' is not a date")
    return(NA)
  }
  if(is.null(dateFU)){
    warning("Argument 'dateFU' is missing")
    return(NA)
  }
  if(regexpr("-", dateFU, fixed=T)[1]!=5){
    warning("Argument 'dateFU' must have the format YYYY-MM-DD")
    return(NA)
  }
  if(!is.null(dateFU)&&is.na(as.Date(dateFU))){
    warning("Argument 'dateFU' is not a date")
    return(NA)
  }
  
  ## Make into data.table 
  d <- as.data.table(data)
  
  ## Konvert to date
  if(!is.null(dateBL)){
    date<-as.Date(dateBL)
  }
  if(!is.null(dateFU)){
    date<-as.Date(dateFU)
  }
  
  ## A different name for the variables: xTempName
  setnames(d,pnr,'pnrxTempName')
  setnames(d,atc,'atcxTempName')
  setnames(d,eksd,'eksdxTempName')
  
  ## Check variable types 
  if( !( class(d[,eksdxTempName])=="Date" | is.numeric(d[,eksdxTempName]) ) ){stop("eksd must be numeric or Date")} 
  if( !is.character(d[,atcxTempName]) ){stop("atc must be character")} #else grep won't work
  
  ## Removes invalid dates and atc codes
  #eksd
  outerrorTemp <- copy(d[is.na(eksdxTempName)])
  d <- d[!is.na(eksdxTempName)]
  if( dim(outerrorTemp)[1]!=0 ){message("Some eksd are missing and have been removed")}
  #atc
  outerror=rbind(outerrorTemp, d[atcxTempName=='',] ) #should we output this also?
  d <- d[atcxTempName!='']
  if( dim(outerror)[1]!=dim(outerrorTemp)[1] ){message("Some atc are missing and have been removed")}
  
  #All anti-hypertensive medication
  atc_C <- d[unlist(lapply("^C",grep,atcxTempName))]
  
  #Population with follow up period (between "index" and "FU")
  pop<-d[,1] #pnr
  setkey(pop,pnrxTempName)
  pop<-pop[.(unique(pnrxTempName)),,mult="first"] #unique pnr
  pop$index<-dateBL #index/baseline
  pop$fu<-dateFU #follow up
  hyp<-data.table(pop$pnrxTempName,pop$index, pop$fu)
  
  #Merge of population and medicaiton
  atc_Cm<-data.table(atc_C$pnrxTempName,atc_C$eksdxTempName,atc_C$atcxTempName)
  colnames(atc_Cm) <- c("pnrxTempName","eksdxTempName","atcxTempName")
  colnames(hyp) <- c("pnrxTempName","index","fu")
  
  hyp2<-merge(hyp, atc_Cm, by="pnrxTempName", all.x=TRUE)
  
  #Only prescriptions from baseline to followup
  hyp2<-hyp2[hyp2$index<=hyp2$eksdxTempName & hyp2$eksdxTempName<=hyp$fu]
  
  #Calculate quarter number from the baseline date
  hyp2$eksd<-as.integer(hyp2$eksdxTempName)
  hyp2$index<-as.integer(as.Date(hyp2$index))
  hyp2$eksd_ba<-hyp2$eksd-hyp2$index
  hyp2$eksd_90<-hyp2$eksd_ba%/%90 + as.logical(hyp2$eksd_ba%%90)
  
  #Defining different treatment regims
  #The definitions must be moved outside the function!
  antiAdrenerg4=c('C02A','C02B','C02C')
  diu4=c('C02L','C03A','C03B','C03D','C03E','C03X','C07B','C07C','C07D','C08G')
  diu5=c('C02DA','C09BA','C09DA')
  diu7=c('C09XA52')
  Vaso5=c('C02DB','C02DD','C02DG')
  Andet4=c('C02L')
  bb4=c('C07A','C07B','C07C','C07D','C07F')
  ccb3=c('C08')
  ccb4=c('C07F','C08G')
  ccb5=c('C09BB','C09DB')
  ras5=c('C09AA','C09BA','C09BB','C09CA','C09DA','C09DB')
  ras7=c('C09XA02','C09XA52')
  
  #Drug categorization
  hyp2$AntiAdrenerg<-as.numeric(substr(hyp2$atcxTempName,1,4) %in% antiAdrenerg4)
  hyp2$diu<-as.numeric(substr(hyp2$atcxTempName,1,5) %in% diu5 | substr(hyp2$atcxTempName,1,4) %in% diu4 | substr(hyp2$atcxTempName,1,7) %in% diu7) 
  hyp2$Andet<-as.numeric(substr(hyp2$atcxTempName,1,4) %in% Andet4)
  hyp2$Vaso<-as.numeric(substr(hyp2$atcxTempName,1,5) %in% Vaso5)
  hyp2$bb<-as.numeric(substr(hyp2$atcxTempName,1,4) %in% bb4)
  hyp2$ccb<-as.numeric(substr(hyp2$atcxTempName,1,4) %in% ccb4 | substr(hyp2$atcxTempName,1,3) %in% ccb3 | substr(hyp2$atcxTempName,1,5) %in% ccb5)
  hyp2$ras<-as.numeric(substr(hyp2$atcxTempName,1,5) %in% ras5 | substr(hyp2$atcxTempName,1,7) %in% ras7)
  
  #Count number of anti-hypertensive drugs pr row
  hyp2$antal_drugs<-0 
  hyp2$antal_drugs<- hyp2$AntiAdrenerg+ hyp2$diu+ hyp2$Andet+ hyp2$Vaso+ hyp2$bb+ hyp2$ccb+ hyp2$ras
  hyp2$antal_drugs[is.na( hyp2$antal_drugs)]<-0 #replace missing with 0
  #Number of drugs per quater
  hyp2[,sum:=sum(antal_drugs), by=list(pnrxTempName,eksd_90)]
  #Hypertension in quarter
  hyp3<-hyp2[,ht_qrt:=sum>=2]
  #Rows with hypertension
  hyp2<-hyp3[ht_qrt==1,]
  
  #Rows without hypertension
  hypno<-hyp3[ht_qrt==0,]
  
  setkey(hypno,pnrxTempName,eksdxTempName) #important for ordering
  #indicator first element per group
  hypno$indicator<-FALSE
  hypno$indicator[which(diff(c(0,as.numeric(hypno$pnrxTempName)))==1)] <- TRUE
  popno<-hypno[indicator==1,]
  popno<-popno[,c(1)]
  popno$HT<-0
  
  #Count number of anti-hypertensive drug and quarters
  setkey(hyp2,pnrxTempName,eksdxTempName) #important for ordering
  #indicator first element per group
  hyp2$indicator<-FALSE
  hyp2$indicator[which(diff(c(0,as.numeric(hyp2$pnrxTempName)))==1)] <- TRUE
  #quarter before
  hyp2[,before:=shift(eksd_90,1), by=pnrxTempName]
  #difference between quaters
  hyp2[,diff:=eksd_90-before]
  hyp2[is.na(get("diff")), diff:=0]
  
  #hypertension if more than one anti-hypertensive drug in (minimum) two quarters in row
  #two quarters in row is indicated by diff equals 1
  hyp2[,HT:=as.numeric(1==hyp2$diff)]
  #hypertension registret on all rows pr patient
  hyp2[,HT:=sum(HT), by=pnrxTempName]
  
  #test<-hyp2
  
  #One row per person with hypertension
  pop<-hyp2[indicator==1,]
  
  #Warning if no C-codes
  if (nrow(popno)==0 & nrow(pop)==0)
  {warning("The dataset contains no atc-code starting with C")}
  
  #Concate person with and without hypertension
  pop<-pop[,c(1,22)]
  pop<-merge(hyp, pop, by="pnrxTempName", all.x=TRUE)
  pop[is.na(get("HT")), HT:=0]
  pop<-pop[,c(1,4)]
  
  setkey(pop,pnrxTempName)
  
  ## Change name in output back
  setnames(pop,'pnrxTempName',pnr)
  
  #return(list(data=pop, test=test))
  return(list(data=pop))
}