#' @title Hypertension at baseline
#'
#' @param data Data set with drugs indicated by atc codes and date of drugs, e.g. lmdb 
#' @param date Baseline date (date of hypertension) "YYYY-MM-DD"
#' @param pnr Variable with ID for each subject/group. Default name: pnr 
#' @param atc Variable with atc codes. Must be type character. Default name: atc 
#' @param eksd Variable with dates. Must be type Date or numeric. Default name: eksd
#'
#' @return A variable indicating wheter there is hypertension at baseline. The variable hypertension at baseline is one if 
#' the person has received two or more types of anti-hypertensive medications within 180 days before 
#' the baseline date. The codes for hypertension are currently defined in the function.
#' Output: A data set with pnr, hypertension variable, number of medications within 180 days before baseline 
#' and types of medications
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
hypertensionBaseline<- function(data,date,pnr='pnr',atc='atc',eksd='eksd'){
  
  options(warn=1)
  if(is.null(data)){
    warning("Argument 'data' is missing")
    return(NA)
  }
  if(is.null(date)){
    warning("Argument 'date' is missing")
    return(NA)
  }
  if(!is.null(date)&&is.na(as.Date(date))){
    warning("Argument 'date' is not a date")
    return(NA)
  }
  
  ## Make into data.table 
  d <- as.data.table(data)
  
  ## Konvert to date
  if(!is.null(date)){
    date<-as.Date(date)
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
  
  #Population with "index" as the time of eventual hypertension
  pop<-d[,1] #pnr
  setkey(pop,pnrxTempName)
  pop<-pop[.(unique(pnrxTempName)),,mult="first"] #unique pnr
  pop$index<-date #index/baseline
  hyp<-data.table(pop$pnrxTempName,pop$index)
  
  #Merge of population and medicaiton
  atc_Cm<-data.table(atc_C$pnrxTempName,atc_C$eksdxTempName,atc_C$atcxTempName)
  colnames(atc_Cm) <- c("pnrxTempName","eksdxTempName","atcxTempName")
  colnames(hyp) <- c("pnrxTempName","index")
  
  hyp2<-merge(hyp, atc_Cm, by="pnrxTempName", all.x=TRUE)
  
  #Only prescriptions around inclusion
  hyp3<-hyp2[-7<=index-eksdxTempName&index-eksdxTempName<=180]
  
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
  hyp3$AntiAdrenerg<-as.numeric(substr(hyp3$atcxTempName,1,4) %in% antiAdrenerg4)
  hyp3$diu<-as.numeric(substr(hyp3$atcxTempName,1,5) %in% diu5 | substr(hyp3$atcxTempName,1,4) %in% diu4 | substr(hyp3$atcxTempName,1,7) %in% diu7) 
  hyp3$Andet<-as.numeric(substr(hyp3$atcxTempName,1,4) %in% Andet4)
  hyp3$Vaso<-as.numeric(substr(hyp3$atcxTempName,1,5) %in% Vaso5)
  hyp3$bb<-as.numeric(substr(hyp3$atcxTempName,1,4) %in% bb4)
  hyp3$ccb<-as.numeric(substr(hyp3$atcxTempName,1,4) %in% ccb4 | substr(hyp3$atcxTempName,1,3) %in% ccb3 | substr(hyp3$atcxTempName,1,5) %in% ccb5)
  hyp3$ras<-as.numeric(substr(hyp3$atcxTempName,1,5) %in% ras5 | substr(hyp3$atcxTempName,1,7) %in% ras7)
  
  #Count number of anti-hypertensive drug
  popm<-pop[,1]
  hyp3m<-data.table(hyp3$pnrxTempName, hyp3$eksdxTempName, hyp3$atcxTempName, hyp3$AntiAdrenerg, hyp3$diu, hyp3$Andet, hyp3$Vaso, hyp3$bb, hyp3$ccb, hyp3$ras)
  colnames(hyp3m) <- c("pnrxTempName","eksdxTempName", "atcxTempName", "AntiAdrenerg","diu", "Andet", "Vaso", "bb", "ccb", "ras")
  pop<-merge(popm, hyp3m, by="pnrxTempName", all.x=TRUE)
  pop$antal_drugs<-0 
  pop$antal_drugs<-pop$AntiAdrenerg+pop$diu+pop$Andet+pop$Vaso+pop$bb+pop$ccb+pop$ras
  pop$antal_drugs[is.na(pop$antal_drugs)]<-0 #replace missing with 0
  
  #Type of drugs
  pop$type1<-ifelse(pop$AntiAdrenerg>0,"AntiAdrenerg","")
  pop$type2<-ifelse(pop$diu>0,"diu","")
  pop$type3<-ifelse(pop$Andet>0,"Andet","")
  pop$type4<-ifelse(pop$Vaso>0,"Vaso","")
  pop$type5<-ifelse(pop$bb>0,"bb","")
  pop$type6<-ifelse(pop$ccb>0,"ccb","")
  pop$type7<-ifelse(pop$ras>0,"ras","")
  pop$type1[is.na(pop$type1)]<-""
  pop$type2[is.na(pop$type2)]<-""
  pop$type3[is.na(pop$type3)]<-""
  pop$type4[is.na(pop$type4)]<-""
  pop$type5[is.na(pop$type5)]<-""
  pop$type6[is.na(pop$type6)]<-""
  pop$type7[is.na(pop$type7)]<-""
  pop$type<-gsub("^\\s+|\\s+$", "", paste(pop$type1,pop$type2,pop$type3,pop$type4,pop$type5,pop$type6,pop$type7, sep=" "))
  
  #drop eksd-column
  pop<-pop[,c(1,3:11,19)] 
  
  #keep unique rows (of pnr and atc)
  setkey(pop)
  pop<-unique(pop)
  
  #drop atc-column
  pop<-pop[,c(1,10:11)] 
  
  #collaps data dy pnr
  setkey(pop,pnrxTempName)
  pop<-setDT(pop)[, list(count=.N, antal_drugs=sum(antal_drugs), type=paste(type, collapse=' ')), by=pnrxTempName]
  
  #keep unique rows (of pnr)
  setkey(pop)
  pop<-unique(pop)
  
  #Hypertension if more than one anti-hypertensive drug
  pop$HT<-as.numeric(2<=pop$antal_drugs)
  
  #drop count
  pop<-pop[,c(1,3:5)]
  
  ## Change name in output back
  setnames(pop,'pnrxTempName',pnr)
  
  return(list(data=pop))
}
