#' @title Hypertension at baseline
#'
#' @param data Data set with drugs indicated by atc codes and date of drugs, e.g. lmdb 
#' @param pnr Variable with ID for each subject/group. Default name: pnr 
#' @param atc Variable with atc codes. Must be type character. Default name: atc 
#' @param eksd Variable with dates. Must be type Date or numeric. Default name: eksd
#' @param baseline Variable with dates. Must be type Date or numeric. Default name: baseline
#'
#' @return List containing two datasets. First contains data with hypertension information. 
#' The second contains all rows with missing data that were removed before hypertension where calculated.
#' The variable hypertension at baseline is one if the person has received two or more types of anti-hypertensive
#' medications within 180 days before the baseline date (and 7 days after due to data errors). 
#' The codes for hypertension are currently defined in the function.
#'
#' @export
#'
#' @examples
#' #Required library
#' library(data.table)
#' 
#' #Load data
#' \dontrun{
#' data(Hypertension) 
#' ht<-hypertensionBaseline(data=Hypertension,pnr='myID',atc='myAtc',eksd='myEksd',baseline='myBaseline')
#' 
#' # A list is returned which contains two data.tables
#' htData  <- ht[[1]]
#' htError <- ht[[2]]
#' }
#' @author Helle Hoejmark Eriksen <helle.e@@rn.dk>
hypertensionBaseline<- function(data,
                                pnr='pnr',
                                atc='atc',
                                eksd='eksd',
                                baseline='baseline'){
    
    options(warn=1)

    ## Make into data.table 
    out <- as.data.table(data)
    
    ## A different name for the variables: xTempName
    setnames(out,pnr,'pnrxTempName')
    setnames(out,atc,'atcxTempName')
    setnames(out,eksd,'eksdxTempName')
    setnames(out,baseline,'baselinexTempName')
    
    ## Check variable types 
    if( !( class(out[,eksdxTempName])=="Date" | is.numeric(out[,eksdxTempName]) ) ){stop("eksd must be numeric or Date")} 
    if( !( class(out[,baselinexTempName])=="Date" | is.numeric(out[,baselinexTempName]) ) ){stop("baseline must be numeric or Date")} 
    if( !is.character(out[,atcxTempName]) ){stop("atc must be character")} #else grep won't work
    
    ## Removes invalid dates and atc codes
    #baseline
    outerror <- copy(out[is.na(baselinexTempName)])
    out <- out[!is.na(baselinexTempName)]
    if( dim(outerror)[1]!=0 ){message("Some baseline are missing and have been removed")} 
    #eksd
    outerrorTemp <- rbind(outerror,out[is.na(eksdxTempName)])
    out <- out[!is.na(eksdxTempName)]
    if( dim(outerrorTemp)[1]!=dim(outerror)[1] ){message("Some eksd are missing and have been removed")}
    #atc
    outerror=rbind(outerrorTemp, out[atcxTempName=='',] ) #should we output this also?
    out <- out[atcxTempName!='']
    if( dim(outerror)[1]!=dim(outerrorTemp)[1] ){message("Some atc are missing and have been removed")}

    
    #All anti-hypertensive medication
    setkey(out,pnrxTempName)
    d <- out[unlist(lapply("^C0",grep,atcxTempName)),c("pnrxTempName","eksdxTempName","atcxTempName","baselinexTempName")]
    #Id
    out <- out[.(unique(pnrxTempName)),c("pnrxTempName"),mult="first"]
    
    #Only prescriptions around inclusion
    d <- d[-7<=baselinexTempName-eksdxTempName & baselinexTempName-eksdxTempName<=180]
    #Defining different treatment regims
    antiA4=c('C02A','C02B','C02C') #antiAdrenerg
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
    
    #Drug categorization (1 if true otherwise 0)
    d$AntiA <-as.numeric(substr(d$atcxTempName,1,4) %in% antiA4)
    d$diu   <-as.numeric(substr(d$atcxTempName,1,5) %in% diu5 | substr(d$atcxTempName,1,4) %in% diu4 | substr(d$atcxTempName,1,7) %in% diu7) 
    d$Andet <-as.numeric(substr(d$atcxTempName,1,4) %in% Andet4)
    d$Vaso  <-as.numeric(substr(d$atcxTempName,1,5) %in% Vaso5)
    d$bb    <-as.numeric(substr(d$atcxTempName,1,4) %in% bb4)
    d$ccb   <-as.numeric(substr(d$atcxTempName,1,4) %in% ccb4 | substr(d$atcxTempName,1,3) %in% ccb3 | substr(d$atcxTempName,1,5) %in% ccb5)
    d$ras   <-as.numeric(substr(d$atcxTempName,1,5) %in% ras5 | substr(d$atcxTempName,1,7) %in% ras7)
    
    # Retain 1 to the last observation
    d[,c("AntiA","diu","Andet","Vaso","bb","ccb","ras"):=list(cummax(AntiA),cummax(diu),cummax(Andet),cummax(Vaso),cummax(bb),cummax(ccb),cummax(ras)),by=pnrxTempName]
    # Last observation containing cummax
    setkey(d,pnrxTempName)
    d <- d[.(unique(pnrxTempName)),,mult="last"]
    
    #Number of different drugs
    d[,antal_drugs:=AntiA+diu+Andet+Vaso+bb+ccb+ras] 
    #Hypertension if more than one anti-hypertensive drug
    d[,HT:=as.numeric(2<=antal_drugs)] 
    #Type of drugs
    d$type1<-ifelse(d$AntiA==1,"AntiAdrenerg","")
    d$type2<-ifelse(d$diu==1,"diu","")
    d$type3<-ifelse(d$Andet==1,"Andet","")
    d$type4<-ifelse(d$Vaso==1,"Vaso","")
    d$type5<-ifelse(d$bb==1,"bb","")
    d$type6<-ifelse(d$ccb==1,"ccb","")
    d$type7<-ifelse(d$ras==1,"ras","")
    d[,type:=gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", paste(type1,type2,type3,type4,type5,type6,type7, sep=" "),perl=TRUE)]
    #drop count
    d<-d[, c("pnrxTempName","antal_drugs","type","HT")]
    #Merge with full population
    out<-merge(d, out, by="pnrxTempName", all=TRUE)
    out[is.na(antal_drugs),c("antal_drugs","type","HT"):=list(0,"",0)]
    
    ## Change name in output back 
    setnames(out,'pnrxTempName',pnr)
    setnames(outerror,'pnrxTempName',pnr)
    setnames(outerror,'atcxTempName',atc)
    setnames(outerror,'eksdxTempName',eksd)
    setnames(outerror,'baselinexTempName',baseline)
    
    outList <- list("HypBase"=out,"RowErrors"=outerror)   
    
    outList
}
