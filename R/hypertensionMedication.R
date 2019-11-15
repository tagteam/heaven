#' @title hypertensionMedication - define presence of hypertension based on 
#' prescriptions
#' @description
#' This function attempts to define presence of hypertension based on the use
#' of at least two drugs for hypertension. The basis for the function is a
#' validation study performed in relation to Olesen et al. (BMJ 2011;342:d124)
#' where the sensitivity was 80.0%, and the specificity was 94.7% sompared to a
#' defined index date and using medication 180 days before that date.  Not 
#' directly validated, this function can also calculate the date of start of 
#' hypertension as a date where an individual has picked up at least two classes
#' of antihypertensive medication for two consecutive periods of 3 months.
#' @usage hypertensionMedication(data, vars=c("ptid","ATCcode",
#'  "prescription.date"), index.date=NULL,medication.definition=hypertensionATC)
#' @author Christian Torp-Pedersen 
#' @param data - A data frame/table containing prescription data
#' @param vars - names of variables containing person identification, ATC code
#'    and date of prescription, and in that order
#' @param index.date - name of a variable in data containing a date where the 
#'    question is whether hypertension was present prior to this date. This 
#'    will usually require merging of that variable to the medication data.
#' @param medication.definition - a named list of character vectors defining
#'    the first part of the ATC codes for classes of antihypertensive medication.
#'    The default version is shown in details.
#' @details
#' This function uses a list of named character vectors to define classes of 
#' antihypertensive medication.  The default versionis
#' \itemize{
#' \item{AntiAdrenerg:  }{'C02A','C02B','C02C'}
#' \item{Diuretic:  }{'C02DA','C03A','C03B','C03D','C03E','C03X','C02DA','C07B','C07C','C07D','C08G','C09BA','C09DA','C09XA52','C02L'}
#' \item{Vasodilator:  }{'C02DB','C02DD','C02DG'}
#' \item{BetaBlocker:  }{'C07A','C07B','C07C','C07D','C07F'}
#' \item{CalciumAntagonist:  }{'C07FB','C08','C08G','C09BB','C09DB'}
#' \item{ReninAngiotensinInhibitor;  }{'C09AA','C09BA','C09BB','C09CA','C09DA','C09DB','C09XA02','C09XA52'}
#' }
#' @return A data.table with the following variables
#' \itemize{
#' \item{ptid: }{The variable from input defining the individual}
#' \item{hypertension: }{if an index variable is presen then it is a 0/1 variable
#' as to wheter hypertension is present. If there is no index.date it is the
#' date of hypertension.}
#' \item{numDrugs: }{Number of drugs at time of hypertension or index.date}
#' }
#' @seealso findCondition
#' @examples
#' require(data.table)
#' dat <-data.table(
#'  ATC=sample(c('C02A','C03A','C02DD','C07A','C07F','C09AA','C09BA'),100,
#'             replace=TRUE),
#'  EKSD=as.Date("2012-01-01")+(sample(1:600,100,replace=TRUE)),
#'  ptid=1:10,
#'  index=as.Date("2013-06-01")+seq(80,98,2))
#' setkey(dat,"ptid")  
#' # Hypertension with indexdate:
#' index <- hypertensionMedication(dat,vars=c("ptid","ATC","EKSD"),
#'  index.date="index")
#' index
#' noindex <-  hypertensionMedication(dat,vars=c("ptid","ATC","EKSD"))
#' noindex
#' @export
hypertensionMedication <- function(data, vars=c("ptid","ATCcode",
      "prescription.date"), index.date=NULL,medication.definition=hypertensionATC)
{
  index=.SD=.N=hypertension=numDrugs=newdate=olddate=NULL
  datt <- copy(data)
  setDT(datt)
  for (x in vars) if(!x %in% names(datt)) stop("variables not in data")
  if (is.null(medication.definition) & !exists("hypertensionATC")) hypertensionATC <- hypertensionATC
  if (!is.null(medication.definition)) hypertensionATC <- medication.definition
  if (!is.list(hypertensionATC)) stop ("Hypertension drugs not a list")
  setnames(datt,vars,c("pt","atc","date")) 
  if (!is.null(index.date)){
    setnames(datt,index.date,"index")
    out <- findCondition(datt,ptid="pt","atc",c("pt","date","index"),hypertensionATC,match="start")
    out <- out[date>=index-180 & date<=index] # Last 6 months
    setkeyv(out,c("pt","X"))
    out <- out[,.SD[1],by=c("pt","X")]
    out <- out[,list(numDrugs=.N),by="pt"]
    out[,hypertension:=as.integer(numDrugs>=2)]
  }
  else {
    out <- findCondition(datt,ptid="pt","atc",c("pt","date"),hypertensionATC,match="start")
    out[,newdate:=zoo::as.Date(zoo::as.yearqtr(date), frac = 1)] # last date of quater
    out[,olddate:=zoo::as.Date(zoo::as.yearqtr(newdate-100,format="%Y-%m-%d"), frac = 1)] # last date of quater before
    setkeyv(out,c("pt","newdate","X"))
    out <- out[,.SD[1], by=c("pt","newdate","X")] # one of each medication class per quater
    out <- out[,list(numDrugs=.N),by=c("pt","olddate","newdate")] # Number of drugs in each quater
    out <- out[numDrugs>=2] # Only keep quaters with 2 drugs
    out <- out[shift(newdate)==olddate] # Keep only when 2 quaters with 2 drugs are consecutive
    out <- out[,.SD[1],.SDcols=c("newdate","numDrugs"),by="pt"]
    setnames(out,"newdate","hypertension")
  }
  setnames(out,"pt",vars[1])
  out[]
}
  
  
