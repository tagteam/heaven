#' @title lexisSeq
#' 
#' @description 
#' lexisSeq is used to split by a sequence of dates - typical use is a sequence of calender dates (time periods) or
#' a sequence of times after birth to split by age in a time dependent manner. Splitting times is provided by "splitvector"
#' and "varname".  If all subjects are to be split by the same times "varname" should be "NULL".  If "varname" is a number
#' it is added to the vector.  A typical use for this adding is to split by age. The vector identifies a series of times after birth and
#' "varname" is the date of birth.
#' 
#' The vector can be provided as a series of fixed times with format="vector".  It can also be provided as from-to-by, three
#' values with format="seq".  In that case a vector is defined with all values between "from" and "to" by each interval of "by".
#' 
#' The variable with default name "value" takes a value of zero to the left of the first interval and increased by 1 as each 
#' element in the vector is passed.
#' 
#' Overall the function provides identical usefulness as the SAS lexis macro
#' @usage
#' lexisSeq(indat,invars,varname=NULL,splitvector,format,value="value")
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end, event and other data - possibly already split
#' @param invars - vector of colum names for id/entry/exit/event - in that order, 
#' example: c("id","start","end","event")
#' @param varname - name of variable to be added to vector
#' @param splitvector - A vector of calender times (integer). Splitvector can be a sequence of fixed times
#' with format="vector" or generate a sequence of from-to-by if given 3 values and format="seq"
#' @param format - either "vector" for fixed times or "seq" to generate a sequence of from-to-by
#' @param value - 0 to the left of the vector, increase of 1 as each element of vector is passed
#' @return
#' The function returns a new data table where records have been split according to the provided vector. Variables
#' unrelated to the splitting are left unchanged.
#' @export
#' @details 
#' The input must be data.table.  This data.table is assumed already to be split by other functions with multiple
#' records having identical participant id. The function extracts thos variables necessary for splitting, splits
#' by the provided vector and finally merges other variable onto the final result.
#' @seealso lexis2 lexisFromTo 
#' @examples
#' library(data.table)
#' dat <- data.table(ptid=c("A","A","B","B","C","C","D","D"),
#'                 start=c(0,100,0,100,0,100,0,100),
#'                 end=c(100,200,100,200,100,200,100,200),
#'                 dead=c(0,1,0,0,0,1,0,1),
#'                 Bdate=c(-5000,-5000,-2000,-2000,0,0,100,100))
#' out <- lexisSeq(indat=dat,invars=c("ptid","start","end","dead"),varname="Bdate",c(0,1000,5000),format="vector")
#' out[]
#' out2 <- lexisSeq(indat=dat,invars=c("ptid","start","end","dead"),varname=NULL,c(0,200,50),format="seq")
#' out2[]
#' @export
lexisSeq <- function(indat # inddata with id/in/out/event - and possibly other variables
                         ,invars #names of id/in/out/event - in that order
                         ,varname=NULL # Name of var to ad to splitvector
                         ,splitvector #Names var date-vars to split by
                         ,format # "seq" for loop (3 values) and "vector" for list of values
                         ,value="value" #Name of output variable holding sequence number
                        ){
  #Tests of data
  if (!is.data.table(indat)) stop("Input not data tables")
  if (class(invars) != "character") stop("Varnames in c(..) not character")
  if (class(varname) != "character" & !is.null(varname)) stop("varname not character or NULL")
  # Get necessary data for split - and prepare final merge with pnrnum
  datt <- copy(indat)
  if (is.null(varname)) datt[,varname:=0]
   else {
     datt[,newvar:=.SD,.SDcols=varname]
     setnames(datt,varname,"varname")
   } 
  datt[,pnrnum:=1:.N]
  
  splitdat <- datt[,.SD,.SDcols=c("pnrnum",invars[2:4],"varname")]
  setnames(splitdat,c("pnrnum","inn","out","event","varname"))
  # Prepare to merge with residual columns
  datt[,(invars[2:4]):=NULL]
  # Check vector
  if (!(format %in% c("vector","seq"))) stop("format must be 'seq' or 'vector'")
  #Create dataset/matrix with ptid and vector
  if (format=="seq"){ ## start stop by
      if ((length(splitvector)!=3) || (splitvector[1]>=splitvector[2]) || (splitvector[3]>=(splitvector[2]-splitvector[1])))
        stop("Argument 'seq' must be a vector of the form (start, stop, by) where start < stop and by < stop-start.")
      splitguide <- splitdat[,{splitvector1=.SD[[1]][1];.(seq(from=splitvector1+splitvector[1],
                    to=splitvector1+splitvector[2],by=splitvector[3]))},.SDcol="varname",by=pnrnum]
      vectorLength <- length(seq(from=splitvector[1], to=splitvector[2],by=splitvector[3]))
    } else{ ## simple vector
      splitguide <- splitdat[,.(.SD[[1]][1]+splitvector),.SDcol="varname",by=pnrnum]
      vectorLength <- length(splitvector)
    }
  setnames(splitguide,"V1","varname")
  splitdat[,varname:=NULL]
  splitdat[,value:=0] # counts sequentially from zero as data is split by vector
  for (i in 1:vectorLength){
    subsplit <- splitguide[,.SD[i],by=pnrnum]
    splitdat <- merge(splitdat,subsplit,by="pnrnum")
    splitdat <- splitdat[,.Call('_heaven_splitDate',PACKAGE = 'heaven',inn, out, event, pnrnum,value, varname)] # call c++ function
    setDT(splitdat)
  }
 setkeyv(splitdat,c("pnrnum","inn")) 
 if (is.null(varname)) datt[,varname:=NULL]
 else {
   setnames(datt,"newvar",varname)
   datt[,varname:=NULL]
 } 
 splitdat <- merge(splitdat,datt,by="pnrnum",all=TRUE) 
 splitdat[,pnrnum:=NULL]
 setnames(splitdat,c("inn","out","event","value"),c(invars[2:4],value))
 splitdat
}