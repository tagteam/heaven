findCondition2 <- function (data,ptid="pnr", vars, keep, conditions,exclusions=NULL, match = "contain", 
          condition.name = "X") 
{
  cond = NULL
  if (match=="start") match.num <- 0L
   else if (match=="exact") match.num <- 1L
   else if (match=="end") match.num <- 2L
   else if (match=="contain") match.num <- 3L
   else stop("Choise of match not appropriate")
  if (!is.character(vars) | !is.character(keep)) 
    stop("Error -  vars or keep not character")
  if (!class(conditions) == "list" | is.null(names(conditions))) 
    stop("Error - Conditions not a named list")
  if(!is.null(exclusions) & (!class(conditions) == "list" | is.null(names(conditions))))
    stop("Error - Exclusions should be NULL or a named list") 
  requireNamespace("data.table")
  setDT(data)
  conditions <- copy(conditions)
  exclusions <- copy(exclusions)
  for (variable in unique(c(vars, keep))) {
    if (!variable %in% names(data)) 
      stop(paste0("Error - ", variable, " not in data to be analysed"))
  }
 #vector of conditions and exclusions  
 condnames <- names(conditions)  
 max.cond <- max(sapply(conditions,length))
 conditions <- lapply(conditions,function(x){c(x,rep("",max.cond-length(x)))}) # All max length, padded with ""
 condnames <- lapply(condnames,function(x){rep(x,max.cond)}) #condnames aligned with conditions
 conditions <- unlist(conditions)
 condnames <- unlist(condnames)

 if(!is.null(exclusions)){
   exclnames <- names(exclusions)
   max.excl <- max(sapply(exclusions,length))
   exclusions <- lapply(exclusions,function(x){c(x,rep("",max.excl-length(x)))})
   exclnames <- lapply(exclnames,function(x){rep(x,max.excl)}) #exclnames aligned with exclitions
   exclusions <- unlist(exclusions)
   exclnames <- unlist(exclnames)
 }
 else{
   exclusions <- ""
   exclnames <- ""
   max.excl <- 0
 }
 
 # Columnes to search and keep
  searchCols <- data[,.SD,.SDcols=c(vars)]
  
 pnrnum <- 1:dim(data)[1]
 searchCols <- as.matrix(searchCols)
 keepCols <- data[,.SD,.SDcols=c(ptid,keep)]
 keepCols[,pnrnum:=1:.N]
 #cpp
 out <- matrixSearch(pnrnum,searchCols,conditions,exclusions,condnames,exclnames,length(condnames),length(exclnames),
                     max.cond,max.excl,dim(data)[1])
 setDT(out)
 out <- merge(out,keepCols,by="pnrnum")
 out
}




