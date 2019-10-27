findCondition2 <- function (data,ptid="pnr", vars, keep, conditions,exclusions=NULL, match = "contain", 
          condition.name = "X") 
{
  cond = NULL
  match <- match.arg(match, c("exact", "contains", 
                              "start","end"))
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
  condnames <- names(conditions)
  if (!is.null(exclusions)) exclnames <- names(exclusions)
  Ex <- FALSE # No exclusion list defaul
  # Conditions as regular expression
  for (i in 1:length(conditions)) {
    conditions[[i]] <- switch(match, 
                              exact    = paste0("^", conditions[[i]],"$"), 
                              contains = conditions[[i]], 
                              start    = paste0("^",conditions[[i]]),
                              end      = paste0(conditions[[i]],"$"))
  }
  if (!is.null(exclusions))
  for (i in 1:length(exclusions)){ #Exclusions as regular expressions
    exclusions[[i]] <- switch(match, 
                              exact    = paste0("^", exclusions[[i]],"$"), 
                              contains = exclusions[[i]], 
                              start    = paste0("^",exclusions[[i]]),
                              end      = paste0(exclusions[[i]],"$"))    
    Ex <- TRUE 
    exclnames <- names(exclusions)
  } 
  else{
    exclnames <- ""
  }
  browser() 
 #Matrices of conditions and exclusions  
 max.cond <- max(sapply(conditions,length))
 conditions <- lapply(conditions,function(x){c(x,rep("",max.cond-length(x)))})
 conditions <- do.call(rbind,conditions)
 if(!is.null(exclusions)){
   max.excl <- max(sapply(exclusions,length))
   exclusions <- lapply(exclusions,function(x){c(x,rep("",max.excl-length(x)))})
   exclusions <- do.call(rbind,exclusions)
 }
 else{
   exclusions <- as.matrix("")
   max.excl <- 0
 }
 
 # Columnes to search and keep
 searchCols <- as.matrix(data[,.SD,.SDcols=c(vars)])
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




