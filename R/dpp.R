##' Create data preprocessing object 
##'
##' Create data preprocessing object (dpp)
##' @title Create data preprocessing object (dpp)
##' @author Helene Charlotte Rytgaard
##' @export
dpp <- function() { 
  out = structure(list(drugdb   = NULL,
                       admdb    = NULL, 
                       drugs    = NULL#, 
                       #period   = c(1, 1e10),
                       #N        = 2, 
                       #maxdepot = 10
                       ),
                  class = "dpp")
  out
}

##' @export
print.dpp <- function(dpp) {
  cat("preprocessing object", "\n")
  cat("-----------------", "\n")
  cat("\n")
  
  ready <- 1
  
  if (length(dpp$drugs) > 0) {
    
    drugs <- lapply(dpp$drugs, function(x) x$atc)
    cat("Registered the following treatment(s):", "\n", 
        paste(paste0(names(drugs), ":"),
              sapply(drugs, function(x) paste(x, collapse = ", ")), "\n"))
    cat("\n")

    doses <- lapply(dpp$drugs, function(x) x$doses)
    
    j <- (1:length(dpp$drugs))[unlist(lapply(doses, length)) == 0]
    
    if (any(unlist(lapply(doses, length)) > 0)) {
      cat("Dose values to be used in calculations:", "\n")
      print(do.call("rbind", lapply(doses, function(x) as.data.frame(x))))
      cat("\n")
    }
    if (length(j) > 0) {
      ready <- 0
      cat(paste("NOTE: Structure of packages for drug(s)", 
                paste(names(dpp$drugs)[j], collapse=", "), "are missing. Please use function 'pack' to specify."))
      cat("\n")
      cat("\n")
    } 
    
    N        <- lapply(dpp$drugs, function(x) x$N)
    period   <- lapply(dpp$drugs, function(x) x$period)
    maxdepot <- lapply(dpp$drugs, function(x) x$maxdepot)
    
    dat <- data.frame(treatments = names(dpp$drugs))
    
    if (sum(unlist(lapply(N, length))) > 0) {
      dat1 <- do.call("rbind", lapply(N, as.data.frame))
      dat1$treatments <- row.names(dat1)
      dat  <- merge(dat, dat1, by = "treatments", all=TRUE)
      colnames(dat)[length(colnames(dat))] <- "N"
    } else {
      cat('NOTE: Use "pwindow<-" to specify N')
      cat("\n")
    }
    if (sum(unlist(lapply(maxdepot, length))) > 0) {
      dat1 <- do.call("rbind", lapply(maxdepot, as.data.frame))
      dat1$treatments <- row.names(dat1)
      dat  <- merge(dat, dat1, by = "treatments", all=TRUE)
      colnames(dat)[length(colnames(dat))] <- "maxdepot"
    } else {
      cat('NOTE: Use "maxdepot<-" to specify maximum depot from one prescription to the next')
      cat("\n")
    } 
    if (sum(unlist(lapply(period, length))) > 0) {
      dat1 <- cbind(do.call("rbind", lapply(period, function(x) { 
        out <- t(as.data.frame(x))
        if (length(out) == 0)
          out <- c(NA, NA)
        return(out)
        })), treatments = names(period))
      dat  <- merge(dat, dat1, by = "treatments", all=TRUE)
      colnames(dat)[c(length(colnames(dat))-1, length(colnames(dat)))] <- paste0("period_", c("start", "end"))
    } else {
      cat('NOTE: Use "period<-" to specify relevant period of time')
      cat("\n")
    }

    if (dim(dat)[2] > 1) {
      row.names(dat) <- dat[, 1]
      cat("\n")
      print(dat[, names(dat) != "treatments", drop=FALSE])
      cat("\n")
    }
  } else {
    ready <- 0
    cat("NOTE: No treatments have been specified yet")
    cat("\n")
  }
  if (length(dpp$drugdb) == 0) {
    ready <- 0
    cat("NOTE: Drug data still needs to be added to the object")
    cat("\n")
  }
  if (length(dpp$admdb) == 0) {
    ready <- 0
    cat("NOTE: Admission dates data still needs to be added to the object")
    cat("\n")
  }  

  cat("\n")
  
  if (ready) {
    cat("-----------------", "\n")
    cat('Use function "process" to estimate prescription lengths and average doses\n')
  }
  
} 
