##' @export
print.dpp <- function(x,...) {
    treatments <- names(x$drugs)
    
    cat("Data preprocessing structure", "\n")
    cat("----------------------------", "\n")
    cat("\n")

  ready <- 1

  if (length(x$drugs) > 0) {
    drugs <- lapply(x$drugs, function(x) x$atc)
    cat("Registered the following treatment(s):", "\n", 
        paste(paste0(names(drugs), ":"),
              sapply(drugs, function(x) paste(x, collapse = ", ")), "\n"))
    cat("\n")

    doses <- lapply(x$drugs, function(x) x$doses)
    
    j <- (1:length(x$drugs))[unlist(lapply(doses, length)) == 0]
    
    if (any(unlist(lapply(doses, length)) > 0)) {
      cat("Dose values to be used in calculations:", "\n")
      print(do.call("rbind", lapply(doses, function(x) as.data.frame(x))))
      cat("\n")
    }
    if (length(j) > 0) {
      ready <- 0
      cat(paste("NOTE: Structure of packages for drug(s)", 
                paste(names(x$drugs)[j], collapse=", "), "are missing. Please use function 'pack' to specify."))
      cat("\n")
      cat("\n")
    } 
    
    N        <- lapply(x$drugs, function(x) x$N)
    period   <- lapply(x$drugs, function(x) x$period)
    maxdepot <- lapply(x$drugs, function(x) x$maxdepot)

    dat <- data.frame(treatments = treatments)
    
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
    if (length(x$drugdb) == 0) {
        ready <- 0
        cat("NOTE: Drug data still missing, add as follows:\ndrugdb(object) <- claim.data")
        cat("\n")
    }
    if (length(x$admdb) == 0) {
        ready <- 0
        cat("NOTE: Admission dates data still missing, add as follows:\nadmdb(object) <- hospital.data")
        cat("\n")
    }
    drugsdone <- treatments[dd <- match(treatments,names(x$processed),nomatch=0)]
    drugsnotdone <- treatments[dd==0]
    if (length(drugsdone)>0){
        cat("Processed drugs", "\n")
        cat("---------------", "\n")
        for (d in drugsdone){
            cat("\n",d,":\n",sep="")
            print(x$processed[[d]])
        }
    }
    if (ready==1 & length(drugsnotdone)>0) {
        cat("-----------------", "\n")
        cat("You many now use the function 'process<-' to estimate prescription lengths\nand average doses of drugs:\n",paste(drugsnotdone,collapse="\n"),"\n",sep="")
    }
} 
