##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param id a 
##' @param maxdepot a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
simdata <- function(which = 1, dos = c(75, 100, 50, 125)) {

  eksd = as.Date(Sys.Date())+sample(c(-1, 1), 100, replace = TRUE)*sample(2000, 100, replace = TRUE)
  
  
  if (which == 1) {
    
    outdata = data.frame(atc = as.character(rep(c("a","b1"), each = 50)), 
                             alph = rep(letters[1:10], length = 100), 
                             eksd = eksd, 
                             pnr = sample(c(2000,1000,3000,4000,5000,6000,7000,8000,9000), 100, replace = TRUE), 
                             atc = rnorm(100), 
                             strnum = sample(dos, 100, replace = TRUE), 
                             apk = sample(3, 100, replace = TRUE), 
                             packsize = sample(c(5, 9, 10, 18, 20), 100, replace = TRUE),
                             stringsAsFactors=FALSE)
    
    outdata = outdata[order(outdata$pnr), ]
  
  } else {
    
    inddto = sort(sample(eksd, 8*4, replace = TRUE))
    
    inddto1 = inddto[1:8]
    inddto2 = inddto[9:16]
    inddto3 = inddto[17:24]
    inddto4 = inddto[25:32]
  
    outdata = data.frame(pnr = unique(sample(c(2000,1000,3000,4000,5000,6000,7000,8000,9000), length(inddto1))), 
                        max_indl = sample(4, 8, replace = TRUE), 
                        inddto1 = inddto1, 
                        uddto1  = as.Date(apply(cbind(inddto1 + sample(15, 8, replace = TRUE), inddto2-1), 1, min),
                                          origin = "1970-01-01"), 
                        inddto2 = inddto2,  
                        uddto2  = as.Date(apply(cbind(inddto2 + sample(15, 8, replace = TRUE), inddto3-1), 1, min), 
                                          origin = "1970-01-01"),   
                        inddto3 = inddto3,  
                        uddto3  = as.Date(apply(cbind(inddto3 + sample(15, 8, replace = TRUE), inddto4-1), 1, min), 
                                          origin = "1970-01-01"), 
                        inddto4 = inddto4, 
                        uddto4  = as.Date(inddto4 + sample(15, 8, replace = TRUE), origin = "1970-01-01"))
    
    outdata = outdata[order(outdata$pnr), ]
    outdata0 = outdata
    
    library(reshape2)
    
    outdata1 = melt(outdata[, 1:ncol(outdata) %% 2 == 1], id.vars = "pnr")
    outdata2 = melt(outdata[, c(0, 1, 3:ncol(outdata) %% 2) == 0], id.vars = "pnr")
    outdata1 = outdata1[order(outdata1$pnr), c(1, 3)]
    outdata2 = outdata2[order(outdata2$pnr), c(1, 3)]
    
    names(outdata1) = c("pnr", "inddto")
    names(outdata2) = c("pnr", "uddto")
    
    outdata = cbind(outdata1, uddto = outdata2[, 2])
    
  }
  
  return(outdata)
  
}


