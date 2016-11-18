##' Function to plot dates etc
##' 
##' Function to plot dates etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plot.dpp <- function(dpp, drug=NULL, id=1) {
  
  if (length(drug) == 0) 
    j <- 1
  else
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]

  drugdb <- data.frame(dpp$drugdb)
  drugdb <- drugdb[drugdb$atc %in% dpp$drugs[[j]]$atc & drugdb$id == id, ]
  admdb  <- data.frame(dpp$admdb)
  admdb  <- admdb[admdb$id == id, ]
  
  T  <- unique(drugdb$pdate)
  T <- sort(T)
  
  nT <- length(T)
  
  if (nT > 1) {
    nadm <- nrow(admdb[admdb$inddto <= T[nT], ])
    
    if (nadm > 0)
      LR <- lapply(1:nadm, function(i) c(admdb$inddto[i], admdb$uddto[i]))
    
    par(mar=c(3.1,3.1,3.1,3.1))
    
    plot(0,0,type="n",xlim=c(T[1],T[nT]),ylim=c(0,120),xlab="Calendar time",ylab="", 
         yaxt='n', xaxt='n', axes=FALSE)
    axis(1, at=T, labels=T, las=0)
    
    ssegs <- function(a, b, pos, pos2=1, col="black", lwd=3, lty=1){
      segments(x0=a, x1=b, y0=pos, y1=pos, lwd=lwd, col=col, lty=lty)
      segments(a, a, y0=pos-pos2, y1=pos+pos2, lty=lty, lwd=lwd, col=col)
      segments(b, b, y0=pos-pos2, y1=pos+pos2, lty=lty, lwd=lwd, col=col)
    }
    
    LRsegs <- function(vLR) {
      ssegs(vLR[1], vLR[2], 50, col="red", lty=1, lwd=2)
    }
    
    Tsegs <- function(i) {
      ssegs(T[i], T[i+1], 50, col="black", lty=1, lwd=2)
    }
    
    sapply(1:(nT-1), Tsegs)
    if (nadm > 0)
      sapply(LR, LRsegs)
    
    if (nadm > 0) {
      atvec    <- c(T, unlist(LR))
      labelvec <- c(sapply(1:nT, function(i) eval(bquote(expression(T[.(i)])))), 
                    sapply(1:length(LR), function(i) c(eval(bquote(expression(L[.(i)]))),
                                                       eval(bquote(expression(R[.(i)]))))))
    } else {
      atvec    <- T
      labelvec <- sapply(1:nT, function(i) eval(bquote(expression(T[.(i)]))))
    }
    
    axis(3,
         lwd=0.1,
         pos=80,
         at=atvec,
         labels=labelvec)
    
    sapply(T, function(x) segments(x, x, y0=0, y1=80, lty=2,lwd=0.5))
    if (nadm > 0)
      sapply(unlist(LR), function(x) segments(x, x, y0=0, y1=80, col="red", lty=2,lwd=0.5))
  } else 
    print("Only one date - no plot produced")
} 