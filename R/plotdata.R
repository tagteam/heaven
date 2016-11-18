##' Function to plot dates etc
##' 
##' Function to plot dates etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plot.dpp <- function(dpp, drug=NULL) {
  
  if (length(drug) == 0) 
    j <- 1
  else
    j <- 1:length(dpp$drugs)[names(dpp$drugs) == drug]
  
  drugdb <- dpp$drugdb[dpp$drugdb$atc %in% d$drugs[[j]]$atc,]
  
  T  <- unique(drugdb$pdate)
  T <- sort(T)
  
  LR <- lapply(1:nrow(dpp$admdb[dpp$admdb$inddto <= T[length(T)], ]), function(i) c(dpp$admdb$inddto[i], dpp$admdb$uddto[i]))
  
  par(mar=c(3.1,3.1,3.1,3.1))
  
  plot(0,0,type="n",xlim=c(T[1],T[length(T)]),ylim=c(0,120),xlab="Calendar time",ylab="", 
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
  
  sapply(1:(length(T)-1), Tsegs)
  sapply(LR, LRsegs)
  
  
  axis(3,
       lwd=0.1,
       pos=80,
       at=c(T, unlist(LR)),
       labels=c(sapply(1:length(T), function(i) eval(bquote(expression(T[.(i)])))), 
                sapply(1:length(LR), function(i) c(eval(bquote(expression(L[.(i)]))),
                                                   eval(bquote(expression(R[.(i)])))))))
  
  sapply(T, function(x) segments(x, x, y0=0, y1=80, lty=2,lwd=0.5))
  sapply(unlist(LR), function(x) segments(x, x, y0=0, y1=80, col="red", lty=2,lwd=0.5))
} 