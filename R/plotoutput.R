##' Function to plot output etc
##' 
##' Function to plot output etc
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
plotoutput <- function(out, drug=NULL) {
  
  if (length(drug) == 0) 
    j <- 1
  else
    j <- 1:length(out)[names(out) == drug]
  
  drugout <- out[[j]]
  
  B   <- drugout$B
  nk  <- drugout$nk
  Sjk <- drugout$Sjk
  i0  <- drugout$i0
  DH  <- drugout$DH
  
  par(mar=c(3.1,3.1,3.1,3.1))
  
  plot(0,0,type="n",xlim=c(B[1],B[length(B)]+nk[length(B)]),ylim=c(0,120),xlab="Calendar time",ylab="", 
       yaxt='n', xaxt='n', axes=FALSE)
  axis(1, at=c(B, B[length(B)]+nk[length(B)]), labels=c(B, ""), las=0)
  
  ssegs <- function(a, b, pos, pos2=1, col="black", lwd=3, lty=1){
    segments(x0=a, x1=b, y0=pos, y1=pos, lwd=lwd, col=col, lty=lty)
    segments(a, a, y0=pos-pos2, y1=pos+pos2, lty=lty, lwd=lwd, col=col)
    segments(b, b, y0=pos-pos2, y1=pos+pos2, lty=lty, lwd=lwd, col=col)
  }
  
  Bnsegs <- function(i) {
    ssegs(B[i], B[i]+nk[i], 50+length(B)*4-i*4, col="blue", lty=5, lwd=1)
    ssegs(B[i]+nk[i], B[i]+nk[i]+DH[i], 50+length(B)*4-i*4, col="darkred", lty=5, lwd=1)
  }
  
  sapply(1:length(B), Bnsegs)
  
  axis(3,
       lwd=0.1,
       pos=80,
       at=c(B, B[length(B)]+nk[length(B)]),
       labels=c(sapply(1:length(B), function(i) eval(bquote(expression(B[.(i)])))), ""))
  
  sapply(B, function(x) segments(x, x, y0=0, y1=80, lty=2,lwd=0.5))
  
  Spoints <- function(i) {
    points(B[i], 50+length(B)*4-i*4, pch=21, cex=2*Sjk[i]/max(Sjk), bg="blue")
  }
  
  sapply(1:length(B), Spoints)
  
  Esegs <- function(i) {
    ssegs(B[i-i0[i]], B[i], 50+length(B)*4-length(B)*4-i*4, col="darkgreen", lty=5, lwd=1)
  }
  
  sapply(1:length(B), Esegs)
}
