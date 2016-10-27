par(mar=c(5.1,3.1,3.1,7.1))

plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="Calendar time",ylab="", 
     yaxt='n', xaxt='n', axes=FALSE)

vt <- c(15, 50, 80)

axis(1, at=vt, labels=c(expression(T[k-1]), expression(T[k]), expression(T[k+1])))
axis(1, at=seq(0, 100, by = 5), labels=rep(NA, 21))

vtype <- 100-c(2.5, 5+2.5, 5+5+2.5, 5+5+2.5+2.5)/18*100

axis(4, at=vtype, labels=c("(I)", "(II)", "(IIIa)", "(IIIb)"),
     las=2, cex.axis=1.1, tck=0.0, lwd=0)

abline(h = 100-5/18*100)
abline(h = 100-10/18*100)

abline(v = vt[1], lty=2)
abline(v = vt[2], lty=2)
abline(v = vt[3], lty=2)

points(vt[1], vtype[1], pch=15, cex=1.3, bg="black")
points(vt[1], vtype[2], pch=17, cex=1.3, bg="black")
points(vt[1], vtype[3], pch=15, cex=1.3, bg="black") 
points(vt[1], vtype[4], pch=19, cex=1.3, bg="black")

points(vt[2], vtype[1], pch=15, cex=1.3, bg="black")
points(vt[2], vtype[2], pch=17, cex=1.3, bg="black")
points(vt[2], vtype[3], pch=15, cex=1.3, bg="black")
points(vt[2], vtype[4], pch=17, cex=1.3, bg="black")

points(vt[3], vtype[3], pch=15, cex=1.3, bg="black")

t12I    <- (vt[2]-vt[1])/1.25+vt[1]
t23I    <- (vt[3]-vt[2])/1.1+vt[2]
t12IIIa <- (vt[2]-vt[1])/1.5+vt[1]

segments(x0=t12I,x1=t12I,y0=vtype[1]-1,y1=vtype[1]+1,lwd=1)
segments(x0=t23I,x1=t23I,y0=vtype[1]-1,y1=vtype[1]+1,lwd=1)
segments(x0=t12IIIa,x1=t12IIIa,y0=vtype[3]-1,y1=vtype[3]+1,lwd=1)

segments(x0=vt[1],x1=t12I,y0=vtype[1],y1=vtype[1],lwd=1)
segments(x0=vt[2],x1=t23I,y0=vtype[1],y1=vtype[1],lwd=1)

segments(x0=vt[1],x1=vt[2],y0=vtype[2],y1=vtype[2],lwd=1)

segments(x0=vt[1],x1=t12IIIa,y0=vtype[3],y1=vtype[3],lwd=1)
segments(x0=vt[2],x1=vt[3],y0=vtype[3],y1=vtype[3],lwd=1)

segments(x0=vt[1],x1=vt[2],y0=vtype[4],y1=vtype[4],lwd=1)

legend("topleft", c(expression(S[1]), expression(S[2]), bquote(S[1]~"or"~S[2])),
       pch=c(19, 17, 15),# bty = "n"
       bg="gray90")



