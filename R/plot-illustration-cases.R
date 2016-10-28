par(mar=c(3.1,3.1,3.1,3.1))

plot(0,0,type="n",xlim=c(10,100),ylim=c(0,100),xlab="Calendar time",ylab="", 
     yaxt='n', xaxt='n', axes=FALSE)

vt <- c(35, 80)

axis(1, at=vt, labels=c(expression(T[k-1]), expression(T[k])))
axis(1, at=seq(0, 100, by = 5), labels=rep(NA, 21))

vtype <- 100-seq(0, 100, length = 8)[c(2, 3, 5, 7)]

axis(4, at=vtype, labels=c("(Ia)", "(Ib)", "(II)", "(III)"),
     las=2, cex.axis=1.1, tck=0.0, lwd=0)

abline(v = vt[1], lty=2)
abline(v = vt[2], lty=2)

##--- case Ia
points(vt[1], vtype[1], pch=19, cex=1.3, bg="black")
points(vt[2], vtype[1], pch=19, cex=2.3, bg="black")
t12I    <- (vt[2]-vt[1])/1.25+vt[1]
#t23I    <- (vt[3]-vt[2])/1.1+vt[2]
segments(x0=t12I,x1=t12I,y0=vtype[1]-1,y1=vtype[1]+1,lwd=1)
#segments(x0=t23I,x1=t23I,y0=vtype[1]-1,y1=vtype[1]+1,lwd=1)
segments(x0=vt[1],x1=t12I,y0=vtype[1],y1=vtype[1],lwd=1)
#segments(x0=vt[2],x1=t23I,y0=vtype[1],y1=vtype[1],lwd=1)

##--- case Ib
points(vt[1], vtype[2], pch=19, cex=1.3, bg="black")
points(vt[2], vtype[2], pch=19, cex=1.3, bg="black")
segments(x0=t12I,x1=t12I,y0=vtype[2]-1,y1=vtype[2]+1,lwd=1)
#segments(x0=t23I,x1=t23I,y0=vtype[2]-1,y1=vtype[2]+1,lwd=1)
segments(x0=vt[1],x1=t12I,y0=vtype[2],y1=vtype[2],lwd=1)
#segments(x0=vt[2],x1=t23I,y0=vtype[2],y1=vtype[2],lwd=1)

##--- case IIa
points(vt[1], vtype[3], pch=19, cex=1.3, bg="black")
points(vt[2], vtype[3], pch=19, cex=1.3, bg="black") 
segments(x0=vt[1],x1=vt[2],y0=vtype[3],y1=vtype[3],lwd=1)
#segments(x0=vt[2],x1=vt[3],y0=vtype[3],y1=vtype[3],lwd=1)
#segments(x0=vt[3],x1=vt[3],y0=vtype[3]-1,y1=vtype[3]+1,lwd=1)

##--- case IIb
#points(vt[1], vtype[4], pch=19, cex=1.3, bg="black")
#points(vt[2], vtype[4], pch=19, cex=1.3, bg="black") 
#segments(x0=vt[1],x1=vt[2],y0=vtype[4],y1=vtype[4],lwd=1)
#t23IIb    <- (vt[3]-vt[2])/1.7+vt[2]
#segments(x0=t23IIb,x1=t23IIb,y0=vtype[4]-1,y1=vtype[4]+1,lwd=1)
#segments(x0=vt[2],x1=t23IIb,y0=vtype[4],y1=vtype[4],lwd=1)

##--- case IIIa
points(vt[1], vtype[4], pch=19, cex=1.3, bg="black")
points(vt[2], vtype[4], pch=19, cex=2.3, bg="black")
segments(x0=vt[1],x1=vt[2],y0=vtype[4],y1=vtype[4],lwd=1)
#segments(x0=vt[2],x1=vt[3],y0=vtype[5],y1=vtype[5],lwd=1)
#segments(x0=vt[3],x1=vt[3],y0=vtype[5]-1,y1=vtype[5]+1,lwd=1)

##--- case IIIb
#t23IIIb <- (vt[3]-vt[2])/1.5+vt[2]
#points(vt[1], vtype[6], pch=19, cex=1.3, bg="black")
#points(vt[2], vtype[6], pch=19, cex=2.3, bg="black")
#segments(x0=vt[1],x1=vt[2],y0=vtype[6],y1=vtype[6],lwd=1)
#segments(x0=vt[2],x1=t23IIIb,y0=vtype[6],y1=vtype[6],lwd=1)
#segments(x0=t23IIIb,x1=t23IIIb,y0=vtype[6]-1,y1=vtype[6]+1,lwd=1)


