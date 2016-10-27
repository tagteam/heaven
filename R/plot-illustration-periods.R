par(mar=c(5.1,3.1,3.1,7.1))

plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="Calendar time",ylab="", 
     yaxt='n', xaxt='n', axes=FALSE)
#title(main="Case II")

set.seed(9)
vt <- sort(round(sample(100, 5)/5)*5)

axis(1, at=vt, labels=c(expression(T[k-4]), expression(T[k-3]), expression(T[k-2]), expression(T[k-1]), expression(T[k])))
axis(1, at=seq(0, 100, by = 5), labels=rep(NA, 21))

abline(v = vt[1], lty=2)
abline(v = vt[2], lty=2)
abline(v = vt[3], lty=2)
abline(v = vt[4], lty=2)
abline(v = vt[5], lty=2)

vtype <- c(25, 75)
axis(4, at=vtype, labels=c(expression(I[k]^(2)), expression(I[k]^(1))),
     las=2, cex.axis=1.1, tck=0.0, lwd=0)

abline(h = 50)

##--- for w=2
points(vt[1], vtype[1], pch=15, cex=1.3, bg="black")
points(vt[2], vtype[1], pch=15, cex=1.3, bg="black")
points(vt[3], vtype[1], pch=15, cex=1.3, bg="black", col="red")
points(vt[4], vtype[1], pch=15, cex=1.3, bg="black", col="red")
points(vt[5], vtype[1], pch=15, cex=1.3, bg="black", col="red")

segments(x0=vt[1],x1=vt[2],y0=vtype[1],y1=vtype[1],lwd=1)

t32 <- (vt[3]-vt[2])/2+vt[2]
segments(x0=vt[2],x1=t32,y0=vtype[1],y1=vtype[1],lwd=1)
segments(x0=t32,x1=t32,y0=vtype[1]-1,y1=vtype[1]+1,lwd=1)

segments(x0=vt[3],x1=vt[4],y0=vtype[1],y1=vtype[1],lwd=2, col="red")
segments(x0=vt[4],x1=vt[5],y0=vtype[1],y1=vtype[1],lwd=2, col="red")

##--- for w=1
points(vt[1], vtype[2], pch=17, cex=1.3, bg="black")
points(vt[2], vtype[2], pch=19, cex=1.3, bg="black")
points(vt[3], vtype[2], pch=19, cex=1.3, bg="black")
points(vt[4], vtype[2], pch=17, cex=1.3, bg="black", col="red")
points(vt[5], vtype[2], pch=17, cex=1.3, bg="black", col="red")

segments(x0=vt[1],x1=vt[2],y0=vtype[2],y1=vtype[2],lwd=1)

t32 <- (vt[3]-vt[2])/2+vt[2]
segments(x0=vt[2],x1=t32,y0=vtype[2],y1=vtype[2],lwd=1)
segments(x0=t32,x1=t32,y0=vtype[2]-1,y1=vtype[2]+1,lwd=1)

segments(x0=vt[3],x1=vt[4],y0=vtype[2],y1=vtype[2],lwd=1)
points(vt[4], vtype[2], pch=17, cex=1.3, bg="black", col="red")
segments(x0=vt[4],x1=vt[5],y0=vtype[2],y1=vtype[2],lwd=2, col="red")

legend("topleft", c(expression(S[1]), expression(S[2]), bquote(S[1]~"or"~S[2])),
       pch=c(19, 17, 15),# bty = "n"
       bg="gray90")

