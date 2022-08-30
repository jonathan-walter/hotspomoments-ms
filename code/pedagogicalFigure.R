## Make pedagogical figure for Hotspomoments manuscript

rm(list=ls())

library(sn)
library(hotspomoments)
library(RColorBrewer)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


snmedian<-function(n=100000, xi, omega, alpha){
  median(rsn(n,xi,omega,alpha))
}


xseq <- seq(-6,6,by=0.05)
ref <- dnorm(xseq)
right.skew <- dsn(xseq, omega=1.7, alpha=2.2) 
left.skew <- dsn(xseq, omega=1.7, alpha=-2.2)
lepto <- dcauchy(xseq, scale=0.85)

pal <- brewer.pal(3, "Set2")


pdf("../outputs/fig1_pedagogical.pdf", width=6.5, height=2.25)

par(mfrow=c(1,3), mar=c(2.5,4.1,1.6,1.1), mgp=c(2.6,1,0))

plot(xseq , ref, type="l", xlim=c(-5,5), lwd=2, lty=2, xlab="", ylab="Probability density")
lines(xseq - qsn(0.5,xi=0, omega=1.4, alpha=2), right.skew, col=pal[1], lwd=2)
mtext("Right-skewed", cex=2/3, line=0.2)
pp <- par("usr")
text(pp[1] + 0.055*diff(pp[1:2]), pp[4] - 0.05*diff(pp[3:4]), "a)")

plot(xseq, ref, type="l", xlim=c(-5,5), lwd=2, lty=2, xlab="", ylab="Probability density")
lines(xseq - qsn(0.5, xi=0, omega=1.4, alpha=-2), left.skew, col=pal[2], lwd=2)
mtext("Left-skewed", cex=2/3, line=0.2)
pp <- par("usr")
text(pp[1] + 0.055*diff(pp[1:2]), pp[4] - 0.05*diff(pp[3:4]), "b)")

plot(xseq, ref, type="l", xlim=c(-5,5), lwd=2, lty=2, xlab="", ylab="Probability density")
lines(xseq , lepto, col=pal[3], lwd=2)
mtext("Leptokurtic", cex=2/3, line=0.2)
pp <- par("usr")
text(pp[1] + 0.055*diff(pp[1:2]), pp[4] - 0.05*diff(pp[3:4]), "c)")

dev.off()





pdf("../outputs/fig1_pedagogical_v2.pdf", width=3.25, height=3.25)

par(mar=c(2,3.4,2.2,1.1), mgp=c(2.1,0.8,0))

plot(xseq , ref, type="l", xlim=c(-5,5), lwd=2, lty=2, xlab="", ylab="Probability density")
lines(xseq - qsn(0.5,xi=0, omega=1.4, alpha=2), right.skew, col=pal[1], lwd=2)
lines(xseq - qsn(0.5, xi=0, omega=1.4, alpha=-2), left.skew, col=pal[2], lwd=2)
lines(xseq , lepto, col=pal[3], lwd=2)

legend("top", legend=c("Normal","Right skew","Left skew","Leptokurtic"), lwd=2, lty=c(2,1,1,1), 
       col=c("black",pal), bty="n", ncol=2, inset=c(0,-0.2), xpd=TRUE, cex=0.9)

dev.off()
