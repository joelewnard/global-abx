setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')

load('predFinal.Rdata'); load('outofsampFinal.Rdata'); load('outofsamptestFinal.Rdata')

dev.off()

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')

pdf(file='ml validation revised.pdf',width=4,height=6)
par(mar=c(4,4,2,0.5))
layout(matrix(1:6,nrow=3,ncol=2,byrow=F))
par(lwd=0.5)
par(tck=-0.025)
par(mgp=c(3,0.35,0))
labs = c('All diarrhea,\nages 0-23m','All ARI,\nages 24-59m','All ARI,\nages 0-59m',
         'Antibiotic-treated diarrhea,\nages 0-23m','Antibiotic-treated ARI,\nages 24-59m',
         'Antibiotic-treated ARI,\nages 0-59m')
for (j in 1:6){
  plot(y=exp(outofsampFinal)[,,j,],x=exp(outofsamptestFinal)[,,j,],pch=16,col=rgb(0.5,0.49,0.51,0.15),cex=0.25,
       xlim=range(exp(c(outofsampFinal[,,j,],outofsamptestFinal[,,j,])),na.rm=T),
       ylim=range(exp(c(outofsampFinal[,,j,],outofsamptestFinal[,,j,])),na.rm=T),axes=F,ann=F)
  lines(x=c(-100,100),y=c(-100,100),col='red',lwd=0.5)
  box(bty='l')
  axis(side=1,lwd=0,lwd.ticks=0.5)
  axis(side=2,lwd=0,lwd.ticks=0.5,las=1)
  mtext(side=1,'Survey estimate',cex=0.65,line=2)
  mtext(side=2,'Holdout sample\nestimate',cex=0.65,line=2)
  mtext(side=3,labs[j],cex=0.55,line=0,adj=0)
}
dev.off()
