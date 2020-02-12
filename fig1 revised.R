setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')

load('efflri02.Rdata')
load('efftrtlri02.Rdata')
load('efftrtabxlri02.Rdata')
load('efflrinegctldiar02.Rdata')
load('efflrinegctltrtdiar02.Rdata')
load('efflrinegctlabxdiar02.Rdata')

load('efflri25.Rdata')
load('efftrtlri25.Rdata')
load('efftrtabxlri25.Rdata')
load('efflrinegctldiar25.Rdata')
load('efflrinegctltrtdiar25.Rdata')
load('efflrinegctlabxdiar25.Rdata')

load('efflri05.Rdata')
load('efftrtlri05.Rdata')
load('efftrtabxlri05.Rdata')
load('efflrinegctldiar05.Rdata')
load('efflrinegctltrtdiar05.Rdata')
load('efflrinegctlabxdiar05.Rdata')


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')

load('effdiar02.Rdata')
load('efftrtdiar02.Rdata')
load('efftrtabxdiar02.Rdata')
load('effdiarnegctlresp02.Rdata')
load('effdiarnegctltrtresp02.Rdata')
load('effdiarnegctlabxresp02.Rdata')

load('effdiar25.Rdata')
load('efftrtdiar25.Rdata')
load('efftrtabxdiar25.Rdata')
load('effdiarnegctlresp25.Rdata')
load('effdiarnegctltrtresp25.Rdata')
load('effdiarnegctlabxresp25.Rdata')

load('effdiar05.Rdata')
load('efftrtdiar05.Rdata')
load('efftrtabxdiar05.Rdata')
load('effdiarnegctlresp05.Rdata')
load('effdiarnegctltrtresp05.Rdata')
load('effdiarnegctlabxresp05.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')


denslri02 = density(1-exp(efflri02)); denslri02$y = denslri02$y#max(denslri02$y)
denslri25 = density(1-exp(efflri25)); denslri25$y = denslri25$y#max(denslri25$y)
denslri05 = density(1-exp(efflri05)); denslri05$y = denslri05$y#max(denslri05$y)

densdiar02 = density(1-exp(effdiar02[,1])); densdiar02$y = densdiar02$y#max(densdiar02$y)
densdiar25 = density(1-exp(effdiar25[,1])); densdiar25$y = densdiar25$y#max(densdiar25$y)
densdiar05 = density(1-exp(effdiar05[,1])); densdiar05$y = densdiar05$y#max(densdiar05$y)

denstrtlri02 = density(1-exp(efftrtlri02)); denstrtlri02$y = denstrtlri02$y#max(denstrtlri02$y)
denstrtlri25 = density(1-exp(efftrtlri25)); denstrtlri25$y = denstrtlri25$y#max(denstrtlri25$y)
denstrtlri05 = density(1-exp(efftrtlri05)); denstrtlri05$y = denstrtlri05$y#max(denstrtlri05$y)

denstrtdiar02 = density(1-exp(efftrtdiar02[,1])); denstrtdiar02$y = denstrtdiar02$y#max(denstrtdiar02$y)
denstrtdiar25 = density(1-exp(efftrtdiar25[,1])); denstrtdiar25$y = denstrtdiar25$y#max(denstrtdiar25$y)
denstrtdiar05 = density(1-exp(efftrtdiar05[,1])); denstrtdiar05$y = denstrtdiar05$y#max(denstrtdiar05$y)

denstrtabxlri02 = density(1-exp(efftrtabxlri02)); denstrtabxlri02$y = denstrtabxlri02$y#max(denstrtabxlri02$y)
denstrtabxlri25 = density(1-exp(efftrtabxlri25)); denstrtabxlri25$y = denstrtabxlri25$y#max(denstrtabxlri25$y)
denstrtabxlri05 = density(1-exp(efftrtabxlri05)); denstrtabxlri05$y = denstrtabxlri05$y#max(denstrtabxlri05$y)

denstrtabxdiar02 = density(1-exp(efftrtabxdiar02[,1])); denstrtabxdiar02$y = denstrtabxdiar02$y#max(denstrtabxdiar02$y)
denstrtabxdiar25 = density(1-exp(efftrtabxdiar25[,1])); denstrtabxdiar25$y = denstrtabxdiar25$y#max(denstrtabxdiar25$y)
denstrtabxdiar05 = density(1-exp(efftrtabxdiar05[,1])); denstrtabxdiar05$y = denstrtabxdiar05$y#max(denstrtabxdiar05$y)

densnegctlresp02 = density(1-exp(effdiarnegctlresp02[,1])); densnegctlresp02$y = densnegctlresp02$y#max(densnegctlresp02$y)
densnegctlresp25 = density(1-exp(effdiarnegctlresp25[,1])); densnegctlresp25$y = densnegctlresp25$y#max(densnegctlresp25$y)
densnegctlresp05 = density(1-exp(effdiarnegctlresp05[,1])); densnegctlresp05$y = densnegctlresp05$y#max(densnegctlresp05$y)

densnegctltrtresp02 = density(1-exp(effdiarnegctltrtresp02[,1])); densnegctltrtresp02$y = densnegctltrtresp02$y#max(densnegctltrtresp02$y)
densnegctltrtresp25 = density(1-exp(effdiarnegctltrtresp25[,1])); densnegctltrtresp25$y = densnegctltrtresp25$y#max(densnegctltrtresp25$y)
densnegctltrtresp05 = density(1-exp(effdiarnegctltrtresp05[,1])); densnegctltrtresp05$y = densnegctltrtresp05$y#max(densnegctltrtresp05$y)

densnegctlabxresp02 = density(1-exp(effdiarnegctlabxresp02[,1])); densnegctlabxresp02$y = densnegctlabxresp02$y#max(densnegctlabxresp02$y)
densnegctlabxresp25 = density(1-exp(effdiarnegctlabxresp25[,1])); densnegctlabxresp25$y = densnegctlabxresp25$y#max(densnegctlabxresp25$y)
densnegctlabxresp05 = density(1-exp(effdiarnegctlabxresp05[,1])); densnegctlabxresp05$y = densnegctlabxresp05$y#max(densnegctlabxresp05$y)

densnegctldiar02 = density(1-exp(efflrinegctldiar02)); densnegctldiar02$y = densnegctldiar02$y#max(densnegctldiar02$y)
densnegctldiar25 = density(1-exp(efflrinegctldiar25)); densnegctldiar25$y = densnegctldiar25$y#max(densnegctldiar25$y)
densnegctldiar05 = density(1-exp(efflrinegctldiar05)); densnegctldiar05$y = densnegctldiar05$y#max(densnegctldiar05$y)

densnegctltrtdiar02 = density(1-exp(efflrinegctltrtdiar02)); densnegctltrtdiar02$y = densnegctltrtdiar02$y#max(densnegctltrtdiar02$y)
densnegctltrtdiar25 = density(1-exp(efflrinegctltrtdiar25)); densnegctltrtdiar25$y = densnegctltrtdiar25$y#max(densnegctltrtdiar25$y)
densnegctltrtdiar05 = density(1-exp(efflrinegctltrtdiar05)); densnegctltrtdiar05$y = densnegctltrtdiar05$y#max(densnegctltrtdiar05$y)

densnegctlabxdiar02 = density(1-exp(efflrinegctlabxdiar02)); densnegctlabxdiar02$y = densnegctlabxdiar02$y#max(densnegctlabxdiar02$y)
densnegctlabxdiar25 = density(1-exp(efflrinegctlabxdiar25)); densnegctlabxdiar25$y = densnegctlabxdiar25$y#max(densnegctlabxdiar25$y)
densnegctlabxdiar05 = density(1-exp(efflrinegctlabxdiar05)); densnegctlabxdiar05$y = densnegctlabxdiar05$y#max(densnegctlabxdiar05$y)


#dev.off()

q.fn = function(obj,x,col,div){
  for (i in 1:length(x)){
    lines(x=rep(x[i],2),y=quantile(obj[,i],c(0.025,0.975)),col=col,lwd=0.75,xpd=T)
    lines(x=x[i]+c(-0.075,0.075),y=quantile(obj[,i],c(0.025,0.025)),col=col,lwd=0.75,xpd=T)
    lines(x=x[i]+c(-0.075,0.075),y=quantile(obj[,i],c(0.975,0.975)),col=col,lwd=0.75,xpd=T)
    points(y=median(obj[,i]),x=x[i],cex=0.75,col=col,bg='white',pch=21,lwd=1)
    
    dens = density(obj[,i])
    lines(y=c(dens$x[which(dens$x>quantile(obj[,i],0.005,na.rm=T)&dens$x<quantile(obj[,i],0.995,na.rm=T))],
              rev(dens$x[which(dens$x>quantile(obj[,i],0.005,na.rm=T)&dens$x<quantile(obj[,i],0.995,na.rm=T))])),
          x=x[i]+c(dens$y[which(dens$x>quantile(obj[,i],0.005,na.rm=T)&dens$x<quantile(obj[,i],0.995,na.rm=T))],
                   -rev(dens$y[which(dens$x>quantile(obj[,i],0.005,na.rm=T)&dens$x<quantile(obj[,i],0.995,na.rm=T))]))/div,
          lwd=0.25,col=col)
  }
}

poly.fn = function(obj,x,colfill,div){
  for (i in 1:length(x)){
    polygon(y=c(obj[[i]]$x,rev(obj[[i]]$x)),x=x[i]+c(obj[[i]]$y,-rev(obj[[i]]$y))/div,col=colfill,lty=0)
  }
}



pdf('fig1 revised.pdf',width=7.2,height=3)

par(mar=c(1,3,1,0.75)); par(mgp=c(3,0.5,0))
vpos = c(2,4)

layout(matrix(c(7,7,7,
                1,2,3,
                8,8,8,
                4,5,6),byrow=T,nrow=4,ncol=3),heights=rep(c(0.1,1),2))


plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.2),lwd=0.5,lty='dotted')}
poly.fn(list(denslri05,denslri02,denslri25),x=c(0.6,2.6,4.6),colfill='lightcyan2',div=55)
poly.fn(list(densnegctldiar05,densnegctldiar02,densnegctldiar25),x=c(1.4,3.4,5.4),colfill='lavenderblush3',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(efflri05,efflri02,efflri25)),x=c(0.6,2.6,4.6),col='cadetblue4',div=55)
q.fn(1-exp(cbind(efflrinegctldiar05,efflrinegctldiar02,efflrinegctldiar25)),x=c(1.4,3.4,5.4),col='darkorchid4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('A1. All cases',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)

polygon(x=c(seq(0.75,1.75,0.01),seq(1.75,0.75,-0.01)),y=0.35+2*c(dnorm(0:100,50,25),-dnorm(0:100,50,25)),
        col='lightcyan2',lty=0)
lines(x=c(seq(0.75,1.75,0.01),seq(1.75,0.75,-0.01)),y=0.35+2*c(dnorm(0:100,50,25),-dnorm(0:100,50,25)),col='cadetblue4',lwd=0.25)
lines(x=c(0.75,1.75),y=rep(0.35,2),col='cadetblue4',lwd=0.75); points(x=1.25,y=0.35,cex=0.75,col='cadetblue4',bg='white',pch=21)
lines(x=rep(0.75,2),y=c(0.33,0.37),col='cadetblue4',lwd=0.75); lines(x=rep(1.75,2),y=c(0.33,0.37),col='cadetblue4',lwd=0.75); 

polygon(x=c(seq(0.75,1.75,0.01),seq(1.75,0.75,-0.01)),y=0.25+2*c(dnorm(0:100,50,25),-dnorm(0:100,50,25)),
        col='lavenderblush3',lty=0)
lines(x=c(seq(0.75,1.75,0.01),seq(1.75,0.75,-0.01)),y=0.25+2*c(dnorm(0:100,50,25),-dnorm(0:100,50,25)),
        col='darkorchid4',lwd=0.25)
lines(x=c(0.75,1.75),y=rep(0.25,2),col='darkorchid4',lwd=0.75); points(x=1.25,y=0.25,cex=0.75,col='darkorchid4',bg='white',pch=21)
lines(x=rep(0.75,2),y=c(0.23,0.27),col='darkorchid4',lwd=0.75); lines(x=rep(1.75,2),y=c(0.23,0.27),col='darkorchid4',lwd=0.75); 
text(x=2,y=c(0.35,0.25),c('ARI endpoints','Diarrhea endpoints'),cex=0.65,adj=0,font=3)


plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.4),lwd=0.5,lty='dotted')}
poly.fn(list(denstrtlri05,denstrtlri02,denstrtlri25),x=c(0.6,2.6,4.6),colfill='lightcyan2',div=55)
poly.fn(list(densnegctltrtdiar05,densnegctltrtdiar02,densnegctltrtdiar25),x=c(1.4,3.4,5.4),colfill='lavenderblush3',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(efftrtlri05,efftrtlri02,efftrtlri25)),x=c(0.6,2.6,4.6),col='cadetblue4',div=55)
q.fn(1-exp(cbind(efflrinegctltrtdiar05,efflrinegctltrtdiar02,efflrinegctltrtdiar25)),x=c(1.4,3.4,5.4),col='darkorchid4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('A2. Cases for which treatment or advice was sought',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)

plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.4),lwd=0.5,lty='dotted')}
poly.fn(list(denstrtabxlri05,denstrtabxlri02,denstrtabxlri25),x=c(0.6,2.6,4.6),colfill='lightcyan2',div=55)
poly.fn(list(densnegctlabxdiar05,densnegctlabxdiar02,densnegctlabxdiar25),x=c(1.4,3.4,5.4),colfill='lavenderblush3',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(efftrtabxlri05,efftrtabxlri02,efftrtabxlri25)),x=c(0.6,2.6,4.6),col='cadetblue4',div=55)
q.fn(1-exp(cbind(efflrinegctlabxdiar05,efflrinegctlabxdiar02,efflrinegctlabxdiar25)),x=c(1.4,3.4,5.4),col='darkorchid4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('A3. Antibiotic-treated cases',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)

plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.4),lwd=0.5,lty='dotted')}
poly.fn(list(densdiar05,densdiar02,densdiar25),x=c(0.6,2.6,4.6),colfill='lavenderblush3',div=55)
poly.fn(list(densnegctlresp05,densnegctlresp02,densnegctlresp25),x=c(1.4,3.4,5.4),colfill='lightcyan2',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(effdiar05[,1],effdiar02[,1],effdiar25[,1])),x=c(0.6,2.6,4.6),col='darkorchid4',div=55)
q.fn(1-exp(cbind(effdiarnegctlresp05[,1],effdiarnegctlresp02[,1],effdiarnegctlresp25[,1])),x=c(1.4,3.4,5.4),col='cadetblue4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('B1. All cases',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)


plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.4),lwd=0.5,lty='dotted')}
poly.fn(list(denstrtdiar05,denstrtdiar02,denstrtdiar25),x=c(0.6,2.6,4.6),colfill='lavenderblush3',div=55)
poly.fn(list(densnegctltrtresp05,densnegctltrtresp02,densnegctltrtresp25),x=c(1.4,3.4,5.4),colfill='lightcyan2',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(efftrtdiar05[,1],efftrtdiar02[,1],efftrtdiar25[,1])),x=c(0.6,2.6,4.6),col='darkorchid4',div=55)
q.fn(1-exp(cbind(effdiarnegctltrtresp05[,1],effdiarnegctltrtresp02[,1],effdiarnegctltrtresp25[,1])),x=c(1.4,3.4,5.4),col='cadetblue4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('B2. Cases for which treatment or advice was sought',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)

plot(xlim=c(0,6),ylim=c(-0.4,0.4),1,axes=F,ann=F)
for (j in 1:2){lines(x=rep(vpos[j],2),y=c(-0.4,0.4),lwd=0.5,lty='dotted')}
poly.fn(list(denstrtabxdiar05,denstrtabxdiar02,denstrtabxdiar25),x=c(0.6,2.6,4.6),colfill='lavenderblush3',div=55)
poly.fn(list(densnegctlabxresp05,densnegctlabxresp02,densnegctlabxresp25),x=c(1.4,3.4,5.4),colfill='lightcyan2',div=55)
abline(h=0,lwd=0.5,col='dark grey')
q.fn(1-exp(cbind(efftrtabxdiar05[,1],efftrtabxdiar02[,1],efftrtabxdiar25[,1])),x=c(0.6,2.6,4.6),col='darkorchid4',div=55)
q.fn(1-exp(cbind(effdiarnegctlabxresp05[,1],effdiarnegctlabxresp02[,1],effdiarnegctlabxresp25[,1])),x=c(1.4,3.4,5.4),col='cadetblue4',div=55)
axis(at=seq(-0.4,0.4,by=0.1),labels=c(-40,NA,-20,NA,0,NA,20,NA,40),tck=-0.025,lwd=0.5,lwd.ticks=0.5,side=2,las=1,cex.axis=0.75)
text(x=c(0,2,4)+0.1,y=rep(-0.375,3),xpd=T,c('Ages\n0-59m','Ages\n0-23m','Ages\n24-59m'),font=3,cex=0.75,adj=0)
text('B3. Antibiotic-treated cases',adj=0,cex=0.75,x=-1.5,y=0.5,xpd=T)
mtext(side=2,'Vaccine effectiveness (%)',line=2,cex=0.5)

par(mar=c(0,0,0,0))
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=-0.036,y=0.5,'A. Effect of 3 or more PCV10/13 doses',font=2,xpd=T,cex=0.85,adj=0)
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=-0.036,y=0.5,'B. Effect of 2 or more rotavirus vaccine doses',font=2,xpd=T,cex=0.85,adj=0)
dev.off()
