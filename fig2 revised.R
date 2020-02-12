setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
load('propdiar02ic.Rdata')
load('proptrtdiar02ic.Rdata')
load('proptrtrehyd02ic.Rdata')
load('proptrtabxdiar02ic.Rdata')
load('velic.Rdata')
load('vemic.Rdata')
#
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('proplri25.Rdata')
load('proptrtlri25.Rdata')
load('proptrtabxlri25.Rdata')
load('proplriSens25.Rdata')
load('proptrtlriSens25.Rdata')
load('proptrtabxlriSens25.Rdata')
#
load('proplri05.Rdata')
load('proptrtlri05.Rdata')
load('proptrtabxlri05.Rdata')
load('proplriSens05.Rdata')
load('proptrtlriSens05.Rdata')
load('proptrtabxlriSens05.Rdata')
#
load('veAOM.Rdata')
load('veIPD.Rdata')

quantile(proptrtabxdiar02ic[,,1],c(0.5,0.025,0.975))

#
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')

quantile(proptrtabxlriSens25,c(0.5,0.025,0.975))

densVEaom = density(veAOM); densVEipd = density(veIPD)
densVElic = density(velic); densVEmic = density(vemic)

densVEaom$y = densVEaom$y/max(densVEmic$y)
densVEipd$y = densVEipd$y/max(densVEmic$y)
densVElic$y = densVElic$y/max(densVEmic$y)
densVEmic$y = densVEmic$y/max(densVEmic$y)
#
##for (i in 1:)
#
vln.fn = function(obj,ypos,j,div){
  obj = obj[obj>(-0.2)&obj<1]
  densobj = density(obj,bw=0.01)
  
  polygon(x=c(densobj$x,rev(densobj$x)),y=ypos+c(densobj$y,-rev(densobj$y))/div,
          col=ifelse(cols[j]=='darkorchid4','lavenderblush3',rgb(r[j],g[j],b[j],0.3)),lty=0)
  
  lines(x=quantile(obj,c(0.025,0.975)),y=rep(ypos,2),col=cols[j])
  lines(x=quantile(obj,c(0.025,0.025)),y=ypos+c(-1,1)*0.15,col=cols[j])
  lines(x=quantile(obj,c(0.975,0.975)),y=ypos+c(-1,1)*0.15,col=cols[j])
  sel = which(densobj$x>=quantile(obj,0.005)&densobj$x<=quantile(obj,0.995))
  lines(x=c(densobj$x[sel],rev(densobj$x[sel])),y=ypos+c(densobj$y[sel],-rev(densobj$y[sel]))/div,
        col=cols[j],lwd=0.25)
  points(x=mean(obj),y=ypos,pch=21,col=cols[j],bg='white',cex=0.75)
}
round.fn = function(x,places){
  out = round(x,places)
  if (out==round(x,0)){
    out = paste(out,'.0',sep='')
  }
  return(out)
}

div = 30
r = c(0,1,1,0.65,1); g = c(0,0,0,0.8,0.65); b = c(1,0,1,0.75,0);
cols = c('darkblue','darkred','darkorchid4','darkolivegreen4','darkorange3')
ypos = 1; div = 30






pdf('fig2 revised.pdf',width=3.5,height=2.5)
layout(matrix(c(6,6,6,
                3,4,4,
                7,8,8,
                5,1,2),nrow=4,byrow=T),widths=c(1,rep(1/2,2)),heights=rep(c(0.1,1),2))
par(tck=-0.05); par(mgp=c(3,0.35,0))
par(mar=c(2.75,2.25,2.25,1)); 
plot(1,xlim=c(0,1),ylim=c(0,1),type='n',axes=F,ann=F)
polygon(x=c(densVEipd$x,rev(densVEipd$x)),y=c(densVEipd$y,rep(0,length(densVEipd$x))),
        col='lavenderblush3',lty=0)
lines(x=densVEipd$x,y=densVEipd$y,col='darkorchid4',lwd=0.5)
box(bty='l',lwd=0.5)
axis(side=1,at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5)
axis(side=2,at=seq(0,1,0.25),lwd=0,lwd.ticks=0.5,las=1,cex.axis=0.65)
text(x=seq(0,1,0.25),y=-0.15,seq(0,100,25),srt=45,adj=1,xpd=T,cex=0.65)
mtext(side=1,line=1,'VE (%)',cex=0.475)
mtext(side=2,line=1.75,'Density',cex=0.475)#,adj=-0.35)
text('C1. Efficacy against vaccine\n       serotype invasive\n       pneumococcal disease',font=1,cex=0.65,adj=0,x=-0.75,y=1.4,xpd=T)
text(x=0.7,y=0.5,'All\nLMICs',font=3,cex=0.65,col='darkorchid4',adj=1)
#text('C. Vaccine efficacy i',font=2,cex=0.8,adj=0,x=-0.75,y=1.38,xpd=T)
#text('C1. PCV',font=3,cex=0.7,adj=0,x=-0.65,y=1.15,xpd=T)

par(mar=c(2.75,2.75,2.25,0.5)); 
plot(1,xlim=c(0,1),ylim=c(0,1),type='n',axes=F,ann=F)
polygon(x=c(densVElic$x,rev(densVElic$x)),y=c(densVElic$y,rep(0,length(densVElic$x))),
        col=rgb(1,0,0,0.3),lty=0)
lines(x=densVElic$x,y=densVElic$y,col='darkred',lwd=0.5)
polygon(x=c(densVEmic$x,rev(densVEmic$x)),y=c(densVEmic$y,rep(0,length(densVEmic$x))),
        col=rgb(0,0,1,0.3),lty=0)
lines(x=densVEmic$x,y=densVEmic$y,col='darkblue',lwd=0.5)
box(bty='l',lwd=0.5)
axis(side=1,at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5)
axis(side=2,at=seq(0,1,0.25),lwd=0,lwd.ticks=0.5,las=1,cex.axis=0.65)
text(x=seq(0,1,0.25),y=-0.15,seq(0,100,25),srt=45,adj=1,xpd=T,cex=0.65)
mtext(side=1,line=1,'VE (%)',cex=0.475)
mtext(side=2,line=1.75,'Density',cex=0.475)#,adj=-0.35)
text('C2. Efficacy against\n       confirmed rotavirus\n       gastroenteritis',font=1,cex=0.65,adj=0,x=-0.75,y=1.4,xpd=T)
text(x=0,y=0.2,'LICs',font=3,cex=0.65,col='dark red',adj=0)
text(x=0.65,y=0.65,'MICs',font=3,cex=0.65,col='dark blue',adj=1)

par(tck=-0.035);
par(mar=c(2.75,4.5,1,0.5)); 
plot(1,xlim=c(-0.2,1),ylim=c(0,5),type='n',axes=F,ann=F)
vln.fn(proplri05,ypos=4.25,j=3,div=15)
vln.fn(proptrtlri05,ypos=2.5,j=3,div=15)
vln.fn(proptrtabxlri05,ypos=0.75,j=3,div=15)
#
abline(v=0,lty='dotted',lwd=0.25)
axis(side=1,at=seq(-0.2,1,0.2),lwd=0.5,lwd.ticks=0.5,labels=NA)
text(x=seq(-0.2,1,0.2),seq(-20,100,20),srt=45,cex=0.65,xpd=T,y=-0.5,adj=1)
mtext(side=1,line=1.25,'Attributable fraction (%)',cex=0.475)
#
text(x=-0.1,y=c(0.75,2.5,4.25),
     c('Antibiotic received','Treatment sought','All cases'),cex=0.65,font=3,adj=1,xpd=T)
text('A1. Ages 0-59 months, all LMIC settings',font=1,cex=0.65,adj=0,x=-0.9,y=5.75,xpd=T)

plot(1,xlim=c(-0.2,1),ylim=c(0,5),type='n',axes=F,ann=F)
vln.fn(proplri25,ypos=4.25,j=3,div=18)
vln.fn(proptrtlri25,ypos=2.5,j=3,div=18)
vln.fn(proptrtabxlri25,ypos=0.75,j=3,div=18)
#
abline(v=0,lty='dotted',lwd=0.25)
axis(side=1,at=seq(-0.2,1,0.2),lwd=0.5,lwd.ticks=0.5,labels=NA)
text(x=seq(-0.2,1,0.2),seq(-20,100,20),srt=45,cex=0.65,xpd=T,y=-0.5,adj=1)
mtext(side=1,line=1.25,'Attributable fraction (%)',cex=0.475)
#
text(x=-0.1,y=c(0.75,2.5,4.25),
     c('Antibiotic received','Treatment sought','All cases'),cex=0.65,font=3,adj=1,xpd=T)
text('A2. Ages 24-59 months, all LMIC settings',font=1,cex=0.65,adj=0,x=-0.9,y=5.75,xpd=T)
#text('B. PCV serotypes as ARI etiology, ages 24-59 months',font=2,cex=0.8,adj=0,x=-0.65,y=6,xpd=T)

#
plot(1,xlim=c(-0.2,1),ylim=c(0,5),type='n',axes=F,ann=F)
vln.fn(propdiar02ic[,,1],ypos=4.5,j=3,div=20)
vln.fn(proptrtdiar02ic[,,1],ypos=3.17,j=3,div=20)
vln.fn(proptrtrehyd02ic[,,1],ypos=1.83,j=3,div=20)
vln.fn(proptrtabxdiar02ic[,,1],ypos=0.5,j=3,div=20)
#
abline(v=0,lty='dotted',lwd=0.25)
axis(side=1,at=seq(-0.2,1,0.2),lwd=0.5,lwd.ticks=0.5,labels=NA,tck=-0.015)
text(x=seq(-0.2,1,0.2),seq(-20,100,20),srt=45,cex=0.65,xpd=T,y=-0.5,adj=1)
mtext(side=1,line=1.25,'Attributable fraction (%)',cex=0.475)
text(x=-0.1,y=c(0.5,1.83,3.17,4.5),
     c('Antibiotic received','Rehydration received','Treatment sought','All cases'),cex=0.65,font=3,adj=1,xpd=T)
text('Ages 0-23 months, all LMIC settings',font=1,cex=0.65,adj=0,x=-0.9,y=5.75,xpd=T)
#text('D. Rotavirus as diarrhea etiology, ages 0-23 months',font=2,cex=0.8,adj=0,x=-0.65,y=6,xpd=T)

#plot(1,xlim=c(-0.2,1),ylim=c(0,5),type='n',axes=F,ann=F)
#vln.fn(propdiar02ic[,,2],ypos=4.5,j=1,div=20)
#vln.fn(proptrtdiar02ic[,,2],ypos=3.17,j=1,div=20)
#vln.fn(proptrtrehyd02ic[,,2],ypos=1.83,j=1,div=20)
#vln.fn(proptrtabxdiar02ic[,,2],ypos=0.5,j=1,div=20)
##
#abline(v=0,lty='dotted',lwd=0.25)
#axis(side=1,at=seq(-0.2,1,0.2),lwd=0.5,lwd.ticks=0.5,labels=NA,tck=-0.015)
#text(x=seq(-0.2,1,0.2),seq(-20,100,20),srt=45,cex=0.65,xpd=T,y=-0.5,adj=1)
#mtext(side=1,line=1.25,'Attributable fraction (%)',cex=0.5)
#text(x=-0.1,y=c(0.5,1.83,3.17,4.5),
#     c('Antibiotic received','Rehydration received','Treatment sought','All cases'),cex=0.7,font=3,adj=1,xpd=T)
#text('E. Rotavirus as diarrhea etiology, ages 0-23 months\n(MIC settings)',font=2,cex=0.8,adj=0,x=-0.65,y=6,xpd=T)
#
#plot(1,xlim=c(-0.2,1),ylim=c(0,5),type='n',axes=F,ann=F)
#vln.fn(propdiar02ic[,,3],ypos=4.5,j=2,div=20)
#vln.fn(proptrtdiar02ic[,,3],ypos=3.17,j=2,div=20)
#vln.fn(proptrtrehyd02ic[,,3],ypos=1.83,j=2,div=20)
#vln.fn(proptrtabxdiar02ic[,,3],ypos=0.5,j=2,div=20)
##
#abline(v=0,lty='dotted',lwd=0.25)
#axis(side=1,at=seq(-0.2,1,0.2),lwd=0.5,lwd.ticks=0.5,labels=NA,tck=-0.015)
#text(x=seq(-0.2,1,0.2),seq(-20,100,20),srt=45,cex=0.65,xpd=T,y=-0.5,adj=1)
#mtext(side=1,line=1.25,'Attributable fraction (%)',cex=0.5)
#text(x=-0.1,y=c(0.5,1.83,3.17,4.5),
#     c('Antibiotic received','Rehydration received','Treatment sought','All cases'),cex=0.7,font=3,adj=1,xpd=T)
#text('F. Rotavirus as diarrhea etiology, ages 0-23 months\n(LIC settings)',font=2,cex=0.8,adj=0,x=-0.65,y=6,xpd=T)

par(mar=c(0,0,0,0))
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=-0.035,y=0.5,xpd=T,cex=0.7,font=2,'A. Vaccine serotype pneumococci as ARI etiology',adj=0)

plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=-0.025,y=0.5,xpd=T,cex=0.7,font=2,'B. Rotavirus as diarrhea etiology',adj=0)

plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=-0.04,y=0.5,xpd=T,cex=0.7,font=2,'C. Vaccine efficacy inputs',adj=0)
dev.off()






