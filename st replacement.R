### hammitt (lancet 2019)
vtK = c(0.338,0.088)
nvtK = c(0.406,0.699)

### gambia (usuf cid 2019)
vtG = c(0.333,0.114)
nvtG = c(0.531,0.744)

#### malawi (heinsbroek vaccine 2018)
vtM = c((8+32+93)/(70+71+330),(19+4+19+17+37)/(146+44+83+103+207))
nvtM = c((19+21+103)/(70+71+330),(45+18+34+55+104)/(146+44+83+103+207))


##### dis per carrier: lewnard CID 2017 (OM)
vtAOM = (11.4+9.9)/(0.39+0.18)
nvtAOM = (3.0+1.8)/(0.37+0.22)

#### dis per carrier: shouval pidj 2009
vtIPD = 0.84/0.67
nvtIPD = (1-0.84)/(1-0.67)

nvtAOM/vtAOM
nvtIPD/vtIPD

#######################
rel = seq(0.01,1,0.01)
#######################
lambdaPreK = vtK[1]#+nvtK[1]*rel
excessK = (nvtK[2]-nvtK[1])*rel/lambdaPreK

lambdaPreG = vtG[1]#+nvtG[1]*rel
excessG = (nvtG[2]-nvtG[1])*rel/lambdaPreG

lambdaPreM = vtM[1]#+nvtM[1]*rel
excessM = (nvtM[2]-nvtM[1])*rel/lambdaPreM

#############

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('proptrtabxlri25.Rdata')
load('proplri25.Rdata')

set.seed(1)
frac = sample(proptrtabxlri25,1e3)
frac1 = sample(proplri25,1e3)

excessKac = excessGac = excessMac = array(NA,dim=c(1e3,100))
excessKac1 = excessGac1 = excessMac1 = array(NA,dim=c(1e3,100))
for (i in 1:1e3){
  
  excessKac1[i,] = (nvtK[2]-nvtK[1])*rel*frac1[i]/lambdaPreK
  excessGac1[i,] = (nvtG[2]-nvtG[1])*rel*frac1[i]/lambdaPreG
  excessMac1[i,] = (nvtM[2]-nvtM[1])*rel*frac1[i]/lambdaPreM
  
  excessKac[i,] = (nvtK[2]-nvtK[1])*rel*frac[i]/lambdaPreK
  excessGac[i,] = (nvtG[2]-nvtG[1])*rel*frac[i]/lambdaPreG
  excessMac[i,] = (nvtM[2]-nvtM[1])*rel*frac[i]/lambdaPreM
}

q95fn = function(x){return(quantile(x,c(0.5,0.025,0.975)))}
excessKac = apply(excessKac,2,q95fn)
excessMac = apply(excessMac,2,q95fn)
excessGac = apply(excessGac,2,q95fn)

excessKac1 = apply(excessKac1,2,q95fn)
excessMac1 = apply(excessMac1,2,q95fn)
excessGac1 = apply(excessGac1,2,q95fn)

############
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
pdf('st replace fig.pdf',width=6,height=3)
par(mar=c(3,3,2,1))
layout(matrix(c(1,2),nrow=1))
par(mgp=c(3,0.35,0))
plot(y=excessK,x=rel,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
par(tck=-0.025)
#polygon(x=c(0.22,0.39,0.39,0.22),y=c(0,0,1,1),col='gray85',lty=0)
abline(v=c(0.22,0.39),lty='dashed',lwd=0.5)
lines(x=c(-100,0.22),y=rep(excessG[22],2),lty='dotted',lwd=0.5)
lines(x=c(-100,0.39),y=rep(excessK[39],2),lty='dotted',lwd=0.5)
lines(y=excessK,x=rel,col='darkorchid4',lwd=0.5); points(y=excessK[c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='darkorchid4',cex=0.65,lwd=0.75)
lines(y=excessG,x=rel,col='darkslategray4',lwd=0.5); points(y=excessG[c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='darkslategray4',cex=0.65,lwd=0.75)
lines(y=excessM,x=rel,col='forestgreen',lwd=0.5); points(y=excessM[c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='forestgreen',cex=0.65,lwd=0.75)
box(bty='l',lwd=0.5)
axis(2,at=seq(0,1,0.2),labels=seq(0,1,0.2),las=1,cex.axis=0.6,lwd=0,lwd.ticks=0.5)
axis(1,at=seq(0,1,0.2),labels=NA,las=1,lwd=0,lwd.ticks=0.5)
text(y=-0.1,x=seq(0,1,0.2),seq(0,1,0.2),srt=45,xpd=T,cex=0.6,adj=1)
mtext(side=2,'Ratio of replacement NVT burden\nto pre-vaccination VT burden',line=1.5,cex=0.65)
mtext(side=1,'Relative disease potential\nof NVT (ref. VT)',line=1.75,cex=0.65)
mtext(side=3,'A',cex=0.75,font=2,adj=0)

plot(y=excessK,x=rel,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
par(tck=-0.025)
abline(v=c(0.22,0.39),lty='dashed',lwd=0.5)
lines(x=c(-100,0.22),y=rep(excessGac[1,22],2),lty='dotted',lwd=0.5)
lines(x=c(-100,0.39),y=rep(excessKac[1,39],2),lty='dotted',lwd=0.5)
lines(y=excessKac[1,],x=rel,col='darkorchid4',lwd=0.5); points(y=excessKac[1,c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='darkorchid4',cex=0.65,lwd=0.75)
lines(y=excessGac[1,],x=rel,col='darkslategray4',lwd=0.5); points(y=excessGac[1,c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='darkslategray4',cex=0.65,lwd=0.75)
lines(y=excessMac[1,],x=rel,col='forestgreen',lwd=0.5); points(y=excessMac[1,c(22,39)],x=c(0.22,0.39),pch=c(23,22),bg='white',col='forestgreen',cex=0.65,lwd=0.75)
box(bty='l',lwd=0.5)
axis(2,at=seq(0,1,0.2),labels=seq(0,1,0.2),las=1,cex.axis=0.6,lwd=0,lwd.ticks=0.5)
axis(1,at=seq(0,1,0.2),labels=NA,las=1,lwd=0,lwd.ticks=0.5)
text(y=-0.1,x=seq(0,1,0.2),seq(0,1,0.2),srt=45,xpd=T,cex=0.6,adj=1)
mtext(side=2,'Ratio of replacement NVT burden to\npre-vaccination ARI burden (all causes)',line=1.5,cex=0.65)
mtext(side=3,'B',cex=0.75,font=2,adj=0)

points(y=1,x=0.5,pch=23,cex=0.65,lwd=0.75,xpd=T)
points(y=0.85,x=0.5,pch=22,cex=0.65,lwd=0.75)
text(y=c(1,0.85),x=0.55,adj=0,c('AOM rel. disease potential\n(Lewnard et al., 2017)','IPD rel. disease potential\n(Shouval et al., 2009)'),cex=0.5,xpd=T)
mtext(side=1,'Relative disease potential\nof NVT (ref. VT)',line=1.75,cex=0.65)

text(x=0.45,y=0.7,'Replacement ests.',cex=0.55,adj=0)
lines(y=rep(0.6,2),x=c(0.45,0.525),lwd=0.5,col='darkorchid4')
lines(y=rep(0.45,2),x=c(0.45,0.525),lwd=0.5,col='darkslategray4')
lines(y=rep(0.3,2),x=c(0.45,0.525),lwd=0.5,col='forestgreen')
text(x=rep(0.55,3),y=c(0.6,0.45,0.3),adj=0,c('Kenya\n(Hammit et al., 2019)',
                                            'The Gambia\n(Usuf et al., 2019)',
                                            'Malawi\n(Heinsbrook et al., 2018)'),cex=0.5)

dev.off()
