##### present estimates at country level by WHO region
##### for ARI inc, Diar inc

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')

load('totDiarMed.Rdata')
load('totDiarAbxMed.Rdata')
load('totLriMed.Rdata')
load('totLriAbxMed.Rdata')
load('totLri059Med.Rdata')
load('totLriAbx059Med.Rdata')
load('country.Rdata')
load('inclCountries.Rdata')
load('exclCountries.Rdata')



cbind(exclCountries,exp(imputedExt[1,,323]))

#### need to put in GDP for each of these countries
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
gdp = read.csv('gdpdat.csv',header=T); gdp = gdp[1:218,]
gdp[gdp=='..'] = NA
gdp[,2] = as.numeric(as.character(gdp[,2]))

gdpval = rep(NA,length(country))
for (i in 1:length(country)){
  if (country[i]%in%gdp[,1]){
    gdpval[i] = gdp[which(gdp[,1]==country[i]),2] 
  }
}


gdpval[which(country=='Central African Republic')] = 418.41
gdpval[which(country=='Congo')] = 2292.97
gdpval[which(country=='Congo Democratic Republic')] = 397.58
gdpval[which(country=='Djibouti')] = 1927.59
gdpval[which(country=='Egypt')] = 2608.26
gdpval[which(country=='Eritrea')] = 582.78
gdpval[which(country=='Gambia')] = 520.65
gdpval[which(country=='Guinea Bissau')] = 544.88
gdpval[which(country=='Iran')] = 6161.10
gdpval[which(country=='Micronesia')] = 2716.32
gdpval[which(country=='Palestine')] = 2420.99
gdpval[which(country=='Somalia')] = 499.82
gdpval[which(country=='South Sudan')] = 237.44
gdpval[which(country=='Venezuela')] = 13018
gdpval[which(country=='Yemen')] = 660.28
gdpval[which(country=='Syria')]



colsInc = colsAbx = bgsInc = bgsAbx = rep(NA,length(country))
colsInc[country%in%inclCountries] = 'darkorchid4'; colsInc[country%in%exclCountries] = 'plum4'
colsAbx[country%in%inclCountries] = 'cadetblue4'; colsAbx[country%in%exclCountries] = 'lightsteelblue4'
bgsInc[country%in%inclCountries] = 'lavenderblush3'; bgsInc[country%in%exclCountries] = 'white'
bgsAbx[country%in%inclCountries] = 'lightcyan2'; bgsAbx[country%in%exclCountries] = 'white'


plot.fn = function(yInputInc,yInputAbx,ymax,by){
  plot(1,xlim=c(2,4.2),ylim=c(0,ymax),axes=F,ann=F)
  for (i in 1:length(country)){
    lines(x=rep(log10(gdpval[i]),2),y=quantile(yInputInc[,i],c(0.025,0.975),na.rm=T),
          col=colsInc[i],lwd=ifelse(country[i]%in%inclCountries,0.25,0.25))
    points(x=log10(gdpval[i]),y=median(yInputInc[,i],na.rm=T),col=colsInc[i],lwd=0.5,cex=0.65,bg=bgsInc[i],pch=21)
    
    lines(x=rep(log10(gdpval[i]),2),y=quantile(yInputAbx[,i],c(0.025,0.975),na.rm=T),
          col=colsAbx[i],lwd=ifelse(country[i]%in%inclCountries,0.25,0.25))
    points(x=log10(gdpval[i]),y=median(yInputAbx[,i],na.rm=T),col=colsAbx[i],lwd=0.5,cex=0.65,bg=bgsAbx[i],pch=21)
  }
  box(bty='l')
  axis(1,at=2:4,lwd=0,lwd.ticks=0.5,labels=NA)
  axis(1,at=log10(c(seq(200,900,100),seq(2000,9000,1000),seq(2e4,9e4,1e4))),lwd=0,lwd.ticks=0.25,labels=NA,tck=-0.015)
  axis(2,at=seq(0,ymax,by),labels=seq(0,ymax,by)*100,cex.axis=0.65,las=1,lwd=0,lwd.ticks=0.5)
  mtext(side=2,'Incidence per 100',cex=0.55,line=1.75)
  mtext(side=1,'GDP per cap., USD',cex=0.55,line=1)
  text(x=c(2,3,4)+0.05,y=rep(-0.125*ymax,3),c(expression(10^2),expression(10^3),expression(10^4)),adj=1,srt=45,xpd=T,cex=0.6)
}


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
pdf("fig3 revised.pdf",width=3.5,height=2.25)
layout(matrix(c(1:2,3,3),nrow=2,ncol=2,byrow=T),widths=c(1,1),heights=c(2,0.25))
par(lwd=0.5)
pchs = 21#rep(NA,length(code)); pchs[countriesExtrap==0] = 16; pchs
par(mar=c(2,2.5,1.75,0.25))
par(mgp=c(3,0.35,0))
par(tck=-0.025)

#plot.fn(yInputInc=totLri059Med,yInputAbx=totLriAbx059Med,ymax=8,by=1)
plot.fn(yInputInc=totLriMed,yInputAbx=totLriAbxMed,ymax=2.5,by=0.5)
mtext(side=3,at=1.15,adj=0,line=0.75,'A. ARI incidence',cex=0.55,xpd=T,font=2)
mtext(side=3,at=1.15,adj=0,line=0.1,'Ages 24-59m',cex=0.55,xpd=T,font=3)
plot.fn(yInputInc=totDiarMed,yInputAbx=totDiarAbxMed,ymax=8,by=2)
mtext(side=3,at=1.15,adj=0,line=0.75,'B. Diarrhea incidence',cex=0.55,xpd=T,font=2)
mtext(side=3,at=1.15,adj=0,line=0.1,'Ages 0-23m',cex=0.55,xpd=T,font=3)

par(mar=rep(0,4))
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
lines(x=c(0,0.1),y=rep(0.6,2),lwd=0.5,col='darkorchid4')
points(x=0.05,y=0.6,cex=0.65,lwd=0.5,col='darkorchid4',bg='lavenderblush3',pch=21)

lines(x=c(0,0.1),y=rep(0.2,2),lwd=0.5,col='plum4')
points(x=0.05,y=0.2,cex=0.65,lwd=0.5,col='plum4',bg='white',pch=21)

lines(x=c(0.45,0.55),y=rep(0.6,2),lwd=0.5,col='cadetblue4')
points(x=0.5,y=0.6,cex=0.65,lwd=0.5,col='cadetblue4',bg='lightcyan2',pch=21)

lines(x=c(0.45,0.55),y=rep(0.2,2),lwd=0.5,col='lightsteelblue4')
points(x=0.5,y=0.2,cex=0.65,lwd=0.5,col='lightsteelblue4',bg='white',pch=21)

text(x=c(0.125,0.125,0.575,0.575),y=c(0.6,0.15,0.6,0.15),
     c('All cases (survey est.)','All cases (extrapolated)',
     'Antibiotic-treated cases (survey est.)','Antibiotic-treated cases (extrapolated)'),
     cex=0.6,adj=0)

dev.off()

