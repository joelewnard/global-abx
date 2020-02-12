setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')

###### incidence under universal coverage
load('spRegAbxIncMaxMed.Rdata'); load('spRegAbxSumMaxMed.Rdata')
load('rotaRegAbxIncMaxMed.Rdata'); load('rotaRegAbxSumMaxMed.Rdata')

###### incidence under current coverage
load('spRegAbxIncMed.Rdata'); load('spRegAbxSumMed.Rdata')
load('rotaRegAbxIncMed.Rdata'); load('rotaRegAbxSumMed.Rdata')

###### incidence under no coverage
load('spIncMed.Rdata'); load('spSumMed.Rdata')
load('rotaIncMed.Rdata'); load('rotaSumMed.Rdata')
load('incomeStat.Rdata')



dev.off()


incplot = function(dat,lb,ub,xoffset,polydiv,tck1,tck2,ylab,lwd){
  plot(1,type='n',axes=F,ann=F,xlim=c(0,4),ylim=c(lb,ub))
  for (i in 1:4){
    xpos = c(1,1.75,2.5,3.5)[i]
    dens = density(dat[i,],na.rm=T)
    
    if (i==4){
      polygon(y=c(dens$x,rev(dens$x)),x=xpos+c(dens$y,-rev(dens$y))/polydiv,col='lightcyan2',lty=0)
      lines(y=c(dens$x[which(dens$x<quantile(dat[i,],0.9975,na.rm=T)&dens$x>quantile(dat[i,],0.01,na.rm=T))],
                rev(dens$x[which(dens$x<quantile(dat[i,],0.9975,na.rm=T)&dens$x>quantile(dat[i,],0.01,na.rm=T))])),
            x=xpos+c(dens$y[which(dens$x<quantile(dat[i,],0.9975,na.rm=T)&dens$x>quantile(dat[i,],0.01,na.rm=T))],
                     -rev(dens$y[which(dens$x<quantile(dat[i,],0.9975,na.rm=T)&dens$x>quantile(dat[i,],0.01,na.rm=T))]))/polydiv,
            col='cadetblue4',lwd=0.25)
    }

    
    lines(y=quantile(dat[i,],c(0.025,0.975),na.rm=T),x=rep(xpos,2),col=ifelse(i==4,'cadetblue4','black'))#,lwd=lwd)
    lines(y=quantile(dat[i,],c(0.025,0.025),na.rm=T),x=xpos+c(-1,1)*xoffset,col=ifelse(i==4,'cadetblue4','black'))#,lwd=lwd)
    lines(y=quantile(dat[i,],c(0.975,0.975),na.rm=T),x=xpos+c(-1,1)*xoffset,col=ifelse(i==4,'cadetblue4','black'))#,lwd=lwd)
    points(y=quantile(dat[i,],0.5,na.rm=T),x=xpos,pch=21,bg='white',col=ifelse(i==4,'cadetblue4','black'),cex=0.65,lwd=lwd)
  }
  box(bty='l')
  axis(side=2,at=lb:ub,tck=tck1,las=1,cex.axis=0.65,labels=10^(lb:ub),lwd=0,lwd.ticks=0.5)
  for (i in lb:(ub-1)){
    axis(side=2,at=log10(2:9)+i,labels=NA,tck=tck2,lwd=0,lwd.ticks=0.5)
  }
  axis(side=1,at=c(1,1.75,2.5,3.5),tck=tck1,las=1,cex.axis=0.65,labels=NA,lwd=0.5,lwd.ticks=0.5,line=0.85)
  text(x=c(1,1.75,2.5,3.5),y=lb-0.25*(ub-lb),srt=45,adj=1,cex=0.65,xpd=T,c('Low','Lower middle','Upper middle','All LMICs'),font=c(1,1,1,2))
  text(x=2.5,y=lb-0.1*(ub-lb),'No vaccination',cex=0.65,font=3,adj=0.5,xpd=T)
  mtext(side=2,ylab,cex=0.5,line=1.5)
}


wideplot = function(dat,lb,ub,xoffset,polydiv,tck1,ylab,by,lwd,bw){
  plot(1,type='n',axes=F,ann=F,xlim=c(0.5,14),ylim=c(lb,ub))
  lowers = c('No vaccination','2018 coverage','Universal coverage')
  for (i in 1:3){
    for (j in 1:4){
      xpos = (i-1)*5+c(1,1.75,2.5,3.5)[j]
      
      dens = density(dat[[i]][j,][which(dat[[i]][j,]>quantile(dat[[i]][j,],0.001,na.rm=T)&dat[[i]][j,]<quantile(dat[[i]][j,],0.995,na.rm=T))],
                     na.rm=T,bw=bw,kernel='optcosine',from=-1e3,to=1e3)
      if (j==4){
        polygon(y=c(dens$x,rev(dens$x)),x=xpos+c(dens$y,-rev(dens$y))/polydiv,col='lavenderblush3',lty=0)
        lines(y=c(dens$x[which(dens$x<quantile(dat[[i]][j,],0.995,na.rm=T)&dens$x>quantile(dat[[i]][j,],0.001,na.rm=T))],
                  rev(dens$x[which(dens$x<quantile(dat[[i]][j,],0.995,na.rm=T)&dens$x>quantile(dat[[i]][j,],0.001,na.rm=T))])),
              x=xpos+c(dens$y[which(dens$x<quantile(dat[[i]][j,],0.995,na.rm=T)&dens$x>quantile(dat[[i]][j,],0.001,na.rm=T))],
                       -rev(dens$y[which(dens$x<quantile(dat[[i]][j,],0.995,na.rm=T)&dens$x>quantile(dat[[i]][j,],0.001,na.rm=T))]))/polydiv,
              col='darkorchid4',lwd=0.25)
      }

      
      lines(y=quantile(dat[[i]][j,],c(0.025,0.975),na.rm=T),x=rep(xpos,2),col=ifelse(j==4,'darkorchid4','black'))#,lwd=lwd)
      lines(y=quantile(dat[[i]][j,],c(0.025,0.025),na.rm=T),x=xpos+c(-1,1)*xoffset,col=ifelse(j==4,'dark red','black'))#,lwd=lwd)
      lines(y=quantile(dat[[i]][j,],c(0.975,0.975),na.rm=T),x=xpos+c(-1,1)*xoffset,col=ifelse(j==4,'dark red','black'))#,lwd=lwd)
      points(y=quantile(dat[[i]][j,],0.5,na.rm=T),x=xpos,pch=21,bg='white',col=ifelse(j==4,'darkorchid4','black'),cex=0.65,lwd=lwd)
    }
    axis(side=1,at=(i-1)*5+c(1,1.75,2.5,3.5),tck=tck1,las=1,cex.axis=0.65,labels=NA,lwd=0.5,lwd.ticks=0.5,line=0.85)
    text(x=(i-1)*5+c(1,1.75,2.5,3.5),y=lb-0.25*(ub-lb),srt=45,adj=1,cex=0.65,xpd=T,c('Low','Lower middle','Upper middle','All LMICs'),
         font=c(1,1,1,2))
    text(x=(i-1)*5+2.5,y=lb-0.1*(ub-lb),lowers[i],cex=0.65,font=3,adj=0.5,xpd=T)
  }
  box(bty='l')
  axis(side=2,at=seq(lb,ub,by),tck=tck1,las=1,cex.axis=0.65,labels=seq(lb,ub,by),lwd=0,lwd.ticks=0.5)
  mtext(side=2,ylab,cex=0.5,line=1.5)
}





pdf('fig4 revised.pdf',width=3.5,height=3.5)
layout(matrix(c(5,5,
                1,2,
                6,6,
                3,4),nrow=4,ncol=2,byrow=T),widths=c(1,2.5),heights=rep(c(0.125,1),2))
par(lwd=0.5); par(mar=c(4.5,2.25,0.5,0.5)); par(mgp=c(3,0.35,0))
incplot(dat=log10(spIncMed[7,3,,]),lb=0,ub=2,xoffset=0.1,polydiv=7,tck1=-0.035,tck2=-0.025,ylab='Incidence per 100',lwd=0.75)
wideplot(dat=list(spSumMed[7,3,,],
                  spRegAbxSumMed[7,3,,],
                  spRegAbxSumMaxMed[7,3,,]),
         lb=0,ub=125,xoffset=0.1,polydiv=1,tck1=-0.035,by=25,ylab=expression(paste('Total cases (',''%*%10^6,')',sep='')),lwd=0.75,bw=0.1)
incplot(dat=log10(rotaIncMed[7,3,,]),lb=0,ub=2,xoffset=0.1,polydiv=7,tck1=-0.035,tck2=-0.025,ylab='Incidence per 100',lwd=0.75)
wideplot(dat=list(rotaSumMed[7,3,,],
                  rotaRegAbxSumMed[7,3,,],
                  rotaRegAbxSumMaxMed[7,3,,]),
         lb=0,ub=125,xoffset=0.1,polydiv=10,tck1=-0.035,by=25,ylab=expression(paste('Total cases (',''%*%10^6,')',sep='')),lwd=0.75,bw=0.01)

polygon(y=130+c(dnorm(seq(-2.5,2.5,0.1),0,1),-rev(dnorm(seq(-2.5,2.5,0.1),0,1)))*10,x=9+c(seq(-2.5,2.5,0.1),rev(seq(-2.5,2.5,0.1)))/2,col='lightcyan2',lty=0,xpd=T)
lines(y=130+c(dnorm(seq(-2.5,2.5,0.1),0,1),-rev(dnorm(seq(-2.5,2.5,0.1),0,1)))*10,x=9+c(seq(-2.5,2.5,0.1),rev(seq(-2.5,2.5,0.1)))/2,col='cadetblue4',lwd=0.25,xpd=T)

polygon(y=110+c(dnorm(seq(-2.5,2.5,0.1),0,1),-rev(dnorm(seq(-2.5,2.5,0.1),0,1)))*10,x=9+c(seq(-2.5,2.5,0.1),rev(seq(-2.5,2.5,0.1)))/2,col='lavenderblush3',lty=0)
lines(y=110+c(dnorm(seq(-2.5,2.5,0.1),0,1),-rev(dnorm(seq(-2.5,2.5,0.1),0,1)))*10,x=9+c(seq(-2.5,2.5,0.1),rev(seq(-2.5,2.5,0.1)))/2,col='darkorchid4',lwd=0.25)

lines(x=9+c(-2.5,2.5)/2,y=rep(130,2),col='cadetblue4',xpd=T)
lines(x=rep(9-1.25,2),y=c(127.5,132.5),col='cadetblue4',xpd=T); lines(x=rep(9+1.25,2),y=c(127.5,132.5),col='cadetblue4',xpd=T)
lines(x=9+c(-2.5,2.5)/2,y=rep(110,2),col='darkorchid4',xpd=T)
lines(x=rep(9-1.25,2),y=c(107.5,112.5),col='darkorchid4',xpd=T); lines(x=rep(9+1.25,2),y=c(107.5,112.5),col='darkorchid4',xpd=T)
points(pch=21,col=c('cadetblue4','darkorchid4'),bg='white',x=c(9,9),y=c(130,110),xpd=T,cex=0.65)
text(y=c(130,110),x=rep(10.75,2),adj=0,c('Incidence est.','Total cases est.'),font=3,cex=0.675,xpd=T)

par(mar=c(0,0,0,0))
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=0,y=0.5,adj=0,xpd=T,'A. Incidence and burden of antibiotic-treated ARI attributable to PCV10/13\n     serotype pneumococci, ages 24-59 months, by country economic stratum',cex=0.75,font=2)
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
text(x=0,y=0.5,adj=0,xpd=T,'B. Incidence and burden of antibiotic-treated diarrhea attributable to rotavirus,\n     ages 0-23 months, by country economic stratum',cex=0.75,font=2)
dev.off()


quantile((spRegAbxSumMed[7,3,4,]-spRegAbxSumMaxMed[7,3,4,])+(rotaRegAbxSumMed[7,3,4,]-rotaRegAbxSumMaxMed[7,3,4,]),c(0.5,0.025,0.975),na.rm=T)
