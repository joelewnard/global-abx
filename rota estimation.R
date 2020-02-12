unique(country)
library(parallel); library(survival); library(MASS); library(metafor)
introyearRV = c(2014+4/12, ### angola
                2012+9.5/12, #### armenia
                2013+11.5/12, ### burundi
                2013+10/12, ### ethiopia
                2013+4/12, ### haiti
                2012+10/12, ### malawi
                2014+11/12, ### senegal
                2009+7/12, ### south africa
                2015, ### tajikistan
                2012+11/12, ### tanzania
                2014+4/12, ### zimbabwe
                2014+3/12) ### sierra leone

income = c(  'lmic', ### angola
             'umic', #### armenia
             'lic', ### burundi
             'lic', ### ethiopia
             'lic', ### haiti
             'lic', ### malawi
             'lic', ### senegal
             'umic', ### south africa
             'lic', ### tajikistan (not implemented)
             'lic', ### tanzania
             'lic', ### zimbabwe
             'lic') ### sierra leone

mortal = c(  'd', ### angola
             'b', #### armenia
             'e', ### burundi
             'e', ### ethiopia
             'd', ### haiti
             'e', ### malawi
             'd', ### senegal
             'e', ### south africa
             'b', ### tajikistan
             'e', ### tanzania
             'e', ### zimbabwe
             'd') ### sierra leone



##################################
#### TAKE ANALYSIS FROM HERE #####
##################################
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
load('dhsdatasetRota.Rdata')
load('laodat.Rdata'); load('sldat.Rdata')
dataset = rbind(dataset,laodat,sldat)


rota2 = dataset$rvdoses
rota2[rota2==1] = NA

dataset = dataset[is.na(dataset$diar)==F&is.na(dataset$pentadoses)==F&is.na(rota2)==F,]



region = dataset$region
wealth = dataset$wealth
edu = dataset$edu
cluster=dataset$cluster
age = dataset$age
country=dataset$country
diar = dataset$diar
agervA = dataset$agervA
agervB = dataset$agervB
agervC = dataset$agervC
rvdoses = dataset$rvdoses
abx = dataset$abx
abxdiar = dataset$abxdiar
pentadoses = dataset$pentadoses
month = dataset$month
year = dataset$year
asx = diar==0
trtdiar = dataset$trtdiar
trtdiar[diar==0] = 0
pcvdoses = dataset$pcvdoses
rota2 = dataset$rvdoses
ivabxdiar = dataset$ivabxdiar
ors = dataset$ors
iv = dataset$iv

trtabxdiar = rep(NA,length(abx))
trtabxdiar[abxdiar==1|ivabxdiar==1] = 1
trtabxdiar[asx==1] = 0

rehyd = rep(NA,length(ors))
rehyd[ors==0] = 0; rehyd[ors>0] = 1; rehyd[iv==1] = 1
rehyd[asx==1] = 0
trtrehyd = rehyd; trtrehyd[trtdiar==1&rehyd==1] = 1; trtrehyd[trtdiar==0|rehyd==0] = 0

sex = ifelse(dataset$sex%in%c('0','FEMALE'),0,ifelse(dataset$sex%in%c('1','MALE'),1,NA))
urban = dataset$urban

rota2 = dataset$rvdoses
rota2[rota2==1] = NA

agelb = c(0,0.5,1,2,3)
ageub = c(0.5,1,2,3,5)


yearintro = ic = mort = c()
for (i in 1:length(introyearRV)){
  yearintro[which(country==unique(country)[i])] = introyearRV[i]
  ic[which(country==unique(country)[i])] = income[i]
  mort[which(country==unique(country)[i])] = mortal[i]
}
sinceintro = year - yearintro
sinceintro[sinceintro<0] = NA

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')

load('diarInputsReg.Rdata')
load('trtdiarInputsReg.Rdata')
load('trtrehydInputsReg.Rdata')
load('trtabxdiarInputsReg.Rdata')
load('diarnegctlabxrespInputs.Rdata')
load('diarnegctltrtrespInputs.Rdata')
load('diarnegctlrespInputs.Rdata')

nums = function(obj){
  case = ctl = case02 = ctl02 = case25 = ctl25 = c()
  for (i in 1:length(obj)){
    for (j in 1:length(obj[[i]])){
      case = c(case,sum(obj[[i]][[j]][age>=0&age<5,1]==1,na.rm=T))
      ctl = c(ctl,sum(obj[[i]][[j]][age>=0&age<5,1]==0,na.rm=T))
      
      case02 = c(case02,sum(obj[[i]][[j]][age>=0&age<2,1]==1,na.rm=T))
      ctl02 = c(ctl02,sum(obj[[i]][[j]][age>=0&age<2,1]==0,na.rm=T))
      
      case25 = c(case25,sum(obj[[i]][[j]][age>=2&age<5,1]==1,na.rm=T))
      ctl25 = c(ctl25,sum(obj[[i]][[j]][age>=2&age<5,1]==0,na.rm=T))
    }
    print(i)
  }
  return(list(range(case),range(ctl),range(case02),range(ctl02),range(case25),range(ctl25)))  
}
nums(diarInputs)


mod.fn = function(i,obj,agelb,ageub,allcountries){
  modAll = modMIC = modLIC = c()
  for (j in 1:20){
    case = obj[[i]][[j]][,1]; m = obj[[i]][[j]][,2]
    
    mod = clogit(case~rota2+strata(m),subset=(age>=agelb&age<ageub))
    modAll[j] = coef(mod)[1]
    
    if (allcountries==1){
      mod = clogit(case~rota2+strata(m),subset=(age>=agelb&age<ageub&ic%in%c('lmic','umic')))
      modMIC[j] = coef(mod)[1]
      
      mod = clogit(case~rota2+strata(m),subset=(age>=agelb&age<ageub&ic%in%c('lic')))
      modLIC[j] = coef(mod)[1]
    } else{
      modMIC[j] = modLIC[j] = NA 
    }
  }
  return(cbind(modAll,modMIC,modLIC))
}


num = 1:20
diar02 = mclapply(num, function(num) mod.fn(num,obj=diarInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
diar25 = mclapply(num, function(num) mod.fn(num,obj=diarInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
diar05 = mclapply(num, function(num) mod.fn(num,obj=diarInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

trtdiar02 = mclapply(num, function(num) mod.fn(num,obj=trtdiarInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
trtdiar25 = mclapply(num, function(num) mod.fn(num,obj=trtdiarInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
trtdiar05 = mclapply(num, function(num) mod.fn(num,obj=trtdiarInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

trtrehyd02 = mclapply(num, function(num) mod.fn(num,obj=trtrehydInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
trtrehyd25 = mclapply(num, function(num) mod.fn(num,obj=trtrehydInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
trtrehyd05 = mclapply(num, function(num) mod.fn(num,obj=trtrehydInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

trtabxdiar02 = mclapply(num, function(num) mod.fn(num,obj=trtabxdiarInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
trtabxdiar25 = mclapply(num, function(num) mod.fn(num,obj=trtabxdiarInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
trtabxdiar05 = mclapply(num, function(num) mod.fn(num,obj=trtabxdiarInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

diarnegctlresp02 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlrespInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
diarnegctlresp25 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlrespInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
diarnegctlresp05 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlrespInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

diarnegctltrtresp02 = mclapply(num, function(num) mod.fn(num,obj=diarnegctltrtrespInputs,agelb=0,ageub=2,allcountries=1),mc.cores=10)
diarnegctltrtresp25 = mclapply(num, function(num) mod.fn(num,obj=diarnegctltrtrespInputs,agelb=2,ageub=5,allcountries=1),mc.cores=10)
diarnegctltrtresp05 = mclapply(num, function(num) mod.fn(num,obj=diarnegctltrtrespInputs,agelb=0,ageub=5,allcountries=1),mc.cores=10)

diarnegctlabxresp02 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlabxrespInputs,agelb=0,ageub=2,allcountries=0),mc.cores=10)
diarnegctlabxresp25 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlabxrespInputs,agelb=2,ageub=5,allcountries=0),mc.cores=10)
diarnegctlabxresp05 = mclapply(num, function(num) mod.fn(num,obj=diarnegctlabxrespInputs,agelb=0,ageub=5,allcountries=0),mc.cores=10)


effdiar02 = efftrtdiar02 = efftrtabxdiar02 = efftrtrehyd02 = effdiarnegctlresp02 = effdiarnegctlabxresp02 = effdiarnegctltrtresp02 = c()
effdiar25 = efftrtdiar25 = efftrtabxdiar25 = efftrtrehyd25 = effdiarnegctlresp25 = effdiarnegctlabxresp25 = effdiarnegctltrtresp25 = c()
effdiar05 = efftrtdiar05 = efftrtabxdiar05 = efftrtrehyd05 = effdiarnegctlresp05 = effdiarnegctlabxresp05 = effdiarnegctltrtresp05 = c()

for (i in 1:20){
  effdiar02 = rbind(effdiar02,diar02[[i]])
  efftrtdiar02 = rbind(efftrtdiar02,trtdiar02[[i]])
  efftrtrehyd02 = rbind(efftrtrehyd02,trtrehyd02[[i]])
  efftrtabxdiar02 = rbind(efftrtabxdiar02,trtabxdiar02[[i]])

  effdiarnegctlresp02 = rbind(effdiarnegctlresp02,diarnegctlresp02[[i]])
  effdiarnegctlabxresp02 = rbind(effdiarnegctlabxresp02,diarnegctlabxresp02[[i]])
  effdiarnegctltrtresp02 = rbind(effdiarnegctltrtresp02,diarnegctltrtresp02[[i]])
  
  effdiar25 = rbind(effdiar25,diar25[[i]])
  efftrtdiar25 = rbind(efftrtdiar25,trtdiar25[[i]])
  efftrtrehyd25 = rbind(efftrtrehyd25,trtrehyd25[[i]])
  efftrtabxdiar25 = rbind(efftrtabxdiar25,trtabxdiar25[[i]])

 effdiarnegctlresp25 = rbind(effdiarnegctlresp25,diarnegctlresp25[[i]])
  effdiarnegctlabxresp25 = rbind(effdiarnegctlabxresp25,diarnegctlabxresp25[[i]])
  effdiarnegctltrtresp25 = rbind(effdiarnegctltrtresp25,diarnegctltrtresp25[[i]])
  
  effdiar05 = rbind(effdiar05,diar05[[i]])
  efftrtdiar05 = rbind(efftrtdiar05,trtdiar05[[i]])
  efftrtrehyd05 = rbind(efftrtrehyd05,trtrehyd05[[i]])
  efftrtabxdiar05 = rbind(efftrtabxdiar05,trtabxdiar05[[i]])
  
  effdiarnegctlresp05 = rbind(effdiarnegctlresp05,diarnegctlresp05[[i]])
  effdiarnegctlabxresp05 = rbind(effdiarnegctlabxresp05,diarnegctlabxresp05[[i]])
  effdiarnegctltrtresp05 = rbind(effdiarnegctltrtresp05,diarnegctltrtresp05[[i]])
}

q95fn = function(x){return(quantile(x,c(0.5,0.025,0.975),na.rm=T))}


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
# 
     save(effdiar02,file='effdiar02.Rdata')
     save(efftrtdiar02,file='efftrtdiar02.Rdata')
     save(efftrtabxdiar02,file='efftrtabxdiar02.Rdata')
     save(efftrtrehyd02,file='efftrtrehyd02.Rdata')

     save(effdiarnegctlresp02,file='effdiarnegctlresp02.Rdata')
     save(effdiarnegctltrtresp02,file='effdiarnegctltrtresp02.Rdata')
     save(effdiarnegctlabxresp02,file='effdiarnegctlabxresp02.Rdata')
# # # 
      save(effdiar25,file='effdiar25.Rdata')
      save(efftrtdiar25,file='efftrtdiar25.Rdata')
      save(efftrtabxdiar25,file='efftrtabxdiar25.Rdata')
      save(efftrtrehyd25,file='efftrtrehyd25.Rdata')

      save(effdiarnegctlresp25,file='effdiarnegctlresp25.Rdata')
      save(effdiarnegctltrtresp25,file='effdiarnegctltrtresp25.Rdata')
      save(effdiarnegctlabxresp25,file='effdiarnegctlabxresp25.Rdata')
      
      save(effdiar05,file='effdiar05.Rdata')
      save(efftrtdiar05,file='efftrtdiar05.Rdata')
      save(efftrtabxdiar05,file='efftrtabxdiar05.Rdata')
      save(efftrtrehyd05,file='efftrtrehyd05.Rdata')

      save(effdiarnegctlresp05,file='effdiarnegctlresp05.Rdata')
      save(effdiarnegctltrtresp05,file='effdiarnegctltrtresp05.Rdata')
      save(effdiarnegctlabxresp05,file='effdiarnegctlabxresp05.Rdata')
#   
  
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
load('effdiar02.Rdata')
load('efftrtdiar02.Rdata')
load('efftrtrehyd02.Rdata')
load('efftrtabxdiar02.Rdata')

load('effdiarnegctlresp02.Rdata')
load('effdiarnegctltrtresp02.Rdata')
load('effdiarnegctlabxresp02.Rdata')

load('effdiar25.Rdata')
load('efftrtdiar25.Rdata')
load('efftrtrehyd25.Rdata')
load('efftrtabxdiar25.Rdata')

load('effdiarnegctlresp25.Rdata')
load('effdiarnegctltrtresp25.Rdata')
load('effdiarnegctlabxresp25.Rdata')

load('effdiar05.Rdata')
load('efftrtdiar05.Rdata')
load('efftrtrehyd05.Rdata')
load('efftrtabxdiar05.Rdata')

load('effdiarnegctlresp05.Rdata')
load('effdiarnegctltrtresp05.Rdata')
load('effdiarnegctlabxresp05.Rdata')



setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/datasets/')

dat = read.csv('ve rota.csv',header=T)
dat = dat[dat$rix==1,]; dat = dat[dat$rct==1,]
y = log(1-dat$ve); ub = log(1-dat$ub)
v = ((y-ub)/1.96)^2
income = as.character(dat$ic)
#library(metafor)
#library(MASS)
mod = rma.uni(yi=y[income=='lic'],vi=v[income=='lic'],method='REML')
set.seed(1); velic = 1-exp(mvrnorm(1e4,coef(mod),vcov(mod)))
mod = rma.uni(yi=y[income!='lic'],vi=v[income!='lic'],method='REML')
set.seed(1); vemic = 1-exp(mvrnorm(1e4,coef(mod),vcov(mod)))
mod = rma.uni(yi=y,vi=v,method='REML')
set.seed(1); veall = 1-exp(mvrnorm(1e4,coef(mod),vcov(mod)))

#library(metafor)
#library(MASS)
quantile(veall,c(0.5,0.025,0.975))
prop.fn = function(ve,or){
  theta = 1-ve
  k = (1-or)/(or-theta)
  prop = k/(k+1)
  return(prop)
}



#quantile(proptrtabxdiar02ic[,,3],c(0.5,0.025,0.975))
propdiar02ic = proptrtdiar02ic = proptrtabxdiar02ic = propdiar25ic = proptrtdiar25ic = proptrtabxdiar25ic = array(NA,dim=c(length(velic),dim(effdiar02)))
proptrtrehyd02ic = proptrtrehyd25ic = array(NA,dim=c(length(velic),dim(effdiar02)))
for (i in 1:length(velic)){
  propdiar02ic[i,,2] = prop.fn(vemic[i],exp(effdiar02[,2]))
  propdiar02ic[i,,3] = prop.fn(velic[i],exp(effdiar02[,3]))
  
  proptrtdiar02ic[i,,2] = prop.fn(vemic[i],exp(efftrtdiar02[,2]))
  proptrtdiar02ic[i,,3] = prop.fn(velic[i],exp(efftrtdiar02[,3]))
  
  proptrtrehyd02ic[i,,2] = prop.fn(vemic[i],exp(efftrtrehyd02[,2]))
  proptrtrehyd02ic[i,,3] = prop.fn(velic[i],exp(efftrtrehyd02[,3]))
  
  proptrtabxdiar02ic[i,,2] = prop.fn(vemic[i],exp(efftrtabxdiar02[,2]))
  proptrtabxdiar02ic[i,,3] = prop.fn(velic[i],exp(efftrtabxdiar02[,3]))
  
  propdiar25ic[i,,2] = prop.fn(vemic[i],exp(effdiar25[,2]))
  propdiar25ic[i,,3] = prop.fn(velic[i],exp(effdiar25[,3]))
  
  proptrtdiar25ic[i,,2] = prop.fn(vemic[i],exp(efftrtdiar25[,2]))
  proptrtdiar25ic[i,,3] = prop.fn(velic[i],exp(efftrtdiar25[,3]))
  
  proptrtrehyd25ic[i,,2] = prop.fn(vemic[i],exp(efftrtrehyd25[,2]))
  proptrtrehyd25ic[i,,3] = prop.fn(velic[i],exp(efftrtrehyd25[,3]))
  
  proptrtabxdiar25ic[i,,2] = prop.fn(vemic[i],exp(efftrtabxdiar25[,2]))
  proptrtabxdiar25ic[i,,3] = prop.fn(velic[i],exp(efftrtabxdiar25[,3]))
}

wmean = function(x){ ### weighted by population ages 0-1y in MICs (large) and LICs (small)
  out = weighted.mean(x,w=c(171142691,16133959))
  return(out)
}
for (i in 1:1e4){
  proptrtabxdiar02ic[i,,1] = apply(proptrtabxdiar02ic[i,,2:3],1,wmean)
  proptrtdiar02ic[i,,1] = apply(proptrtdiar02ic[i,,2:3],1,wmean)
  proptrtrehyd02ic[i,,1] = apply(proptrtrehyd02ic[i,,2:3],1,wmean)
  propdiar02ic[i,,1] = apply(propdiar02ic[i,,2:3],1,wmean)
  print(i)
}




setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
save(proptrtdiar02ic,file='proptrtdiar02ic.Rdata')
save(propdiar02ic,file='propdiar02ic.Rdata')
save(proptrtabxdiar02ic,file='proptrtabxdiar02ic.Rdata')
save(proptrtrehyd02ic,file='proptrtrehyd02ic.Rdata')
save(proptrtdiar25ic,file='proptrtdiar25ic.Rdata')
save(propdiar25ic,file='propdiar25ic.Rdata')
save(proptrtabxdiar25ic,file='proptrtabxdiar25ic.Rdata')
save(proptrtrehyd25ic,file='proptrtrehyd25ic.Rdata')

save(vemic,file='vemic.Rdata')
save(velic,file='velic.Rdata')
