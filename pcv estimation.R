setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('dhsdatasetPCV.Rdata')
load('laodat.Rdata'); load('sldat.Rdata')

dataset = rbind(dataset,laodat,sldat)
dataset = dataset[is.na(dataset$pentadoses)==F,]
dataset = dataset[is.na(dataset$breath)==F&is.na(dataset$cough)==F&is.na(dataset$chest)==F,]
dataset = dataset[is.na(dataset$pcvdoses)==F,]
dataset = dataset[is.na(dataset$nose)==F,]

wealth = dataset$wealth
region = dataset$region
edu = dataset$edu
cluster=dataset$cluster
age = dataset$age
country=dataset$country
diar = dataset$diar
agepcvA = dataset$agepcvA
agepcvB = dataset$agepcvB
agepcvC = dataset$agepcvC
pcvdoses = dataset$pcvdoses
rvdoses = dataset$rvdoses
abx = dataset$abx
ivabx = dataset$ivabx
abxdiar = dataset$abxdiar
ivabxdiar = dataset$ivabxdiar
pentadoses = dataset$pentadoses
penta = pentadoses>0
month = dataset$month
year = dataset$year
chest = dataset$chest; breath = dataset$breath; nose = dataset$nose; cough = dataset$cough
asx = chest==0&cough==0&breath==0&nose==0
lri = chest==1
trtresp = dataset$trtresp
trtresp[asx==1] = 0
trtlri = trtresp
trtlri[lri==0] = 0
pcvdoses = dataset$pcvdoses
abx[ivabx==1&is.na(abx)] = 1

trtabxlri = rep(NA,length(abx))
trtabxlri[abx==1|ivabx==1] = 1
trtabxlri[lri==0] = NA
trtabxlri[asx==1] = NA

ari = cough==1|breath==1
trtari = ari&trtresp
trtabxari = rep(NA,length(abx))
trtabxari = ari&abx

trtabxdiar = rep(NA,length(abx))
trtabxdiar[abxdiar==1|ivabxdiar==1] = 1

fever = dataset$fever
sex = ifelse(dataset$sex%in%c('0','FEMALE'),0,ifelse(dataset$sex%in%c('1','MALE'),1,NA))
urban = dataset$urban

pcv3 = pcvdoses; pcv3[pcvdoses%in%c(1,2)] = NA


unique(country)

unique(country)
introyearPCV = c(2011+2.5/12, ### albania
              2013+5/12, ### angola
              2014+8.5/12, #### armenia
              2011+8.5/12, ### burundi
              2011+9.5/12, ### ethiopia
              2011+10.5/12, ### malawi
              2015+0.5/12, ### nepal
              2012+10/12, ### pakistan
              2013+6.5/12, ### philippines (phased)
              2013+10/12, ### senegal
              2009+3/12, ### south africa
              2012+11/12, ### tanzania
              2013+4/12, ### uganda
              2012+7/12, ### zimbabwe
              2013+9/12, ### lao
              2011+1/12) ### sierra leone

income = c(
  'umic', ### albania
  'lmic', ### angola
  'umic', #### armenia
  'lic', ### burundi
  'lic', ### ethiopia
  'lic', ### malawi
    'lic', ### nepal
  'lmic', ### pakistan
    'lmic', ### philippines (phased)
  'lic', ### senegal
  'umic', ### south africa
  'lic', ### tanzania
    'lic', ### uganda
  'lic', ### zimbabwe
    'lmic', ### lao
  'lic') ### sierra leone

mortal = c(
  'b', ### albania
  'd', ### angola
  'b', #### armenia
  'e', ### burundi
  'e', ### ethiopia
  'e', ### malawi
  'd', ### nepal
  'd', ### pakistan
  'b', ### philippines (phased)
  'd', ### senegal
  'e', ### south africa
  'e', ### tanzania
  'e', ### uganda
  'e', ### zimbabwe
  'b', ### lao
  'd') ### sierra leone

yearintro = ic = mort = c()
for (i in 1:length(introyearPCV)){
  yearintro[which(country==unique(country)[i])] = introyearPCV[i]
  ic[which(country==unique(country)[i])] = income[i]
  mort[which(country==unique(country)[i])] = mortal[i]
}
sinceintro = year - yearintro
sinceintro[sinceintro<0] = NA

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv/')

load('lriInputs.Rdata')
load('trtlriInputs.Rdata')
load('trtabxlriInputs.Rdata')
load('lrinegctldiarInputs.Rdata')
load('lrinegctlabxdiarInputs.Rdata')
load('lrinegctltrtdiarInputs.Rdata')

table(trtabxlri[lri==1&is.na(pcv3)==F])

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
nums(lrinegctltrtdiarInputs)


library(survival)
mod.fn = function(i,obj,agelb,ageub){
  modAll = modMIC = modLIC = c() ### do this by mortality
  for (j in 1:20){
    case = obj[[i]][[j]][,1]; m = obj[[i]][[j]][,2]
    mod = clogit(case~pcv3+strata(m),subset=(age>=agelb&age<ageub),method='approximate')
    modAll[j] = coef(mod)[1]
  }
  return(modAll)
}



num = 1:100
library(parallel)
lri02 = mclapply(num, function(num) mod.fn(num,obj=lriInputs,agelb=0,ageub=2),mc.cores=10)
lri25 = mclapply(num, function(num) mod.fn(num,obj=lriInputs,agelb=2,ageub=5),mc.cores=10)
lri05 = mclapply(num, function(num) mod.fn(num,obj=lriInputs,agelb=0,ageub=5),mc.cores=10)

trtlri02 = mclapply(num, function(num) mod.fn(num,obj=trtlriInputs,agelb=0,ageub=2),mc.cores=10)
trtlri25 = mclapply(num, function(num) mod.fn(num,obj=trtlriInputs,agelb=2,ageub=5),mc.cores=10)
trtlri05 = mclapply(num, function(num) mod.fn(num,obj=trtlriInputs,agelb=0,ageub=5),mc.cores=10)

trtabxlri02 = mclapply(num, function(num) mod.fn(num,obj=trtabxlriInputs,agelb=0,ageub=2),mc.cores=10)
trtabxlri25 = mclapply(num, function(num) mod.fn(num,obj=trtabxlriInputs,agelb=2,ageub=5),mc.cores=10)
trtabxlri05 = mclapply(num, function(num) mod.fn(num,obj=trtabxlriInputs,agelb=0,ageub=5),mc.cores=10)

lrinegctldiar02 = mclapply(num, function(num) mod.fn(num,obj=lrinegctldiarInputs,agelb=0,ageub=2),mc.cores=10)
lrinegctldiar25 = mclapply(num, function(num) mod.fn(num,obj=lrinegctldiarInputs,agelb=2,ageub=5),mc.cores=10)
lrinegctldiar05 = mclapply(num, function(num) mod.fn(num,obj=lrinegctldiarInputs,agelb=0,ageub=5),mc.cores=10)

lrinegctlabxdiar02 = mclapply(num, function(num) mod.fn(num,obj=lrinegctlabxdiarInputs,agelb=0,ageub=2),mc.cores=10)
lrinegctlabxdiar25 = mclapply(num, function(num) mod.fn(num,obj=lrinegctlabxdiarInputs,agelb=2,ageub=5),mc.cores=10)
lrinegctlabxdiar05 = mclapply(num, function(num) mod.fn(num,obj=lrinegctlabxdiarInputs,agelb=0,ageub=5),mc.cores=10)

lrinegctltrtdiar02 = mclapply(num, function(num) mod.fn(num,obj=lrinegctltrtdiarInputs,agelb=0,ageub=2),mc.cores=10)
lrinegctltrtdiar25 = mclapply(num, function(num) mod.fn(num,obj=lrinegctltrtdiarInputs,agelb=2,ageub=5),mc.cores=10)
lrinegctltrtdiar05 = mclapply(num, function(num) mod.fn(num,obj=lrinegctltrtdiarInputs,agelb=0,ageub=5),mc.cores=10)




efflri02 = unlist(lri02)
efflri25 = unlist(lri25)
efflri05 = unlist(lri05)

efftrtlri02 = unlist(trtlri02)
efftrtlri25 = unlist(trtlri25)
efftrtlri05 = unlist(trtlri05)

efftrtabxlri02 = unlist(trtabxlri02)
efftrtabxlri25 = unlist(trtabxlri25)
efftrtabxlri05 = unlist(trtabxlri05)

efflrinegctldiar02 = unlist(lrinegctldiar02)
efflrinegctldiar25 = unlist(lrinegctldiar25)
efflrinegctldiar05 = unlist(lrinegctldiar05)

efflrinegctlabxdiar02 = unlist(lrinegctlabxdiar02)
efflrinegctlabxdiar25 = unlist(lrinegctlabxdiar25)
efflrinegctlabxdiar05 = unlist(lrinegctlabxdiar05)

efflrinegctltrtdiar02 = unlist(lrinegctltrtdiar02)
efflrinegctltrtdiar25 = unlist(lrinegctltrtdiar25)
efflrinegctltrtdiar05 = unlist(lrinegctltrtdiar05)

round(100*quantile(1-exp(efflrinegctltrtdiar05),c(0.5,0.025,0.975)),1)

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
save(efflri02,file='efflri02.Rdata')
save(efflri25,file='efflri25.Rdata')
save(efflri05,file='efflri05.Rdata')
save(efftrtlri02,file='efftrtlri02.Rdata')
save(efftrtlri25,file='efftrtlri25.Rdata')
save(efftrtlri05,file='efftrtlri05.Rdata')
save(efftrtabxlri02,file='efftrtabxlri02.Rdata')
save(efftrtabxlri25,file='efftrtabxlri25.Rdata')
save(efftrtabxlri05,file='efftrtabxlri05.Rdata')
save(efflrinegctldiar02,file='efflrinegctldiar02.Rdata')
save(efflrinegctldiar25,file='efflrinegctldiar25.Rdata')
save(efflrinegctldiar05,file='efflrinegctldiar05.Rdata')
save(efflrinegctlabxdiar02,file='efflrinegctlabxdiar02.Rdata')
save(efflrinegctlabxdiar25,file='efflrinegctlabxdiar25.Rdata')
save(efflrinegctlabxdiar05,file='efflrinegctlabxdiar05.Rdata')
save(efflrinegctltrtdiar02,file='efflrinegctltrtdiar02.Rdata')
save(efflrinegctltrtdiar25,file='efflrinegctltrtdiar25.Rdata')
save(efflrinegctltrtdiar05,file='efflrinegctltrtdiar05.Rdata')


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('efflri02.Rdata')
load('efflri25.Rdata')
load('efflri05.Rdata')
load('efftrtlri02.Rdata')
load('efftrtlri25.Rdata')
load('efftrtlri05.Rdata')
load('efftrtabxlri02.Rdata')
load('efftrtabxlri25.Rdata')
load('efftrtabxlri05.Rdata')
load('efflrinegctldiar02.Rdata')
load('efflrinegctldiar25.Rdata')
load('efflrinegctldiar05.Rdata')
load('efflrinegctlabxdiar02.Rdata')
load('efflrinegctlabxdiar25.Rdata')
load('efflrinegctlabxdiar05.Rdata')
load('efflrinegctltrtdiar02.Rdata')
load('efflrinegctltrtdiar25.Rdata')
load('efflrinegctltrtdiar05.Rdata')

quantile(1-exp(efftrtabxlri25),c(0.5,0.025,0.975))

### OM PCV serotype efficacy: FinnOM, COMPAS trial
####### FinnOM: 57 (44, 67)
####### COMPAS: 55.7 (21.5, 75.0)
####### POET: 52.6 (35.0, 65.5)
####### O'Brien: 64 (-34, 90)
y = log(1-c(0.57,0.557,0.526,0.64))
ub = log(1-c(0.67,0.75,0.655,0.9))
v = ((y-ub)/1.96)^2

library(metafor)
mod = rma.uni(yi=y,vi=v,method='REML')
summary(mod)
library(MASS)
set.seed(1); veAOM = 1-exp(mvrnorm(1e4,coef(mod),vcov(mod)))

### IPD PCV serotype efficacy
####### in LMIC settings
####### Gambia: Cutts 2009 (Lancet): 77 (51-90)
####### S Af: Klugman 2003 (NEJM): 83 (23, 96)
####### COMPAS: Tregnaghi 2014 (PLoS Med): 100 (77.3, 100)
y = log(1-c(0.77,0.83,0.99)); lb = log(1-c(0.51,0.23,0.773))
v = ((lb-y)/1.96)^2
mod = rma.uni(yi=y,vi=v,method='REML')
set.seed(1); veIPD = 1-exp(mvrnorm(1e4,coef(mod),vcov(mod)))

#save(veAOM,file='veAOM.Rdata'); save(veIPD,file='veIPD.Rdata')

prop.fn = function(ve,or){
  theta = 1-ve
  k = (1-or)/(or-theta)
  prop = k/(k+1)
  return(prop)
}


proplri02 = proplri25 = proptrtlri02 = proptrtlri25 = proptrtabxlri02 = proptrtabxlri25 = proplri05 = proptrtlri05 = proptrtabxlri05 = array(NA,dim=c(length(veIPD),length(efflri02)))
for (i in 1:length(veIPD)){
  proplri02[i,] = prop.fn(veIPD[i],exp(efflri02))
  proplri25[i,] = prop.fn(veIPD[i],exp(efflri25))
  proplri05[i,] = prop.fn(veIPD[i],exp(efflri05))
  
  proptrtlri02[i,] = prop.fn(veIPD[i],exp(efftrtlri02))
  proptrtlri25[i,] = prop.fn(veIPD[i],exp(efftrtlri25))
  proptrtlri05[i,] = prop.fn(veIPD[i],exp(efftrtlri05))
  
  proptrtabxlri02[i,] = prop.fn(veIPD[i],exp(efftrtabxlri02))
  proptrtabxlri25[i,] = prop.fn(veIPD[i],exp(efftrtabxlri25))
  proptrtabxlri05[i,] = prop.fn(veIPD[i],exp(efftrtabxlri05))
}

quantile(proptrtabxlri25,c(0.5,0.025,0.975))

save(proplri02,file='proplri02.Rdata')
save(proplri25,file='proplri25.Rdata')
save(proplri05,file='proplri05.Rdata')
save(proptrtlri02,file='proptrtlri02.Rdata')
save(proptrtlri25,file='proptrtlri25.Rdata')
save(proptrtlri05,file='proptrtlri05.Rdata')
save(proptrtabxlri02,file='proptrtabxlri02.Rdata')
save(proptrtabxlri25,file='proptrtabxlri25.Rdata')
save(proptrtabxlri05,file='proptrtabxlri05.Rdata')


proplriSens02 = proplriSens25 = proptrtlriSens02 = proptrtlriSens25 = proptrtabxlriSens02 = proptrtabxlriSens25 = proplriSens05 = proptrtlriSens05 = proptrtabxlriSens05 = array(NA,dim=c(length(veIPD),length(efflri02)))
for (i in 1:length(veAOM)){
  proplriSens02[i,] = prop.fn(veAOM[i],exp(efflri02))
  proplriSens25[i,] = prop.fn(veAOM[i],exp(efflri25))
  proplriSens05[i,] = prop.fn(veAOM[i],exp(efflri05))
  
  proptrtlriSens02[i,] = prop.fn(veAOM[i],exp(efftrtlri02))
  proptrtlriSens25[i,] = prop.fn(veAOM[i],exp(efftrtlri25))
  proptrtlriSens05[i,] = prop.fn(veAOM[i],exp(efftrtlri05))
  
  proptrtabxlriSens02[i,] = prop.fn(veAOM[i],exp(efftrtabxlri02))
  proptrtabxlriSens25[i,] = prop.fn(veAOM[i],exp(efftrtabxlri25))
  proptrtabxlriSens05[i,] = prop.fn(veAOM[i],exp(efftrtabxlri05))
}


save(proplriSens02,file='proplriSens02.Rdata')
save(proplriSens25,file='proplriSens25.Rdata')
save(proplriSens05,file='proplriSens05.Rdata')
save(proptrtlriSens02,file='proptrtlriSens02.Rdata')
save(proptrtlriSens25,file='proptrtlriSens25.Rdata')
save(proptrtlriSens05,file='proptrtlriSens05.Rdata')
save(proptrtabxlriSens02,file='proptrtabxlriSens02.Rdata')
save(proptrtabxlriSens25,file='proptrtabxlriSens25.Rdata')
save(proptrtabxlriSens05,file='proptrtabxlriSens05.Rdata')


