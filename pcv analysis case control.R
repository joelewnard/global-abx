
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
antimalarial = dataset$antimalarial
trtresp[antimalarial==1] = NA
trtlri = trtresp
trtlri[lri==0] = 0
pcvdoses = dataset$pcvdoses
abx[ivabx==1&is.na(abx)] = 1



trtabxlri = rep(NA,length(abx))
trtabxlri[abx==1|ivabx==1] = 1
trtabxlri[lri==0] = NA
trtabxlri[asx==1] = NA
trtabxlri[antimalarial==1] = NA

ari = cough==1|breath==1
trtari = ari&trtresp
trtabxari = rep(NA,length(abx))
trtabxari = ari&abx

trtabxdiar = rep(NA,length(abx))
trtabxdiar[abxdiar==1|ivabxdiar==1] = 1

trtdiar = rep(NA,length(abx))
trtdiar[dataset$trtdiar==1] = 1

fever = dataset$fever
sex = ifelse(dataset$sex%in%c('0','FEMALE'),0,ifelse(dataset$sex%in%c('1','MALE'),1,NA))
urban = dataset$urban

pcv3 = pcvdoses; pcv3[pcvdoses%in%c(1,2)] = NA

diar = dataset$diar
asxdiar = diar==0

#####################################
###### all LRI
#####################################

countries = unique(country)

rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    dats = which(country==countries[j])
    
    lriA = lri[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    
    ord = sample(which(lriA==1),sum(lriA==1,na.rm=T))
    add = 0
    used = rep(0,length(lriA)); case = m = rep(NA,length(lriA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&feverA==feverA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}

library(parallel)

set.seed(1)
num = 1:20
out = list()
for (i in 1:20){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    lriInputs = out
    save(lriInputs,file='lriInputs.Rdata')
  }
  print(i)
}

#####################################
###### trt LRI
#####################################


rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    dats = which(country==countries[j])
    
    trtlriA = trtlri[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    
    ord = sample(which(trtlriA==1),sum(trtlriA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtlriA)); case = m = rep(NA,length(trtlriA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&feverA==feverA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}

set.seed(1)
num = 1:20
out = list()
for (i in 1:20){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    trtlriInputs = out
    save(trtlriInputs,file='trtlriInputs.Rdata')
  }
  print(i)
}

#####################################
###### abx trt LRI ##################
#####################################
rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    dats = which(country==countries[j])
    
    trtabxlriA = trtabxlri[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    
    ord = sample(which(trtabxlriA==1),sum(trtabxlriA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtabxlriA)); case = m = rep(NA,length(trtabxlriA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&feverA==feverA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}

set.seed(10)
num = 1:20
out = list()
for (i in 1:20){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    trtabxlriInputs = out
    save(trtabxlriInputs,file='trtabxlriInputs.Rdata')
  }
  print(i)
}




rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){

    dats = which(country==countries[j])
    
    diarA = diar[dats]
    asxA = asxdiar[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    rvdosesA = rvdoses[dats]
    
    ord = sample(which(diarA==1),sum(diarA==1,na.rm=T))
    add = 0
    used = rep(0,length(diarA)); case = m = rep(NA,length(diarA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&rvdosesA==rvdosesA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}



set.seed(1)
num = 1:20
out = list()
for (i in 1:100){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    lrinegctldiarInputs = out
    save(lrinegctldiarInputs,file='lrinegctldiarInputs.Rdata')
  }
  print(i)
}



rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtdiarA = trtdiar[dats]
    asxA = asxdiar[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    rvdosesA = rvdoses[dats]
    
    ord = sample(which(trtdiarA==1),sum(trtdiarA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtdiarA)); case = m = rep(NA,length(trtdiarA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&rvdosesA==rvdosesA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}


set.seed(1)
num = 1:20
out = list()
for (i in 1:100){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    lrinegctltrtdiarInputs = out
    save(lrinegctltrtdiarInputs,file='lrinegctltrtdiarInputs.Rdata')
  }
  print(i)
}




rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtabxdiarA = trtabxdiar[dats]
    asxA = asxdiar[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    feverA = fever[dats]
    rvdosesA = rvdoses[dats]
    
    ord = sample(which(trtabxdiarA==1),sum(trtabxdiarA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtabxdiarA)); case = m = rep(NA,length(trtabxdiarA))
    
    for (i in ord){
      mchest = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&rvdosesA==rvdosesA[i])
      if (length(mchest)>=3){
        sel = sample(mchest,3,replace=F)
        case[i] = 1
        case[sel] = 0
        m[i] = m[sel] = i
        used[i] = used[sel] = 1
      }
    }
    m = m + j*1e6
    out[[j]] = cbind(case,m)
  }
  output = c()
  for (j in 1:length(countries)){
    output = rbind(output,out[[j]])
  }
  return(output)
}


set.seed(1)
num = 1:20
out = list()
for (i in 1:100){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    lrinegctlabxdiarInputs = out
    save(lrinegctlabxdiarInputs,file='lrinegctlabxdiarInputs.Rdata')
  }
  print(i)
}

