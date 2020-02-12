
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
ivabx = dataset$ivabx
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
fever = dataset$fever
asxFev = fever==0
solidfuel = dataset$solidfuel
san = dataset$san
antimalarial = dataset$antimalarial

trtabxdiar = rep(NA,length(abx))
trtabxdiar[abxdiar==1|ivabxdiar==1] = 1
trtabxdiar[asx==1] = 0

rehyd = rep(NA,length(ors))
rehyd[ors==0] = 0; rehyd[ors>0] = 1; rehyd[iv==1] = 1
rehyd[asx==1] = 0
trtrehyd = rehyd; trtrehyd[trtdiar==1&rehyd==1] = 1; trtrehyd[trtdiar==0|rehyd==0] = 0

sex = ifelse(dataset$sex%in%c('0','FEMALE'),0,ifelse(dataset$sex%in%c('1','MALE'),1,NA))
urban = dataset$urban

chest = dataset$chest; breath = dataset$breath; nose = dataset$nose; cough = dataset$cough
ari = cough==1|breath==1
asxresp = chest==0&cough==0&breath==0&nose==0
lri = chest==1
abx[ivabx==1&is.na(abx)] = 1
#trtabxlri = rep(NA,length(abx))
#trtabxlri[abx==1|ivabx==1] = 1
#trtabxlri[antimalarial==1] = NA

trtabxlri = rep(NA,length(abx))
trtabxlri[(abx==1|ivabx==1)&ari==1] = 1
trtabxlri[ari==0] = NA
trtabxlri[asx==1] = NA

trtresp = dataset$trtresp
trtari = trtlri = rep(NA,length(abx))
trtari[ari==1&trtresp==1] = 1
trtari[asx==1] = NA


table(trtabxlri)

agelb = c(0,0.5,1,2,3)
ageub = c(0.5,1,2,3,5)

rota2 = dataset$rvdoses
rota2[rota2==1] = NA


### match on maternal education
### match on pentavalent

############################
####### All diar ###########
############################

countries = unique(country)

rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){

    dats = which(country==countries[j])
    
    diarA = diar[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    
    ord = sample(which(diarA==1),sum(diarA==1,na.rm=T))
    add = 0
    used = rep(0,length(diarA)); case = m = rep(NA,length(diarA))
    
    for (i in ord){
      mdiar = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i])
      if (length(mdiar)>=2){
        sel = sample(mdiar,2,replace=F)
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
set.seed(1)
for (i in 1:20){
  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  if ((i/20)==round(i/20)){
    diarInputs = out
    save(diarInputs,file='diarInputs.Rdata')
  }
  print(i)
}

############################
####### Trt diar ###########
############################


rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtdiarA = trtdiar[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    
    ord = sample(which(trtdiarA==1),sum(trtdiarA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtdiarA)); case = m = rep(NA,length(trtdiarA))
    
    for (i in ord){
      mdiar = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i])
      if (length(mdiar)>=2){
        sel = sample(mdiar,2,replace=F)
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
    trtdiarInputs = out
    save(trtdiarInputs,file='trtdiarInputs.Rdata')
  }
  print(i)
}

#################################
####### Trtrehyd diar ###########
#################################


rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtrehydA = trtrehyd[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    
    ord = sample(which(trtrehydA==1),sum(trtrehydA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtrehydA)); case = m = rep(NA,length(trtrehydA))
    
    for (i in ord){
      mdiar = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i])
      if (length(mdiar)>=2){
        sel = sample(mdiar,2,replace=F)
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
    trtrehydInputs = out
    save(trtrehydInputs,file='trtrehydInputs.Rdata')
  }
  print(i)
}


############################
####### Abx diar ###########
############################

rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtabxdiarA = trtabxdiar[dats]
    asxA = asx[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    
    ord = sample(which(trtabxdiarA==1),sum(trtabxdiarA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtabxdiarA)); case = m = rep(NA,length(trtabxdiarA))
    
    for (i in ord){
      mdiar = which(asxA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i])
      if (length(mdiar)>=2){
        sel = sample(mdiar,2,replace=F)
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
    trtabxdiarInputs = out
    save(trtabxdiarInputs,file='trtabxdiarInputs.Rdata')
  }
  print(i)
}

########## Neg controls




rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    ariA = ari[dats]
    asxrespA = asxresp[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    pcvdosesA = pcvdoses[dats]#>0
    feverA = fever[dats]
    
    ord = sample(which(ariA==1),sum(ariA==1,na.rm=T))
    add = 0
    used = rep(0,length(ariA)); case = m = rep(NA,length(ariA))
    
    for (i in ord){
      mdiar = which(asxrespA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pcvdosesA==pcvdosesA[i]&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&feverA==feverA[i])
      if (length(mdiar)>=3){
        sel = sample(mdiar,3,replace=F)
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
    diarnegctlrespInputs = out
    save(diarnegctlrespInputs,file='diarnegctlrespInputs.Rdata')
  }
  print(i)
}


rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    ariA = trtari[dats]
    asxrespA = asxresp[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    pcvdosesA = pcvdoses[dats]#>0
    feverA = fever[dats]

    
    ord = sample(which(ariA==1),sum(ariA==1,na.rm=T))
    add = 0
    used = rep(0,length(ariA)); case = m = rep(NA,length(ariA))
    
    for (i in ord){
      mdiar = which(asxrespA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&pcvdosesA==pcvdosesA[i]&pentadosesA==pentadosesA[i]&wealthA==wealthA&urbanA==urbanA[i]&feverA==feverA[i])
      if (length(mdiar)>=3){
        sel = sample(mdiar,3,replace=F)
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
    diarnegctltrtrespInputs = out
    save(diarnegctltrtrespInputs,file='diarnegctltrtrespInputs.Rdata')
  }
  print(i)
}





rep.fn = function(num){
  
  out = list()
  
  for (j in 1:length(countries)){
    
    dats = which(country==countries[j])
    
    trtabxlriA = trtabxlri[dats]
    asxrespA = asxresp[dats]
    ageA = age[dats]
    yearA = year[dats]
    pentadosesA = pentadoses[dats]
    urbanA = urban[dats]
    wealthA = wealth[dats]
    pcvdosesA = pcvdoses[dats]#>0
    feverA = fever[dats]

 
    ord = sample(which(trtabxlriA==1),sum(trtabxlriA==1,na.rm=T))
    add = 0
    used = rep(0,length(trtabxlriA)); case = m = rep(NA,length(trtabxlriA))
    
    for (i in ord){
      mdiar = which(asxrespA&(abs(ageA-ageA[i])<(1/12))&(abs(yearA-yearA[i])<(1/12))&used==0&wealthA==wealthA&urbanA==urbanA[i]&pcvdosesA==pcvdosesA[i]&pentadosesA==pentadosesA[i]&feverA==feverA[i])
      if (length(mdiar)>=2){
        sel = sample(mdiar,2,replace=F)
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



set.seed(2)
num = 1:20
out = list()
for (i in 1:100){

  out[[i]] = mclapply(num, function(num) rep.fn(num),mc.cores=20)
  
  if ((i/20)==round(i/20)){
    diarnegctlabxrespInputs = out
    save(diarnegctlabxrespInputs,file='diarnegctlabxrespInputs.Rdata')
  }
  print(i)
}


