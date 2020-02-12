
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('efflri25.Rdata')
load('efftrtabxlri25.Rdata')
load('proptrtabxlri25.Rdata')
load('proplri25.Rdata')
load('proptrtabxlriSens25.Rdata')
load('proplriSens25.Rdata')


load('efflri05.Rdata')
load('efftrtabxlri05.Rdata')
load('proptrtabxlri05.Rdata')
load('proplri05.Rdata')
load('proptrtabxlriSens05.Rdata')
load('proplriSens05.Rdata')

load('veIPD.Rdata')
load('veAOM.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
load('effdiar02.Rdata')
load('efftrtdiar02.Rdata')
load('efftrtabxdiar02.Rdata')
load('proptrtabxdiar02ic.Rdata')
load('propdiar02ic.Rdata')
load('velic.Rdata'); load('vemic.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('diarInc.Rdata'); load('diarAbx.Rdata'); load('lriInc.Rdata'); load('lriAbx.Rdata'); load('lriInc059.Rdata'); load('lriAbx059.Rdata')
load('diarIncLB.Rdata'); load('lriIncLB.Rdata'); load('diarIncMed.Rdata'); load('lriIncMed.Rdata'); load('lriInc059LB.Rdata'); load('lri059IncMed.Rdata')




load('imputedExtend.Rdata')
load('final.dat.Rdata')
countries = unique(final.dat$country)

other = read.csv('wb data updated.csv',header=T)

other = other[other[,1]=='2014',]

countries = unique(final.dat$country)

##### rename countries to match existing
country = as.character(other$Country.Name)

country[country=='Bahamas, The'] = 'Bahamas'
country[country=='Congo, Dem. Rep.'] = 'Congo Democratic Republic'
country[country=='Congo, Rep.'] = 'Congo'
country[country=='Egypt, Arab Rep.'] = 'Egypt'
country[country=='Gambia, The'] = 'Gambia'
country[country=='Iran, Islamic Rep.'] = 'Iran'
country[country=="Korea, Dem. People's Rep."] = 'DPR Korea'
country[country=='Korea, Rep.'] = 'Korea'
country[country=='Micronesia, Fed. Sts.'] = 'Micronesia'
country[country=='Venezuela, RB'] = 'Venezuela'
country[country=='Yemen, Rep.'] = 'Yemen'
country[country=='West Bank and Gaza'] = 'Palestine'
country[country=='Guinea-Bissau'] = 'Guinea Bissau'



library(maptools)
data(wrld_simpl)
mapcountries = as.character(wrld_simpl$NAME)


countries[which((countries%in%mapcountries)==F)]
mapcountries[mapcountries=='Democratic Republic of the Congo'] = 'Congo Democratic Republic'
mapcountries[mapcountries=='Swaziland'] = 'Eswatini'
mapcountries[mapcountries=='Kyrgyzstan'] = 'Kyrgyz Republic'
mapcountries[mapcountries=='Burma'] = 'Myanmar'
mapcountries[mapcountries=='United Republic of Tanzania'] = 'Tanzania'
mapcountries[mapcountries=="Lao People's Democratic Republic"] = 'Lao PDR'
mapcountries[mapcountries=='Guinea-Bissau'] = 'Guinea Bissau'
mapcountries[mapcountries=='Viet Nam'] = 'Vietnam'

countries[((countries%in%country)==F)]

mapcountries[(mapcountries%in%country)==F]
mapcountries[mapcountries=='Cape Verde'] = 'Cabo Verde'
mapcountries[mapcountries=='Micronesia, Federated States of'] = 'Micronesia'
mapcountries[mapcountries=='Iran (Islamic Republic of)'] = 'Iran'
mapcountries[mapcountries=='Libyan Arab Jamahiriya'] = 'Libya'
mapcountries[mapcountries=='The former Yugoslav Republic of Macedonia'] = 'North Macedonia'
mapcountries[mapcountries=='Republic of Moldova'] = 'Moldova'
mapcountries[mapcountries=='Russia'] = 'Russian Federation'
mapcountries[mapcountries=='Saint Lucia'] = 'St. Lucia'
mapcountries[mapcountries=='Saint Vincent and the Grenadines'] = 'St. Vincent and the Grenadines'


othercountries = unique(country)
ic = rep(NA,length(othercountries))
ic[othercountries%in%c('Angola','Bangladesh','Bhutan','Bolivia','Cabo Verde','Cambodia','Cameroon','Congo',"Cote d'Ivoire",'Djibouti',
                'Egypt','El Salvador','Eswatini','Georgia','Ghana','Honduras','India','Indonesia','Kenya','Kiribati','Kosovo',
                'Kyrgyz Republic','Lao PDR','Lesotho','Mauritania','Micronesia','Moldova','Mongolia','Morocco','Myanmar','Nicaragua',
                'Nigeria','Pakistan','Papua New Guinea','Philippines','Sao Tome and Principe','Solomon Islands','Sri Lanka','Sudan','Timor-Leste',
                'Tunisia','Ukraine','Uzbekistan','Vanuatu','Vietnam','Palestine','Zambia')] = 'lmic'
ic[othercountries%in%c('Albania','Algeria','American Samoa','Armenia','Azerbaijan','Belarus','Belize','Bosnia and Herzegovina',
                'Botswana','Brazil','Bulgaria','China','Colombia','Costa Rica','Cuba','Dominica','Dominican Republic',
                'Ecuador','Equatorial Guinea','Fiji','Gabon','Grenada','Guatemala','Guyana','Iran','Iraq',
                'Jamaica','Jordan','Kazakhstan','Lebanon','Libya','Malaysia','Maldives','Marshall Islands','Mauritius',
                'Mexico','Montenegro','Namibia','Nauru','North Macedonia','Paraguay','Peru','Romania',
                'Russian Federation','Samoa','Serbia','South Africa','St. Lucia','St. Vincent and the Grenadines','Suriname',
                'Thailand','Tonga','Turkey','Turkmenistan','Tuvalu','Venezuela')] = 'umic'
ic[othercountries%in%c('Afghanistan','Benin','Burkina Faso','Burundi','Central African Republic','Chad','Comoros','Congo Democratic Republic','Congo',
                'Eritrea','Ethiopia','Gambia','Guinea','Guinea Bissau','Haiti','DPR Korea','Liberia',
                'Madagascar','Malawi','Mali','Mozambique','Nepal','Niger','Rwanda','Senegal','Sierra Leone','Somalia',
                'South Sudan','Syria','Tajikistan','Tanzania','Togo','Uganda','Yemen','Zimbabwe')] = 'lic'


##### multiple imputation to generate 5 datasets of country values


library(Amelia)

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
other = read.csv('wb data updated.csv',header=T)
othernames = other$Country.Name
year14 = other[,1]=='2014'
othernames = othernames[year14]

#other = other[other[,1]=='2014',]              
for (i in 1:dim(other)[2]){
  other[,i] = as.character(other[,i])
}
other[other=='..'] = NA
other = other[,c(1,3:dim(other)[2])]

rm = c()
for (i in c(1:dim(other)[2])){
    other[,i] = as.numeric(other[,i]) 
  rm[i] = sum(is.na(other[,i])==F)>100
}
other = other[,rm==1]
#dim(other); 


library(parallel)
library(mlbench)
set.seed(1); imputedX = amelia(other,m=5,parallel='multicore',ncpus=5,p2s=2,empri=0.1*dim(other)[1],tolerance=1e-4)

save(imputedX,file='imputedX.Rdata')
load('imputedX.Rdata')


imputed = array(NA,dim=c(5,sum(year14,na.rm=T),323))
imputed[1,,1:322] = as.matrix(imputedX$imputations$imp1[which(year14==1),2:323])
imputed[2,,1:322] = as.matrix(imputedX$imputations$imp2[which(year14==1),2:323])
imputed[3,,1:322] = as.matrix(imputedX$imputations$imp3[which(year14==1),2:323])
imputed[4,,1:322] = as.matrix(imputedX$imputations$imp4[which(year14==1),2:323])
imputed[5,,1:322] = as.matrix(imputedX$imputations$imp5[which(year14==1),2:323])


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
gdp = read.csv('gdpdat.csv',header=T)
gdpcountry = gdp[,1]; gdpvals = gdp[,2]
gdpvals[gdpvals=='..'] = NA
gdpcountry = gdpcountry[1:218]; gdpvals = gdpvals[1:218]

othernames = as.character(othernames); gdpcountry = as.character(gdpcountry)

gdpinput = rep(NA,217)
for (i in 1:217){
  if (othernames[i]%in%gdpcountry){
    gdpinput[i] = as.numeric(as.character(gdpvals[which(gdpcountry==othernames[i])[1]]))
  }
}

gdpinput[which(othernames=='Central African Republic')] = 418.41
gdpinput[which(othernames=='Djibouti')] = 1927.59
gdpinput[which(othernames=='Eritrea')] = 582.78
gdpinput[which(othernames=='Korea, Dem. People’s Rep.')] = 1700
gdpinput[which(othernames=='Somalia')] = 499.82
gdpinput[which(othernames=='South Sudan')] = 237.44
gdpinput[which(othernames=='Syrian Arab Republic')] = 2900

for (i in 1:5){imputed[i,,323] = log(gdpinput)}

imputed = imputed[,ic%in%c('lic','umic','lmic'),]
country = othercountries[ic%in%c('lic','umic','lmic')]
#checkcountry = othernames[ic%in%c('lic','umic','lmic')]

incl = which((country%in%countries)&country!='Sudan')
excl = which((country%in%countries)==F|country=='Sudan')
inclCountries = country[incl]; exclCountries = country[excl]
imputedObs = imputed[,incl,]
imputedExt = imputed[,excl,]

##### reduce dimensions to LIC/LMIC/UMIC countries

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('diarInc.Rdata'); load('diarAbx.Rdata'); load('lriInc.Rdata'); load('lriAbx.Rdata'); load('lriInc059.Rdata'); load('lriAbx059.Rdata')
load('diarIncLB.Rdata'); load('lriIncLB.Rdata'); load('diarIncMed.Rdata'); load('lriIncMed.Rdata'); load('lriInc059LB.Rdata'); load('lri059IncMed.Rdata')

ydiarInc = ydiarIncLB = ydiarIncMed = ydiarAbx = ylriInc = ylriIncLB = ylriIncMed = ylriAbx = ylriInc059 = ylriInc059LB = ylriInc059Med = ylriAbx059 = array(NA,dim=c(5e3,length(country)))
for (i in 1:length(country)){
  if (country[i]%in%inclCountries){
    ydiarInc[,i] = c(diarInc[,,which(countries==country[i])])
    ydiarIncLB[,i] = c(diarIncLB[,,which(countries==country[i])])
    ydiarIncMed[,i] = c(diarIncMed[,,which(countries==country[i])])
    
    ydiarAbx[,i] = c(diarAbx[,,which(countries==country[i])])
    
    ylriInc[,i] = c(lriInc[,,which(countries==country[i])])
    ylriIncLB[,i] = c(lriIncLB[,,which(countries==country[i])])
    ylriIncMed[,i] = c(lriIncMed[,,which(countries==country[i])])
    
    ylriAbx[,i] = c(lriAbx[,,which(countries==country[i])])

    ylriInc059[,i] = c(lriInc059[,,which(countries==country[i])])
    ylriInc059LB[,i] = c(lriInc059LB[,,which(countries==country[i])])
    ylriInc059Med[,i] = c(lri059IncMed[,,which(countries==country[i])])
    
    ylriAbx059[,i] = c(lriAbx059[,,which(countries==country[i])])
  }
}

ydiarIncObs = ydiarInc[,which(country%in%inclCountries)]
ydiarIncLBObs = ydiarIncLB[,which(country%in%inclCountries)]
ydiarIncMedObs = ydiarIncMed[,which(country%in%inclCountries)]
ydiarAbxObs = ydiarAbx[,which(country%in%inclCountries)]

ylriIncObs = ylriInc[,which(country%in%inclCountries)]
ylriIncLBObs = ylriIncLB[,which(country%in%inclCountries)]
ylriIncMedObs = ylriIncMed[,which(country%in%inclCountries)]
ylriAbxObs = ylriAbx[,which(country%in%inclCountries)]

ylriInc059Obs = ylriInc059[,which(country%in%inclCountries)]
ylriInc059LBObs = ylriInc059LB[,which(country%in%inclCountries)]
ylriInc059MedObs = ylriInc059Med[,which(country%in%inclCountries)]
ylriAbx059Obs = ylriAbx059[,which(country%in%inclCountries)]


colnames = names(data)

set.seed(1)

inputs = list(ydiarIncMedObs,ylriIncMedObs,ylriInc059MedObs,ydiarIncMedObs*ydiarAbxObs,ylriIncMedObs*ylriAbxObs,ylriInc059MedObs*ylriAbx059Obs)

mlPred = function(imps,inputs,imputedObs,imputedExt,nit,ind){
  
  predTest = trueTest = array(NA,dim=c(nit,length(inputs),5))
  predOut = array(NA,dim=c(nit,length(inputs),58))
  
  for (i in 1:nit){
    for (j in 1:length(inputs)){
      data = data.frame(y=inputs[[j]][ind[i],],x=imputedObs[imps,,])
      inTrain = createDataPartition(y=data$y,
                                    p=0.9,
                                    list=F)
      training = data[inTrain,]
      testing = data[-inTrain,]
    
      plsFit = train(x=training[,2:324],y=log(training[,1]),
                     method='gbm',verbose=F)
      trial = predict(plsFit,testing)
      if (length(trial)==length(predTest[i,j,])){
        predTest[i,j,] = trial
        trueTest[i,j,] = log(testing$y) 
      }
      
      outData = data.frame(x=imputedExt[imps,,])
      trial = predict(plsFit,outData)
      if (length(trial)==length(predOut[i,j,])){
        predOut[i,j,] = trial  
      }
      
      print(c(i,j))
    }
  }
  return(list(predTest,trueTest,predOut))
}

library(caret)
library(MASS)
library(parallel)
library(gbm)


#set.seed(1)
set.seed(101) ### added for iteration started at i=101
nrep = 200; imps = 1:5; nit = 5
outofsampA = outofsamptestA = array(NA,dim=c(length(imps),nrep*nit,length(inputs),5))
predA = array(NA,dim=c(length(imps),nrep*nit,length(inputs),length(exclCountries)))
#load(file='outofsamp.Rdata'); load('outofsamptest.Rdata'); load('pred.Rdata')

for (i in 101:nrep){
  ## i = 200; added for last iteration (loop broken at i=200)
  print(c(i,'start')); a = as.numeric(Sys.time())
  out = mclapply(imps, function(imps) mlPred(imps,
                                       inputs=inputs,
                                       imputedObs=imputedObs,
                                       imputedExt=imputedExt,
                                       nit=nit,
                                       ind=((i-1)*nit+1:nit)),
                 mc.cores=5)
  print(c(i,'stop',as.numeric(Sys.time())-a))
  
  for (m in 1:5){
      outofsampA[m,(i-1)*nit+1:nit,,] = out[[m]][[1]]
      outofsamptestA[m,(i-1)*nit+1:nit,,] = out[[m]][[2]]
      predA[m,(i-1)*nit+1:nit,,] = out[[m]][[3]]
  }
  print(i)
  save(outofsampA,file='outofsampA.Rdata')
  save(outofsamptestA,file='outofsamptestA.Rdata')
  save(predA,file='predA.Rdata')
  print(c(i,'saved'))
}


load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsamp.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsamptest.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/pred.Rdata")

load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsampA.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsamptestA.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/predA.Rdata")

outofsampFinal = array(NA,dim=dim(outofsamp))
outofsampFinal[,1:500,,] = outofsamp[,1:500,,]
outofsampFinal[,501:1000,,] = outofsampA[,501:1000,,]

outofsamptestFinal = array(NA,dim=dim(outofsamptest))
outofsamptestFinal[,1:500,,] = outofsamptest[,1:500,,]
outofsamptestFinal[,501:1000,,] = outofsamptestA[,501:1000,,]

predFinal = array(NA,dim=dim(pred))
predFinal[,1:500,,] = pred[,1:500,,]
predFinal[,501:1000,,] = predA[,501:1000,,]

save(predFinal,file='predFinal.Rdata'); save(outofsampFinal,file='outofsampFinal.Rdata'); save(outofsamptestFinal,file='outofsamptestFinal.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(country,file='country.Rdata'); save(inclCountries,file='inclCountries.Rdata'); save(exclCountries,file='exclCountries.Rdata')

length(inclCountries)


country=unique(country) ### 
country = sort(country)


estDiarInc = estDiarIncLB = estDiarAbx = estLriInc = estLriIncLB = estLriAbx = array(NA,dim=c(1e3,5,length(country)))
for (i in 1:length(country)){
  if (country[i]%in%countries){
    estDiarInc[,,i] = diarInc[,,which(countries==country[i])]
    estDiarIncLB[,,i] = diarIncLB[,,which(countries==country[i])]
    estDiarAbx[,,i] = diarAbx[,,which(countries==country[i])]
    estLriInc[,,i] = lriInc[,,which(countries==country[i])]
    estLriIncLB[,,i] = lriIncLB[,,which(countries==country[i])]
    estLriAbx[,,i] = lriAbx[,,which(countries==country[i])]
  } else{
    if (country[i]%in%exclCountries){
      for (j in 1:4){
        estDiarInc[,j,i] = exp(pred[j,,1,which(exclCountries==country[i])])
        estDiarAbx[,j,i] = exp(pred[j,,2,which(exclCountries==country[i])])
        estLriInc[,j,i] = exp(pred[j,,3,which(exclCountries==country[i])])
        estLriAbx[,j,i] = exp(pred[j,,4,which(exclCountries==country[i])])
        
        estDiarIncLB[,j,i] = exp(predLB[j,,1,which(exclCountries==country[i])])
        estLriIncLB[,j,i] = exp(predLB[j,,2,which(exclCountries==country[i])])
      }
    }
  }
}



countryEsts = array(NA,dim=c(length(country),4)) ### ests for diarInc, % diarAbx, lriInc, % lriAbx
countryEstsLB = array(NA,dim=c(length(country),2))
## projected or observed
for (i in 1:length(country)){
  countryEsts[i,1] = median(estDiarInc[,,i],na.rm=T)
  countryEsts[i,2] = median(estDiarAbx[,,i],na.rm=T)
  countryEsts[i,3] = median(estLriInc[,,i],na.rm=T)
  countryEsts[i,4] = median(estLriAbx[,,i],na.rm=T)
  
  countryEstsLB[i,1] = median(estDiarIncLB[,,i],na.rm=T)
  countryEstsLB[i,2] = median(estLriIncLB[,,i],na.rm=T)
}

totDiar = totLri = totDiarAbx = totLriAbx = array(NA,dim=c(5e3,length(country)))
totDiarLB = totLriLB = totDiarAbxLB = totLriAbxLB = array(NA,dim=c(5e3,length(country)))
spAbx = rotaAbx = pcvAvertAbx = rvAvertAbx = array(NA,dim=dim(totDiar))
spAbxLB = rotaAbxLB = pcvAvertAbxLB = rvAvertAbxLB = array(NA,dim=dim(totDiar))
for (i in 1:length(country)){
  totDiar[,i] = c(estDiarInc[,,i])
  totLri[,i] = c(estLriInc[,,i])
  totDiarAbx[,i] = c(estDiarInc[,,i]*estDiarAbx[,,i])
  totLriAbx[,i] = c(estLriInc[,,i]*estLriAbx[,,i])
  
  totDiarLB[,i] = c(estDiarIncLB[,,i])
  totLriLB[,i] = c(estLriIncLB[,,i])
  totDiarAbxLB[,i] = c(estDiarIncLB[,,i]*estDiarAbx[,,i])
  totLriAbxLB[,i] = c(estLriIncLB[,,i]*estLriAbx[,,i])
}

### rv implemented, pcv implemented, ic status, region

incomeStat = rvImp = pcvImp = region = c()
for (i in 1:length(country)){
  incomeStat[i] = c(ic[which(othercountries==country[i])],NA)[1]
}
incomeStat[which(country=='Central African Republic')] = 'lic'
incomeStat[which(country=='Russian Federation')] = 'umic'
incomeStat[which(country=='Guinea Bissau')] = 'lic'

region[country%in%c('Algeria','Angola','Benin','Botswana','Burkina Faso','Burundi','Cameroon','Cabo Verde',
                    'Central African Republic','Chad','Comoros',"Cote d'Ivoire",'Congo Democratic Republic',
                    'Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea Bissau',
                    'Kenya','Lesotho','Liberia','Madagascar','Malawi','Mali','Mauritania','Mauritius','Mozambique',
                    'Namibia','Niger','Nigeria','Congo','Rwanda','Sao Tome and Principe','Senegal','Seychelles',
                    'Sierra Leone','South Africa','Eswatini','Togo','Uganda','Tanzania','Zambia','Zimbabwe',
                    'South Sudan')] = 'AFR'
region[country%in%c('Belize','Bolivia','Brazil','Colombia','Costa Rica','Cuba','Dominica','Dominican Republic',
                    'Ecuador','El Salvador','Grenada','Guatemala','Guyana','Haiti','Honduras','Jamaica','Mexico','Nicaragua',
                    'Paraguay','Peru','Trinidad and Tobago','Venezuela','St. Lucia','St. Vincent and the Grenadines',
                    'Suriname')] = 'AMR'
region[country%in%c('Bangladesh','Bhutan','India','Indonesia','Maldives','Myanmar','Nepal','Sri Lanka','Thailand',
                    'Timor-Leste')] = 'SEAR'
region[country%in%c('Albania','Armenia','Azerbaijan','Belarus','Bosnia and Herzegovina','Bulgaria','Kazakhstan',
                    'Kyrgyz Republic','Montenegro','North Macedonia','Moldova','Romania','Serbia','Tajikistan',
                    'Turkey','Turkmenistan','Ukraine','Uzbekistan','Georgia','Kosovo','Russian Federation')] = 'EUR'
region[country%in%c('Afghanistan','Djibouti','Egypt','Iran','Iraq','Jordan','Kuwait','Lebanon','Libya',
                    'Morocco','Pakistan','Somalia','Sudan','Syria','Tunisia','Yemen','Palestine')] = 'EMR'
region[country%in%c('Cambodia','China','Fiji','Lao PDR','Malaysia','Micronesia','Nauru','Papua New Guinea','Philippines',
                    'Samoa','Tonga','Tuvalu','Vanuatu','Vietnam','American Samoa','Kiribati','Marshall Islands',
                    'Mongolia','Solomon Islands')] = 'WPR'

rvImplemented = pcvImplemented = rep(1,length(country))
pcvImplemented[country%in%c('Algeria','Antigua and Barbuda','Belize','Bhutan','Bosnia and Herzegovina',
                            'Cabo Verde','Chad','China','Comoros','Cuba','Dominica','Egypt','Equatorial Guinea',
                            'Gabon','Grenada','Guinea','Iran','Jordan','North Macedonia','Malaysia','Maldives','Montenegro',
                            'Nauru','Romania','St. Lucia','St. Vincent and the Grenadines','Samoa','Serbia',
                            'Somalia','South Sudan','Sri Lanka','Suriname','Syria','Tajikistan','Thailand','Timor-Leste',
                            'Tonga','Tunisia','Turkmenistan','Tuvalu','Ukraine','Vanuatu','Vietnam')] = 0
rvImplemented[country%in%c('Albania','Algeria','Azerbaijan','Bangladesh',"Barbados",'Belarus','Belize','Benin',
                           'Bhutan','Bosnia and Herzegovina','Cambodia','Cabo Verde','Central African Republic',
                           'Chad','China','Comoros','Congo Democratic Republic','Costa Rica','Cuba','Dominica',
                           'Egypt','Equatorial Guinea','Gabon','Grenada','Guinea','Indonesia','Iran','Jamaica',
                           'Kazakhstan','Kyrgyz Republic','Lao PDR','Lebanon','North Macedonia','Malaysia','Maldives',
                           'Mongolia','Montenegro','Myanmar','Nauru','Nepal','Nigeria','Niue','Papua New Guinea',
                           'Romania','St. Lucia','St. Vincent and the Grenadines','Samoa','Serbia','Solomon Islands',
                           'Somalia','South Sudan','Sri Lanka','Suriname','Syria','Timor-Leste','Tonga',
                           'Trinidad and Tobago','Tunisia','Turkey','Turkmenistan','Tuvalu','Ukraine',
                           'Vanuatu','Vietnam','North Macedonia','Russian Federation')] = 0
### fill in gaps: sudan, central af rep

for (j in which(country%in%c('Sudan','Central African Republic','Russian Federation'))){
  for (i in 1:4e3){
    totDiar[i,j] = mean(totDiar[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totLri[i,j] = mean(totLri[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totDiarAbx[i,j] = mean(totDiarAbx[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totLriAbx[i,j] = mean(totLriAbx[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    
    totDiarLB[i,j] = mean(totDiarLB[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totLriLB[i,j] = mean(totLriLB[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totDiarAbxLB[i,j] = mean(totDiarAbxLB[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
    totLriAbxLB[i,j] = mean(totLriAbxLB[i,which(region==region[j]&incomeStat==incomeStat[j])],na.rm=T)
  }
}

save(incomeStat,file='incomeStat.Rdata')
save(region,file='region.Rdata'); save(pcvImplemented,file='pcvImplemented.Rdata')
save(rvImplemented,file='rvImplemented.Rdata'); save(totDiar,file='totDiar.Rdata')
save(totLri,file='totLri.Rdata'); save(totDiarAbx,file='totDiarAbx.Rdata'); save(totLriAbx,file='totLriAbx.Rdata')
save(totDiarLB,file='totDiarLB.Rdata')
save(totLriLB,file='totLriLB.Rdata'); save(totDiarAbxLB,file='totDiarAbxLB.Rdata'); save(totLriAbxLB,file='totLriAbxLB.Rdata')

load('totSpAbx.Rdata'); load('totRotaAbx.Rdata')
load('totDiar.Rdata'); load('totLri.Rdata')
load('totDiarAbx.Rdata'); load('totLriAbx.Rdata')
### VT-Sp related Abx consump
### PCV-avertible abx consump

set.seed(1)
propAbxSp = sample(proptrtabxlri25,5e3)
propAbxRotaMIC = sample(proptrtabxdiar02ic[,,2],5e3)
propAbxRotaLIC = sample(proptrtabxdiar02ic[,,3],5e3)
veAbxSp = 1-exp(sample(efftrtabxlri25,5e3,replace=T))
veAbxRvMIC = 1-exp(sample(efftrtabxdiar02[,2],5e3,replace=T))
veAbxRvLIC = 1-exp(sample(efftrtabxdiar02[,3],5e3,replace=T))

propAbxSpSens = sample(proptrtabxlriSens25,5e3)


totSpAbx = totPcvAvertAbx = totRotaAbx = totRvAvertAbx = array(NA,dim=dim(totDiar))
totSpAbxLB = totPcvAvertAbxLB = totRotaAbxLB = totRvAvertAbxLB = array(NA,dim=dim(totDiar))
totSpAbxSensLB = totPcvAvertAbxSensLB = array(NA,dim=dim(totDiar))
for (i in 1:length(country)){
  totSpAbx[,i] = totLriAbx[,i]*propAbxSp
  totPcvAvertAbx[,i] = totLriAbx[,i]*veAbxSp
  
  totSpAbxLB[,i] = totLriAbxLB[,i]*propAbxSp
  totPcvAvertAbxLB[,i] = totLriAbxLB[,i]*veAbxSp
  
  totSpAbxSensLB[,i] = totLriAbxLB[,i]*propAbxSpSens
  totPcvAvertAbxSensLB[,i] = totLriAbxLB[,i]*veAbxSp
  if (incomeStat[i]=='lic'){
    totRotaAbx[,i] = totDiarAbx[,i]*propAbxRotaLIC
    totRvAvertAbx[,i] = totDiarAbx[,i]*veAbxRvLIC
    
    totRotaAbxLB[,i] = totDiarAbxLB[,i]*propAbxRotaLIC
    totRvAvertAbxLB[,i] = totDiarAbxLB[,i]*veAbxRvLIC
  } else{
    totRotaAbx[,i] = totDiarAbx[,i]*propAbxRotaMIC
    totRvAvertAbx[,i] = totDiarAbx[,i]*veAbxRvMIC
    
    totRotaAbxLB[,i] = totDiarAbxLB[,i]*propAbxRotaMIC
    totRvAvertAbxLB[,i] = totDiarAbxLB[,i]*veAbxRvMIC
  }
}


save(totSpAbx,file='totSpAbx.Rdata'); save(totPcvAvertAbx,file='totPcvAvertAbx.Rdata')
save(totRotaAbx,file='totRotaAbx.Rdata'); save(totRvAvertAbx,file='totRvAvertAbx.Rdata')

save(totSpAbxLB,file='totSpAbxLB.Rdata'); save(totPcvAvertAbxLB,file='totPcvAvertAbxLB.Rdata')
save(totRotaAbxLB,file='totRotaAbxLB.Rdata'); save(totRvAvertAbxLB,file='totRvAvertAbxLB.Rdata')
#save(country,file='country.Rdata')

pops = read.csv('countrypops.csv',header=T)
pop = c(); for (i in 1:length(country)){
  if (country[i]%in%pops$name){
    pop[i] = pops$y2016[which(pops$name==country[i])]
  }
}
country[is.na(pop)]
pop[country=='Congo'] = pops$y2016[which(pops$name=='Congo, Rep.')]
pop[country=='Congo Democratic Republic'] = pops$y2016[which(pops$name=='Congo, Dem. Rep.')]
pop[country=='Egypt'] = pops$y2016[which(pops$name=='Egypt, Arab Rep.')]
pop[country=='Eritrea'] = 4.475e6
pop[country=='Gambia'] = pops$y2016[which(pops$name=='Gambia, The')]
pop[country=='Iran'] = pops$y2016[which(pops$name=='Iran, Islamic Rep.')]
pop[country=='Micronesia'] = pops$y2016[which(pops$name=='Micronesia, Fed. Sts.')]
pop[country=='Palestine'] = pops$y2016[which(pops$name=='West Bank and Gaza')]
pop[country=='Venezuela'] = pops$y2016[which(pops$name=='Venezuela, RB')]
pop[country=='Yemen'] = pops$y2016[which(pops$name=='Yemen, Rep.')]
pop[country=='Guinea Bissau'] = pops$y2016[which(pops$name=='Guinea-Bissau')]

popAge = read.csv('popAge.csv',header=T)
pop01 = pop24 = rep(NA,length(country))
countrynames = as.character(popAge$name)
countrynames[countrynames=='Bolivia (Plurinational State of)'] = 'Bolivia'
countrynames[countrynames=='Democratic Republic of the Congo'] = 'Congo Democratic Republic'
countrynames[countrynames=="Côte d'Ivoire"] = "Cote d'Ivoire"
countrynames[countrynames=='Swaziland'] = 'Eswatini'
countrynames[countrynames=='Iran (Islamic Republic of)'] = 'Iran'
countrynames[countrynames=='Kyrgyzstan'] = 'Kyrgyz Republic'
countrynames[countrynames=="Lao People's Democratic Republic"] = 'Lao PDR'
countrynames[countrynames=='Republic of Moldova'] = 'Moldova'
countrynames[countrynames=='TFYR Macedonia'] = 'North Macedonia'
countrynames[countrynames=='State of Palestine'] = 'Palestine'
countrynames[countrynames=='Saint Lucia'] = 'St. Lucia'
countrynames[countrynames=='Saint Vincent and the Grenadines'] = 'St. Vincent and the Grenadines'
countrynames[countrynames=='United Republic of Tanzania'] = 'Tanzania'
countrynames[countrynames=='Venezuela (Bolivarian Republic of)'] = 'Venezuela'
countrynames[countrynames=='Viet Nam'] = 'Vietnam'
countrynames[countrynames=='Guinea-Bissau'] = 'Guinea Bissau'
country[(country%in%countrynames)==F]
for (i in 1:length(country)){
  if (country[i]%in%countrynames){
    pop01[i] = 1e3*(popAge$age0+popAge$age1)[which(countrynames==country[i])]
    pop24[i] = 1e3*(popAge$age2+popAge$age3+popAge$age4)[which(countrynames==country[i])]
  }
}
for (i in which(is.na(pop01))){
  sel = which(incomeStat==incomeStat[i]&region==region[i])
  pop01[i] = mean(pop01[sel]/pop[sel],na.rm=T)*pop[i]
  pop24[i] = mean(pop24[sel]/pop[sel],na.rm=T)*pop[i]
}

save(pop,file='pop.Rdata'); save(pop01,file='pop01.Rdata'); save(pop24,file='pop24.Rdata')
