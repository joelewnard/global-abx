setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')


setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('diarInc.Rdata'); load('diarAbx.Rdata'); load('lriInc.Rdata'); load('lriAbx.Rdata'); load('lriInc059.Rdata'); load('lriAbx059.Rdata')
load('diarIncLB.Rdata'); load('lriIncLB.Rdata'); load('diarIncMed.Rdata'); load('lriIncMed.Rdata'); load('lriInc059LB.Rdata'); load('lri059IncMed.Rdata')
load('imputedExtend.Rdata')
load('final.dat.Rdata')
countries = unique(final.dat$country)
other = read.csv('wb data updated.csv',header=T)

other = other[other[,1]=='2014',]

countries = unique(final.dat$country)

survyrlb = survyrub = c()
for (i in 1:length(countries)){
  survyrlb[i] = floor(min(final.dat$year[final.dat$country==countries[i]],na.rm=T))
  survyrub[i] = floor(max(final.dat$year[final.dat$country==countries[i]]))
}

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(region,file='region.Rdata')
load('country.Rdata')

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
ic[othercountries%in%c('Afghanistan','Benin','Burkina Faso','Burundi','Central African Republic','Chad','Comoros','Congo Democratic Republic',
                       'Eritrea','Ethiopia','Gambia','Guinea','Guinea Bissau','Haiti','DPR Korea','Liberia',
                       'Madagascar','Malawi','Mali','Mozambique','Nepal','Niger','Rwanda','Senegal','Sierra Leone','Somalia',
                       'South Sudan','Syria','Tajikistan','Tanzania','Togo','Uganda','Yemen','Zimbabwe')] = 'lic'

##### multiple imputation to generate 5 datasets of country values

country = country[ic%in%c('lic','umic','lmic')]



incl = which((country%in%countries)&country!='Sudan')
excl = which((country%in%countries)==F|country=='Sudan')
inclCountries = country[incl]; exclCountries = country[excl]

#length(exclCountries)

load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsampFinal.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/outofsamptestFinal.Rdata")
load("/Users/joelewnard/Google Drive (jlewnard@berkeley.edu)/DHS/diar inc/predFinal.Rdata")

### indexed as follows:
### dim=c(length(imps),nrep*nit,length(inputs),length(exclCountries))
### imputation, iteration (1000), inputs (6 variables), country--- 58 predicted,
### inputs: (1); ydiarIncMedObs (2); ylriIncMedObs (3); ylriInc059MedObs (4); ydiarIncMedObs*ydiarAbxObs (5); ylriIncMedObs*ylriAbxObs (6) ylriInc059MedObs*ylriAbx059Obs

country = sort(country)

estDiarIncMed = estDiarAbxMed = estLriIncMed = estLriAbxMed = estLriInc059Med = estLriAbx059Med = array(NA,dim=c(1e3,5,length(country)))
for (i in 1:length(country)){
  if (country[i]%in%countries){
    estDiarIncMed[,,i] = diarIncMed[,,which(countries==country[i])]
    estDiarAbxMed[,,i] = diarAbx[,,which(countries==country[i])]*diarIncMed[,,which(countries==country[i])]
    
    estLriIncMed[,,i] = lriIncMed[,,which(countries==country[i])]
    estLriAbxMed[,,i] = lriAbx[,,which(countries==country[i])]*lriIncMed[,,which(countries==country[i])]
    
    estLriInc059Med[,,i] = lri059IncMed[,,which(countries==country[i])]
    estLriAbx059Med[,,i] = lriAbx059[,,which(countries==country[i])]*lri059IncMed[,,which(countries==country[i])]
  }
  if (country[i]%in%exclCountries){
    for (j in 1:5){
      estDiarIncMed[,j,i] = exp(predFinal[j,,1,which(exclCountries==country[i])])
      estDiarAbxMed[,j,i] = exp(predFinal[j,,4,which(exclCountries==country[i])])
      
      estLriIncMed[,j,i] = exp(predFinal[j,,2,which(exclCountries==country[i])])
      estLriAbxMed[,j,i] = exp(predFinal[j,,5,which(exclCountries==country[i])])
      
      estLriInc059Med[,j,i] = exp(predFinal[j,,3,which(exclCountries==country[i])])
      estLriAbx059Med[,j,i] = exp(predFinal[j,,6,which(exclCountries==country[i])])
    }
  }
}


countryEsts = array(NA,dim=c(length(country),6)) ### ests for diarInc, diarAbx, LriInc, LriAbx, LriInc059, LriAbx059
## projected or observed
for (i in 1:length(country)){
  countryEsts[i,1] = median(estDiarIncMed[,,i],na.rm=T)
  countryEsts[i,2] = median(estDiarAbxMed[,,i],na.rm=T)
  countryEsts[i,3] = median(estLriIncMed[,,i],na.rm=T)
  countryEsts[i,4] = median(estLriAbxMed[,,i],na.rm=T)
  countryEsts[i,5] = median(estLriInc059Med[,,i],na.rm=T)
  countryEsts[i,6] = median(estLriAbx059Med[,,i],na.rm=T)
}


totDiarMed = totLriMed = totDiarAbxMed = totLriAbxMed = totLri059Med = totLriAbx059Med = array(NA,dim=c(5e3,length(country)))
for (i in 1:length(country)){
  totDiarMed[,i] = c(estDiarIncMed[,,i])
  totDiarAbxMed[,i] = c(estDiarAbxMed[,,i])
  
  totLriMed[,i] = c(estLriIncMed[,,i])
  totLriAbxMed[,i] = c(estLriAbxMed[,,i])
  
  totLri059Med[,i] = c(estLriInc059Med[,,i])
  totLriAbx059Med[,i] = c(estLriAbx059Med[,,i])
}

################################################################################
#### code to compare incidence rates of all-cause outcomes by country  #########
################################################################################


#### saving estimates

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(totDiarMed,file='totDiarMed.Rdata')
save(totDiarAbxMed,file='totDiarAbxMed.Rdata')
save(totLriMed,file='totLriMed.Rdata')
save(totLriAbxMed,file='totLriAbxMed.Rdata')
save(totLri059Med,file='totLri059Med.Rdata')
save(totLriAbx059Med,file='totLriAbx059Med.Rdata')
save(country,file='country.Rdata')
save(inclCountries,file='inclCountries.Rdata')
save(exclCountries,file='exclCountries.Rdata')
### rv implemented, pcv implemented, ic status, region

#################################################################
##### proportion of variance explained ##########################
#################################################################
slopeLri = slopeLriAbx = slopeDiar = slopeDiarAbx = c()
varLri = varLriAbx = varDiar = varDiarAbx = varLriMod = varLriAbxMod = varDiarMod = varDiarAbxMod = c()
for (i in 1:5e3){
  mod = lm(log(totLriMed[i,]+1e-6)~log(gdpval))
  slopeLri[i] = coef(mod)[2]
  varLriMod[i] = var(mod$residuals)
  varLri[i] = var(log(totLriMed[i,]+1e-6))
  
  mod = lm(log(totLriAbxMed[i,]+1e-6)~log(gdpval))
  slopeLriAbx[i] = coef(mod)[2]
  varLriAbxMod[i] = var(mod$residuals)
  varLriAbx[i] = var(log(totLriAbxMed[i,]+1e-6))
  
  mod = lm(log(totDiarMed[i,]+1e-6)~log(gdpval))
  slopeDiar[i] = coef(mod)[2]
  varDiarMod[i] = var(mod$residuals)
  varDiar[i] = var(log(totDiarMed[i,]+1e-6))
  
  mod = lm(log(totDiarAbxMed[i,]+1e-6)~log(gdpval))
  slopeDiarAbx[i] = coef(mod)[2]
  varDiarAbxMod[i] = var(mod$residuals)
  varDiarAbx[i] = var(log(totDiarAbxMed[i,]+1e-6))
  print(i)
}
quantile(slopeDiar,c(0.5,0.025,0.975))
quantile((varDiar - varDiarMod)/varDiar,c(0.5,0.025,0.975),na.rm=T)


incomeStat = rvImp = pcvImp = region = c()
for (i in 1:length(country)){
  incomeStat[i] = c(ic[which(othercountries==country[i])],NA)[1]
}


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

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(incomeStat,file='incomeStat.Rdata')
save(region,file='region.Rdata'); save(pcvImplemented,file='pcvImplemented.Rdata')
save(rvImplemented,file='rvImplemented.Rdata'); save(totDiar,file='totDiar.Rdata')


####################################################################
##### run from here to generate results ############################
####################################################################

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('efflri25.Rdata')
load('efftrtabxlri25.Rdata')
load('proptrtabxlri25.Rdata')
load('proplri25.Rdata')
load('proptrtabxlriSens25.Rdata')
load('proplriSens25.Rdata')
load('veIPD.Rdata')
load('veAOM.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe rota')
load('effdiar02.Rdata')
load('efftrtdiar02.Rdata')
load('efftrtabxdiar02.Rdata')
load('proptrtabxdiar02ic.Rdata')
load('propdiar02ic.Rdata')
load('velic.Rdata'); load('vemic.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
load('region.Rdata'); load('pcvImplemented.Rdata'); load('rvImplemented.Rdata')
load('incomeStat.Rdata')

veAbxSp = rep(1-exp(efftrtabxlri25),3)[1:5e3]
veAbxSp05 = rep(1-exp(efftrtabxlri05),3)[1:5e3]

veAbxRvMIC = rep(1-exp(efftrtabxdiar02[,2]),3)[1:5e3]
veAbxRvLIC = rep(1-exp(efftrtabxdiar02[,3]),3)[1:5e3]

veAbxRvIc = array(NA,dim=c(5e3,135))
for (i in 1:length(country)){
  if (ic[i]%in%c('umic','lmic')){
    veAbxRvIc[,i] = veAbxRvMIC 
  } else{
    veAbxRvIc[,i] = veAbxRvLIC
  }
}

hist(veAbxRvIc[,11])

prop.fn = function(ve,or){
  theta = 1-ve
  k = (1-or)/(or-theta)
  prop = k/(k+1)
  return(prop)
}

propAbxSp = propAbxSpSens = propAbxRotaMIC = propAbxRotaLIC = c()
for (i in 1:5e3){
  propAbxSp[i] = prop.fn(veIPD[i],1-veAbxSp[i])
  propAbxSpSens[i] = prop.fn(veAOM[i],1-veAbxSp[i])
  propAbxRotaMIC[i] = prop.fn(vemic[i],1-veAbxRvMIC[i])
  propAbxRotaLIC[i] = prop.fn(velic[i],1-veAbxRvLIC[i])
}

propAbxSp059 = propAbxSpSens059 = c()
for (i in 1:5e3){
  propAbxSp059[i] = prop.fn(veIPD[i],1-veAbxSp05[i])
  propAbxSpSens059[i] = prop.fn(veAOM[i],1-veAbxSp05[i])
}

totSpAbxMed = totPcvAvertAbxMed = totRotaAbxMed = totRvAvertAbxMed = array(NA,dim=dim(totDiarMed))
totSpAbxSensMed = totPcvAvertAbxSensMed = array(NA,dim=dim(totDiarMed))
for (i in 1:length(country)){
  totSpAbxMed[,i] = totLriAbxMed[,i]*propAbxSp
  totPcvAvertAbxMed[,i] = totLriAbxMed[,i]*veAbxSp
  
  totSpAbxSensMed[,i] = totLriAbxMed[,i]*propAbxSpSens
  totPcvAvertAbxSensMed[,i] = totLriAbxMed[,i]*veAbxSp
  
  if (incomeStat[i]=='lic'){
    totRotaAbxMed[,i] = totDiarAbxMed[,i]*propAbxRotaLIC
    totRvAvertAbxMed[,i] = totDiarAbxMed[,i]*veAbxRvLIC
  } else{
    totRotaAbxMed[,i] = totDiarAbxMed[,i]*propAbxRotaMIC
    totRvAvertAbxMed[,i] = totDiarAbxMed[,i]*veAbxRvMIC
  }
}

totSpAbx059Med = totPcvAvertAbx059Med = totSpAbxSens059Med = totPcvAvertAbxSens059Med = array(NA,dim=dim(totDiarMed))
for (i in 1:length(country)){
  totSpAbx059Med[,i] = totLriAbx059Med[,i]*propAbxSp059
  totPcvAvertAbx059Med[,i] = totLriAbx059Med[,i]*veAbxSp05
  totSpAbxSens059Med[,i] = totLriAbx059Med[,i]*propAbxSpSens059
  totPcvAvertAbxSens059Med[,i] = totLriAbx059Med[,i]*veAbxSp05
}

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(totSpAbx059Med,file='totSpAbx059Med.Rdata'); save(totPcvAvertAbx059Med,file='totPcvAvertAbx059Med.Rdata')
save(totSpAbxSens059Med,file='totSpAbxSens059Med.Rdata'); save(totPcvAvertAbxSens059Med,file='totPcvAvertAbxSens059Med.Rdata')

save(totSpAbxMed,file='totSpAbxMed.Rdata'); save(totPcvAvertAbxMed,file='totPcvAvertAbxMed.Rdata')
save(totRotaAbxMed,file='totRotaAbxMed.Rdata'); save(totRvAvertAbxMed,file='totRvAvertAbxMed.Rdata')

save(totSpAbxSensMed,file='totSpAbxSensMed.Rdata'); save(totPcvAvertAbxSensMed,file='totPcvAvertAbxSensMed.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
pops = read.csv('countrypops.csv',header=T)
pop = c(); for (i in 1:length(country)){
  if (country[i]%in%pops$name){
    pop[i] = pops$y2016[which(pops$name==country[i])]
  }
}


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

#sum(pop01[ic!='lic'],na.rm=T)
#save(pop,file='pop.Rdata'); save(pop01,file='pop01.Rdata'); save(pop24,file='pop24.Rdata')

###############################################################################################
###### estimated incidence absent vaccination: array by region/income/implementation ##########
###############################################################################################
load('pop01.Rdata'); load('pop24.Rdata')
load('totSpAbxMed.Rdata'); load('totSpAbxSensMed.Rdata')
load('totSpAbx059Med.Rdata'); load('totSpAbxSens059Med.Rdata')
load('totRotaAbxMed.Rdata'); load('totRotaAbxLB.Rdata'); load('totRotaAbxUB.Rdata')
load('totSpAbxUB.Rdata'); load('totSpAbxLB.Rdata'); load('totSpAbxSensUB.Rdata'); load('totSpAbxSensLB.Rdata')
load('totSpAbx059UB.Rdata'); load('totSpAbx059LB.Rdata'); load('totSpAbxSens059UB.Rdata'); load('totSpAbxSens059LB.Rdata')
load('totRotaAbxUB.Rdata'); load('totRotaAbxLB.Rdata')
load('region.Rdata')

regions = c('AFR','AMR','EMR','EUR','SEAR','WPR','Tot')
spRegAbxIncMed = spRegAbxSumMed = rotaRegAbxIncMed = rotaRegAbxSumMed = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059Med = spRegAbxSum059Med = array(NA,dim=dim(spRegAbxIncMed))
spRegAbxIncSensMed = spRegAbxSumSensMed = array(NA,dim=dim(spRegAbxIncMed))
spRegAbxIncSens059Med = spRegAbxSumSens059Med = array(NA,dim=dim(spRegAbxInc059Med))

spRegAbxIncLB = spRegAbxSumLB = rotaRegAbxIncLB = rotaRegAbxSumLB = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059LB = spRegAbxSum059LB = array(NA,dim=dim(spRegAbxIncLB))
spRegAbxIncSensLB = spRegAbxSumSensLB = array(NA,dim=dim(spRegAbxIncLB))
spRegAbxIncSens059LB = spRegAbxSumSens059LB = array(NA,dim=dim(spRegAbxInc059LB))

spRegAbxIncUB = spRegAbxSumUB = rotaRegAbxIncUB = rotaRegAbxSumUB = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059UB = spRegAbxSum059UB = array(NA,dim=dim(spRegAbxIncUB))
spRegAbxIncSensUB = spRegAbxSumSensUB = array(NA,dim=dim(spRegAbxIncUB))
spRegAbxIncSens059UB = spRegAbxSumSens059UB = array(NA,dim=dim(spRegAbxInc059UB))

regVals = list(); for (i in 1:6){regVals[[i]] = regions[i]}; regVals[[7]] = unique(region)
impVals = list(); impVals[[1]] = 1; impVals[[2]] = 0; impVals[[3]] = c(0,1)
incVals = list(); incVals[[1]] = 'lic'; incVals[[2]] = 'lmic'; incVals[[3]] = 'umic'; incVals[[4]] = c('lic','lmic','umic')
for (i in 1:7){
  for (j in 1:3){
    for (k in 1:4){
      if (sum(region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        spCut = totSpAbxMed[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncMed[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumMed[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxUB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncUB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumUB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxLB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncLB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumLB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSensMed[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSensMed[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSensMed[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSensLB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSensLB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSensLB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSensUB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSensUB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSensUB[i,j,k,] = spOut[1:5e3]
        
        ########## ages 0-59m
        spCut = totSpAbx059Med[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059Med[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059Med[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbx059UB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059UB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059UB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbx059LB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059LB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059LB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSens059Med[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSens059Med[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSens059Med[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSens059LB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSens059LB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSens059LB[i,j,k,] = spOut[1:5e3]
        
        spCut = totSpAbxSens059UB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = (pop01+pop24)[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncSens059UB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumSens059UB[i,j,k,] = spOut[1:5e3]
      }
      if (sum(region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        rotaCut = totRotaAbxMed[,region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]    
        popCut = pop01[region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncMed[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumMed[i,j,k,] = rotaOut[1:5e3]
        
        rotaCut = totRotaAbxUB[,region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]    
        popCut = pop01[region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncUB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumUB[i,j,k,] = rotaOut[1:5e3]
        
        rotaCut = totRotaAbxLB[,region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]    
        popCut = pop01[region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncLB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumLB[i,j,k,] = rotaOut[1:5e3]
      }
    }
  }
}

spRegAbxIncMed = spRegAbxIncMed*100
spRegAbxSumMed = spRegAbxSumMed/1e6
spRegAbxInc059Med = spRegAbxInc059Med*100
spRegAbxSum059Med = spRegAbxSum059Med/1e6
rotaRegAbxIncMed = rotaRegAbxIncMed*100
rotaRegAbxSumMed = rotaRegAbxSumMed/1e6
spRegAbxIncSensMed = spRegAbxIncSensMed*100
spRegAbxSumSensMed = spRegAbxSumSensMed/1e6
spRegAbxIncSens059Med = spRegAbxIncSens059Med*100
spRegAbxSumSens059Med = spRegAbxSumSens059Med/1e6

spRegAbxIncUB = spRegAbxIncUB*100
spRegAbxSumUB = spRegAbxSumUB/1e6
spRegAbxInc059UB = spRegAbxInc059UB*100
spRegAbxSum059UB = spRegAbxSum059UB/1e6
rotaRegAbxIncUB = rotaRegAbxIncUB*100
rotaRegAbxSumUB = rotaRegAbxSumUB/1e6
spRegAbxIncSensUB = spRegAbxIncSensUB*100
spRegAbxSumSensUB = spRegAbxSumSensUB/1e6
spRegAbxIncSens059UB = spRegAbxIncSens059UB*100
spRegAbxSumSens059UB = spRegAbxSumSens059UB/1e6

spRegAbxIncLB = spRegAbxIncLB*100
spRegAbxSumLB = spRegAbxSumLB/1e6
spRegAbxInc059LB = spRegAbxInc059LB*100
spRegAbxSum059LB = spRegAbxSum059LB/1e6
rotaRegAbxIncLB = rotaRegAbxIncLB*100
rotaRegAbxSumLB = rotaRegAbxSumLB/1e6
spRegAbxIncSensLB = spRegAbxIncSensLB*100
spRegAbxSumSensLB = spRegAbxSumSensLB/1e6
spRegAbxIncSens059LB = spRegAbxIncSens059LB*100
spRegAbxSumSens059LB = spRegAbxSumSens059LB/1e6

spIncMed = spRegAbxIncMed; spSumMed = spRegAbxSumMed; rotaIncMed = rotaRegAbxIncMed; rotaSumMed = rotaRegAbxSumMed;
spIncSensMed = spRegAbxIncSensMed; spSumSensMed = spRegAbxSumSensMed; 

spIncLB = spRegAbxIncLB; spSumLB = spRegAbxSumLB; rotaIncLB = rotaRegAbxIncLB; rotaSumLB = rotaRegAbxSumLB;
spIncSensLB = spRegAbxIncSensLB; spSumSensLB = spRegAbxSumSensLB; 

spIncUB = spRegAbxIncUB; spSumUB = spRegAbxSumUB; rotaIncUB = rotaRegAbxIncUB; rotaSumUB = rotaRegAbxSumUB;
spIncSensUB = spRegAbxIncSensUB; spSumSensUB = spRegAbxSumSensUB; 

spInc059Med = spRegAbxInc059Med; spInc059LB = spRegAbxInc059LB; spInc059UB = spRegAbxInc059UB
spSum059Med = spRegAbxSum059Med; spSum059LB = spRegAbxSum059LB; spSum059UB = spRegAbxSum059UB
spIncSens059Med = spRegAbxIncSens059Med; spIncSens059LB = spRegAbxIncSens059LB; spIncSens059UB = spRegAbxIncSens059UB
spSumSens059Med = spRegAbxSumSens059Med; spSumSens059LB = spRegAbxSumSens059LB; spSumSens059UB = spRegAbxSumSens059UB

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(spIncMed,file='spIncMed.Rdata'); save(spSumMed,file='spSumMed.Rdata')
save(spIncSensMed,file='spIncSensMed.Rdata'); save(spSumSensMed,file='spSumSensMed.Rdata')
save(spInc059Med,file='spInc059Med.Rdata'); save(spSum059Med,file='spSum059Med.Rdata')
save(spIncSens059Med,file='spIncSens059Med.Rdata'); save(spSumSens059Med,file='spSumSens059Med.Rdata')
save(rotaIncMed,file='rotaIncMed.Rdata'); save(rotaSumMed,file='rotaSumMed.Rdata')

save(spIncUB,file='spIncUB.Rdata'); save(spSumUB,file='spSumUB.Rdata')
save(spIncSensUB,file='spIncSensUB.Rdata'); save(spSumSensUB,file='spSumSensUB.Rdata')
save(spInc059UB,file='spInc059UB.Rdata'); save(spSum059UB,file='spSum059UB.Rdata')
save(spIncSens059UB,file='spIncSens059UB.Rdata'); save(spSumSens059UB,file='spSumSens059UB.Rdata')
save(rotaIncUB,file='rotaIncUB.Rdata'); save(rotaSumUB,file='rotaSumUB.Rdata')

save(spIncLB,file='spIncLB.Rdata'); save(spSumLB,file='spSumLB.Rdata')
save(spIncSensLB,file='spIncSensLB.Rdata'); save(spSumSensLB,file='spSumSensLB.Rdata')
save(spInc059LB,file='spInc059LB.Rdata'); save(spSum059LB,file='spSum059LB.Rdata')
save(spIncSens059LB,file='spIncSens059LB.Rdata'); save(spSumSens059LB,file='spSumSens059LB.Rdata')
save(rotaIncLB,file='rotaIncLB.Rdata'); save(rotaSumLB,file='rotaSumLB.Rdata')


for (j in 1:4){print(round(quantile(spIncSens059UB[7,3,j,],c(0.5,0.025,0.975),na.rm=T),1))}

############################################################################################################
###### estimated incidence under observed vaccine coverage: array by region/income/implementation ##########
############################################################################################################

rvcov = rep(NA,length(rvImplemented)); rvcov[rvImplemented==0] = 0
rvcountries = c('Afghanistan','Angola','Armenia',
                'Bolivia','Botswana','Brazil','Bulgaria',
                'Burkina Faso','Burundi','Cameroon','Colombia','Congo',
                "Cote d'Ivoire",'Djibouti','Dominican Republic','Ecuador',
                'El Salvador','Eritrea','Eswatini','Ethiopia','Fiji','Gambia',
                'Georgia','Ghana','Guatemala','Guinea Bissau','Guyana','Haiti',
                'Honduras','India','Iraq','Jordan','Kenya','Kiribati','Lesotho',
                'Liberia','Libya','Madagascar','Malawi','Mali','Marshall Islands',
                'Mauritania','Mauritius','Mexico','Micronesia','Moldova','Morocco',
                'Mozambique','Namibia','Nicaragua','Niger','Pakistan','Paraguay',
                'Peru','Philippines','Rwanda','Sao Tome and Principe','Senegal',
                'Sierra Leone','South Africa','Sudan','Tajikistan','Tanzania',
                'Togo','Uganda','Uzbekistan','Venezuela','Yemen',
                'Zambia','Zimbabwe')

rvvals = c(0.6,0.75,0.93,0.87,0.74,0.8,0.31,0.91,0.92,0.78,0.9,0.83,0.72,0.87,0.82,0.85,
           0.82,0.96,0.9,0.93,0.86,0.93,0.79,0.94,0.87,0.85,0.91,0.73,0.92,0.73,0.6,0.93,
           0.78,0.97,0.7,0.87,0.97,0.89,0.9,0.6,0.42,0.89,0.95,0.77,0.52,0.75,0.99,0.9,0.92,
           0.99,0.89,0.58,0.79,0.85,0.82,0.98,0.95,0.63,0.96,0.8,0.94,0.96,0.98,0.89,0.36,
           0.84,0.18,0.79,0.91,0.9)

for (i in 1:length(rvcountries)){rvcov[which(country==rvcountries[i])] = rvvals[i]}


rvcov[which(country=='Palestine')] = mean(rvcov[region=='EMR'&rvImplemented==1],na.rm=T)
rvcov[which(country=='Thailand')] = mean(rvcov[region=='SEAR'&rvImplemented==1],na.rm=T)
rvcov[which(country=='American Samoa')] = mean(rvcov[region=='WPR'&rvImplemented==1],na.rm=T)
rvcov[which(country=='Kosovo')] = mean(rvcov[region=='EUR'&rvImplemented==1],na.rm=T)

pcvcov = rep(NA,length(pcvImplemented)); pcvcov[pcvImplemented==0] = 0
pcvcountries = c('Afghanistan','Albania','Angola','Bangladesh','Armenia','Azerbaijan','Belarus',
                 'Benin','Bolivia','Botswana','Brazil','Bulgaria','Burkina Faso','Burundi',
                 'Cambodia','Cameroon','Central African Republic','Colombia','Congo',
                 'Congo Democratic Republic','Costa Rica',"Cote d'Ivoire",'Djibouti','Dominican Republic',
                 'Ecuador','El Salvador','Eritrea','Eswatini','Ethiopia','Fiji','Gambia','Georgia',
                 'Ghana','Guatemala','Guinea Bissau','Guyana','Haiti','Honduras','India','Indonesia',
                 'Iraq','Jamaica','Kazakhstan','Kenya','Kiribati','Kyrgyz Republic','Lao PDR','Lebanon',
                 'Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Marshall Islands','Mauritania',
                 'Mauritius','Mexico','Micronesia','Mongolia','Morocco','Mozambique','Myanmar','Namibia',
                 'Nepal','Nicaragua','Niger','Nigeria','Pakistan','Papua New Guinea','Paraguay','Peru',
                 'Philippines','Russian Federation','Rwanda','Sao Tome and Principe','Senegal',
                 'Sierra Leone','Solomon Islands','South Africa','Sudan','Tanzania','Togo','Turkey',
                 'Uganda','Uzbekistan','Venezuela','Yemen','Zambia','Zimbabwe','Moldova')

pcvvals = c(0.84,0.99,0.82,0.99,0.92,0.95,0.98,0.75,0.83,0.77,0.84,0.89,0.91,0.9,0.93,0.79,0.73,0.94,0.83,
            0.94,0.96,0.94,0.84,0.7,0.85,0.75,0.95,0.88,0.94,0.86,0.93,0.81,0.96,0.85,0.82,0.91,0.01,0.91,
            0.44,0.08,0.32,0.44,0.95,0.81,0.94,0.92,0.83,0.82,0.83,0.97,0.96,0.91,0.92,0.63,0.67,0.88,
            0.96,0.88,0.67,0.98,0.99,0.9,0.91,0.81,0.82,0.99,0.79,0.58,0.72,0.43,0.79,0.82,0.6,0.82,0.97,
            0.95,0.81,0.94,0.84,0.83,0.93,0.98,0.88,0.97,0.64,0.96,0,0.79,0.9,0.89,0.94)

for (i in 1:length(pcvcov)){pcvcov[which(country==pcvcountries[i])] = pcvvals[i]}

pcvcov[which(country=='Palestine')] = mean(pcvcov[region=='EMR'&pcvImplemented==1],na.rm=T)
pcvcov[which(country=='American Samoa')] = mean(pcvcov[region=='WPR'&pcvImplemented==1],na.rm=T)
pcvcov[which(country=='Kosovo')] = mean(pcvcov[region=='EUR'&pcvImplemented==1],na.rm=T)

regions = c('AFR','AMR','EMR','EUR','SEAR','WPR','Tot')

load('totLriAbxMed.Rdata')
load('totLriAbx059Med.Rdata')
load('totDiarAbxMed.Rdata')

load('totLriAbxLB.Rdata')
load('totLriAbx059LB.Rdata')
load('totDiarAbxLB.Rdata')

load('totLriAbxUB.Rdata')
load('totLriAbx059UB.Rdata')
load('totDiarAbxUB.Rdata')

spRegAbxIncMed = spRegAbxSumMed = rotaRegAbxIncMed = rotaRegAbxSumMed = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059Med = spRegAbxSum059Med = array(NA,dim=dim(spRegAbxIncMed))

spRegAbxIncLB = spRegAbxSumLB = rotaRegAbxIncLB = rotaRegAbxSumLB = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059LB = spRegAbxSum059LB = array(NA,dim=dim(spRegAbxIncLB))

spRegAbxIncUB = spRegAbxSumUB = rotaRegAbxIncUB = rotaRegAbxSumUB = array(NA,dim=c(7,3,4,5e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxInc059UB = spRegAbxSum059UB = array(NA,dim=dim(spRegAbxIncUB))

regVals = list(); for (i in 1:6){regVals[[i]] = regions[i]}; regVals[[7]] = unique(region)
impVals = list(); impVals[[1]] = 1; impVals[[2]] = 0; impVals[[3]] = c(0,1)
incVals = list(); incVals[[1]] = 'lic'; incVals[[2]] = 'lmic'; incVals[[3]] = 'umic'; incVals[[4]] = c('lic','lmic','umic')
for (i in 1:7){
  for (j in 1:3){
    for (k in 1:4){
      if (sum(region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        
        sel = region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbxMed[l,sel] - pcvcov[sel]*veAbxSp[l]*totLriAbxMed[l,sel]
        }
        popCut = pop24[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncMed[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumMed[i,j,k,] = spOut[1:5e3]
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbxLB[l,sel] - pcvcov[sel]*veAbxSp[l]*totLriAbxLB[l,sel]
        }
        popCut = pop24[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncLB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumLB[i,j,k,] = spOut[1:5e3]
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbxUB[l,sel] - pcvcov[sel]*veAbxSp[l]*totLriAbxUB[l,sel]
        }
        popCut = pop24[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncUB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSumUB[i,j,k,] = spOut[1:5e3]
        
        #############################################
        ######### 0-59 month age group ##############
        #############################################
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbx059Med[l,sel] - pcvcov[sel]*veAbxSp05[l]*totLriAbx059Med[l,sel]
        }
        popCut = (pop24+pop01)[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059Med[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059Med[i,j,k,] = spOut[1:5e3]
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbx059LB[l,sel] - pcvcov[sel]*veAbxSp05[l]*totLriAbx059LB[l,sel]
        }
        popCut = (pop24+pop01)[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059LB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059LB[i,j,k,] = spOut[1:5e3]
        
        spCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          spCut[l,] = totSpAbx059UB[l,sel] - pcvcov[sel]*veAbxSp05[l]*totLriAbx059UB[l,sel]
        }
        popCut = (pop24+pop01)[sel]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc059UB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
        spRegAbxSum059UB[i,j,k,] = spOut[1:5e3]
        
      }
      if (sum(region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        
        sel = region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]

        rotaCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          rotaCut[l,] = totRotaAbxMed[l,sel] - rvcov[sel]*veAbxRvIc[l,sel]*totDiarAbxMed[l,sel]
        }
        popCut = pop01[sel]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncMed[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumMed[i,j,k,] = rotaOut[1:5e3]
        
        rotaCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          rotaCut[l,] = totRotaAbxUB[l,sel] - rvcov[sel]*veAbxRvIc[l,sel]*totDiarAbxUB[l,sel]
        }
        popCut = pop01[sel]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncUB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumUB[i,j,k,] = rotaOut[1:5e3]
        
        rotaCut = array(NA,dim=c(5e3,sum(sel)))
        for (l in 1:5e3){
          rotaCut[l,] = totRotaAbxLB[l,sel] - rvcov[sel]*veAbxRvIc[l,sel]*totDiarAbxLB[l,sel]
        }
        popCut = pop01[sel]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncLB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
        rotaRegAbxSumLB[i,j,k,] = rotaOut[1:5e3]
      }
    }
  }
}

spRegAbxIncMed = spRegAbxIncMed*100
spRegAbxSumMed = spRegAbxSumMed/1e6
rotaRegAbxIncMed = rotaRegAbxIncMed*100
rotaRegAbxSumMed = rotaRegAbxSumMed/1e6

spRegAbxIncLB = spRegAbxIncLB*100
spRegAbxSumLB = spRegAbxSumLB/1e6
rotaRegAbxIncLB = rotaRegAbxIncLB*100
rotaRegAbxSumLB = rotaRegAbxSumLB/1e6

spRegAbxIncUB = spRegAbxIncUB*100
spRegAbxSumUB = spRegAbxSumUB/1e6
rotaRegAbxIncUB = rotaRegAbxIncUB*100
rotaRegAbxSumUB = rotaRegAbxSumUB/1e6

spRegAbxInc059Med = spRegAbxInc059Med*100
spRegAbxSum059Med = spRegAbxSum059Med/1e6
spRegAbxInc059LB = spRegAbxInc059LB*100
spRegAbxSum059LB = spRegAbxSum059LB/1e6
spRegAbxInc059UB = spRegAbxInc059UB*100
spRegAbxSum059UB = spRegAbxSum059UB/1e6





setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(spRegAbxIncMed,file='spRegAbxIncMed.Rdata'); save(spRegAbxSumMed,file='spRegAbxSumMed.Rdata')
save(spRegAbxInc059Med,file='spRegAbxInc059Med.Rdata'); save(spRegAbxSum059Med,file='spRegAbxSum059Med.Rdata')
save(rotaRegAbxIncMed,file='rotaRegAbxIncMed.Rdata'); save(rotaRegAbxSumMed,file='rotaRegAbxSumMed.Rdata')

save(spRegAbxIncUB,file='spRegAbxIncUB.Rdata'); save(spRegAbxSumUB,file='spRegAbxSumUB.Rdata')
save(spRegAbxInc059UB,file='spRegAbxInc059UB.Rdata'); save(spRegAbxSum059UB,file='spRegAbxSum059UB.Rdata')
save(rotaRegAbxIncUB,file='rotaRegAbxIncUB.Rdata'); save(rotaRegAbxSumUB,file='rotaRegAbxSumUB.Rdata')

save(spRegAbxIncLB,file='spRegAbxIncLB.Rdata'); save(spRegAbxSumLB,file='spRegAbxSumLB.Rdata')
save(spRegAbxInc059LB,file='spRegAbxInc059LB.Rdata'); save(spRegAbxSum059LB,file='spRegAbxSum059LB.Rdata')
save(rotaRegAbxIncLB,file='rotaRegAbxIncLB.Rdata'); save(rotaRegAbxSumLB,file='rotaRegAbxSumLB.Rdata')

######################################################################################################################
##### incidence under universal coverage #############################################################################
######################################################################################################################

spRegAbxIncMaxMed = spRegAbxSumMaxMed = rotaRegAbxIncMaxMed = rotaRegAbxSumMaxMed = array(NA,dim=dim(spRegAbxIncMed))
spRegAbxIncMaxUB = spRegAbxSumMaxUB = rotaRegAbxIncMaxUB = rotaRegAbxSumMaxUB = array(NA,dim=dim(spRegAbxIncUB))
spRegAbxIncMaxLB = spRegAbxSumMaxLB = rotaRegAbxIncMaxLB = rotaRegAbxSumMaxLB = array(NA,dim=dim(spRegAbxIncLB))
spRegAbxIncMax059Med = spRegAbxSumMax059Med = array(NA,dim=dim(spRegAbxIncMed))
spRegAbxIncMax059UB = spRegAbxSumMax059UB = array(NA,dim=dim(spRegAbxIncUB))
spRegAbxIncMax059LB = spRegAbxSumMax059LB = array(NA,dim=dim(spRegAbxIncLB))
for (i in 1:7) for (j in 1:3) for (k in 1:4){
  if (sum(region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
    
    sel = region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbxMed[l,sel] - veAbxSp[l]*totLriAbxMed[l,sel]
    }
    popCut = pop24[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMaxMed[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMaxMed[i,j,k,] = spOut[1:5e3]
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbxLB[l,sel] - veAbxSp[l]*totLriAbxLB[l,sel]
    }
    popCut = pop24[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMaxLB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMaxLB[i,j,k,] = spOut[1:5e3]
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbxUB[l,sel] - veAbxSp[l]*totLriAbxUB[l,sel]
    }
    popCut = pop24[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMaxUB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMaxUB[i,j,k,] = spOut[1:5e3]
    
    ################################
    #### 0-59 month age group ######
    ################################
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbx059Med[l,sel] - veAbxSp05[l]*totLriAbx059Med[l,sel]
    }
    popCut = (pop24+pop01)[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMax059Med[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMax059Med[i,j,k,] = spOut[1:5e3]
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbx059LB[l,sel] - veAbxSp05[l]*totLriAbx059LB[l,sel]
    }
    popCut = (pop24+pop01)[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMax059LB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMax059LB[i,j,k,] = spOut[1:5e3]
    
    spCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      spCut[l,] = totSpAbx059UB[l,sel] - veAbxSp05[l]*totLriAbx059UB[l,sel]
    }
    popCut = (pop24+pop01)[sel]
    spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
    spRegAbxIncMax059UB[i,j,k,] = (spOut/sum(popCut))[1:5e3]
    spRegAbxSumMax059UB[i,j,k,] = spOut[1:5e3]
    
  }
  if (sum(region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
    
    sel = region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]
    
    rotaCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      rotaCut[l,] = totRotaAbxMed[l,sel] - veAbxRvIc[l,sel]*totDiarAbxMed[l,sel]
    }
    popCut = pop01[sel]
    rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
    rotaRegAbxIncMaxMed[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
    rotaRegAbxSumMaxMed[i,j,k,] = rotaOut[1:5e3]
    
    rotaCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      rotaCut[l,] = totRotaAbxUB[l,sel] - veAbxRvIc[l,sel]*totDiarAbxUB[l,sel]
    }
    popCut = pop01[sel]
    rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
    rotaRegAbxIncMaxUB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
    rotaRegAbxSumMaxUB[i,j,k,] = rotaOut[1:5e3]
    
    rotaCut = array(NA,dim=c(5e3,sum(sel)))
    for (l in 1:5e3){
      rotaCut[l,] = totRotaAbxLB[l,sel] - veAbxRvIc[l,sel]*totDiarAbxLB[l,sel]
    }
    popCut = pop01[sel]
    rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
    rotaRegAbxIncMaxLB[i,j,k,] = (rotaOut/sum(popCut))[1:5e3]
    rotaRegAbxSumMaxLB[i,j,k,] = rotaOut[1:5e3]
  }
}

spRegAbxIncMaxMed = spRegAbxIncMaxMed*100
spRegAbxSumMaxMed = spRegAbxSumMaxMed/1e6
rotaRegAbxIncMaxMed = rotaRegAbxIncMaxMed*100
rotaRegAbxSumMaxMed = rotaRegAbxSumMaxMed/1e6

spRegAbxIncMaxLB = spRegAbxIncMaxLB*100
spRegAbxSumMaxLB = spRegAbxSumMaxLB/1e6
rotaRegAbxIncMaxLB = rotaRegAbxIncMaxLB*100
rotaRegAbxSumMaxLB = rotaRegAbxSumMaxLB/1e6

spRegAbxIncMaxUB = spRegAbxIncMaxUB*100
spRegAbxSumMaxUB = spRegAbxSumMaxUB/1e6
rotaRegAbxIncMaxUB = rotaRegAbxIncMaxUB*100
rotaRegAbxSumMaxUB = rotaRegAbxSumMaxUB/1e6

spRegAbxIncMax059Med = spRegAbxIncMax059Med*100
spRegAbxSumMax059Med = spRegAbxSumMax059Med/1e6
spRegAbxIncMax059LB = spRegAbxIncMax059LB*100
spRegAbxSumMax059LB = spRegAbxSumMax059LB/1e6
spRegAbxIncMax059UB = spRegAbxIncMax059UB*100
spRegAbxSumMax059UB = spRegAbxSumMax059UB/1e6



setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(spRegAbxIncMaxMed,file='spRegAbxIncMaxMed.Rdata'); save(spRegAbxSumMaxMed,file='spRegAbxSumMaxMed.Rdata')
save(rotaRegAbxIncMaxMed,file='rotaRegAbxIncMaxMed.Rdata'); save(rotaRegAbxSumMaxMed,file='rotaRegAbxSumMaxMed.Rdata')



addPrevSp = spRegAbxSumMed - spRegAbxSumMaxMed
addPrevRota = rotaRegAbxSumMed - rotaRegAbxSumMaxMed

round.fn = function(x,places){
  out = round(x,places)
  if (out==round(x,0)){
    out = paste(out,'.0',sep='')
  }
  return(out)
}


tabSp = tabRota = tabSpLB = tabRotaLB = matrix(NA,4*7,2*4)
for (i in 1:7){
  for (j in 1:3){
    for (k in 1:4){
      tabSp[(i-1)*4+1+j,(k-1)*2+1] = paste(round(median(spRegAbxInc[i,j,k,],na.rm=T),1),' (',
                                           round(quantile(spRegAbxInc[i,j,k,],0.025,na.rm=T),1),'-',
                                           round(quantile(spRegAbxInc[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabSp[(i-1)*4+1+j,(k-1)*2+2] = paste(round(median(spRegAbxSum[i,j,k,],na.rm=T),1),' (',
                                           round(quantile(spRegAbxSum[i,j,k,],0.025,na.rm=T),1),'-',
                                           round(quantile(spRegAbxSum[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabRota[(i-1)*4+1+j,(k-1)*2+1] = paste(round(median(rotaRegAbxInc[i,j,k,],na.rm=T),1),' (',
                                             round(quantile(rotaRegAbxInc[i,j,k,],0.025,na.rm=T),1),'-',
                                             round(quantile(rotaRegAbxInc[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabRota[(i-1)*4+1+j,(k-1)*2+2] = paste(round(median(rotaRegAbxSum[i,j,k,],na.rm=T),1),' (',
                                             round(quantile(rotaRegAbxSum[i,j,k,],0.025,na.rm=T),1),'-',
                                             round(quantile(rotaRegAbxSum[i,j,k,],0.975,na.rm=T),1),')',sep='')
      
      tabSpLB[(i-1)*4+1+j,(k-1)*2+1] = paste(round(median(spRegAbxIncLB[i,j,k,],na.rm=T),1),' (',
                                             round(quantile(spRegAbxIncLB[i,j,k,],0.025,na.rm=T),1),'-',
                                             round(quantile(spRegAbxIncLB[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabSpLB[(i-1)*4+1+j,(k-1)*2+2] = paste(round(median(spRegAbxSumLB[i,j,k,],na.rm=T),1),' (',
                                             round(quantile(spRegAbxSumLB[i,j,k,],0.025,na.rm=T),1),'-',
                                             round(quantile(spRegAbxSumLB[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabRotaLB[(i-1)*4+1+j,(k-1)*2+1] = paste(round(median(rotaRegAbxIncLB[i,j,k,],na.rm=T),1),' (',
                                               round(quantile(rotaRegAbxIncLB[i,j,k,],0.025,na.rm=T),1),'-',
                                               round(quantile(rotaRegAbxIncLB[i,j,k,],0.975,na.rm=T),1),')',sep='')
      tabRotaLB[(i-1)*4+1+j,(k-1)*2+2] = paste(round(median(rotaRegAbxSumLB[i,j,k,],na.rm=T),1),' (',
                                               round(quantile(rotaRegAbxSumLB[i,j,k,],0.025,na.rm=T),1),'-',
                                               round(quantile(rotaRegAbxSumLB[i,j,k,],0.975,na.rm=T),1),')',sep='')
    }
  }
}
tabSp[tabSp=='NA (NA-NA)'] = '- -'; tabRota[tabRota=='NA (NA-NA)'] = '- -'
tabSpLB[tabSpLB=='NA (NA-NA)'] = '- -'; tabRotaLB[tabRotaLB=='NA (NA-NA)'] = '- -'

#tabRotaLB
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
write.csv(tabSp,file='tabSp.csv'); write.csv(tabRota,file='tabRota.csv')
write.csv(tabSpLB,file='tabSpLB.csv'); write.csv(tabRotaLB,file='tabRotaLB.csv')




####################################################
###### reporting output: table by country ##########
####################################################

### country

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')

###########################################################
############# direct effects by income status #############
###########################################################




country[region=='EUR'&incomeStat=='lic']

spRegAbxInc = spRegAbxInc*100
spRegAbxSum = spRegAbxSum/1e6
rotaRegAbxInc = rotaRegAbxInc*100
rotaRegAbxSum = rotaRegAbxSum/1e6

spRegAbxIncLB = spRegAbxIncLB*100
spRegAbxSumLB = spRegAbxSumLB/1e6
rotaRegAbxIncLB = rotaRegAbxIncLB*100
rotaRegAbxSumLB = rotaRegAbxSumLB/1e6


dirSpInc = spRegAbxInc; dirSpSum = spRegAbxSum; dirRotaInc = rotaRegAbxInc; dirRotaSum = rotaRegAbxSum;
dirSpIncLB = spRegAbxIncLB; dirSpSumLB = spRegAbxSumLB; dirRotaIncLB = rotaRegAbxIncLB; dirRotaSumLB = rotaRegAbxSumLB;

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(dirSpInc,file='dirSpInc.Rdata'); save(dirSpSum,file='dirSpSum.Rdata')
save(dirRotaInc,file='dirRotaInc.Rdata'); save(dirRotaSum,file='dirRotaSum.Rdata')
save(dirSpIncLB,file='dirSpIncLB.Rdata'); save(dirSpSumLB,file='dirSpSumLB.Rdata')
save(dirRotaIncLB,file='dirRotaIncLB.Rdata'); save(dirRotaSumLB,file='dirRotaSumLB.Rdata')



regions = c('AFR','AMR','EMR','EUR','SEAR','WPR','Tot')
spRegAbxInc = spRegAbxSum = rotaRegAbxInc = rotaRegAbxSum = array(NA,dim=c(7,3,4,4e3)) ### region, implementation, income (all +1 for all settings)
spRegAbxIncLB = spRegAbxSumLB = rotaRegAbxIncLB = rotaRegAbxSumLB = array(NA,dim=c(7,3,4,4e3)) ### region, implementation, income (all +1 for all settings)

regVals = list(); for (i in 1:6){regVals[[i]] = regions[i]}; regVals[[7]] = unique(region)
impVals = list(); impVals[[1]] = 1; impVals[[2]] = 0; impVals[[3]] = c(0,1)
incVals = list(); incVals[[1]] = 'lic'; incVals[[2]] = 'lmic'; incVals[[3]] = 'umic'; incVals[[4]] = c('lic','lmic','umic')
for (i in 1:7){
  for (j in 1:3){
    for (k in 1:4){
      if (sum(region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        spCut = totPcvAvertAbx[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOut = as.matrix(spCut)%*%as.matrix(popCut,ncol=1)
        spRegAbxInc[i,j,k,] = (spOut/sum(popCut))[1:4e3]
        spRegAbxSum[i,j,k,] = spOut[1:4e3]
        
        spCutLB = totPcvAvertAbxLB[,region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]] 
        popCut = pop24[region%in%regVals[[i]]&pcvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        spOutLB = as.matrix(spCutLB)%*%as.matrix(popCut,ncol=1)
        spRegAbxIncLB[i,j,k,] = (spOutLB/sum(popCut))[1:4e3]
        spRegAbxSumLB[i,j,k,] = spOutLB[1:4e3]
      }
      if (sum(region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]])>0){
        rotaCut = totRvAvertAbx[,region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]  
        popCut = pop01[region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        rotaOut = as.matrix(rotaCut)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxInc[i,j,k,] = (rotaOut/sum(popCut))[1:4e3]
        rotaRegAbxSum[i,j,k,] = rotaOut[1:4e3]
        
        rotaCutLB = totRvAvertAbxLB[,region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]] 
        popCut = pop01[region%in%regVals[[i]]&rvImplemented%in%impVals[[j]]&incomeStat%in%incVals[[k]]]
        rotaOutLB = as.matrix(rotaCutLB)%*%as.matrix(popCut,ncol=1)
        rotaRegAbxIncLB[i,j,k,] = (rotaOutLB/sum(popCut))[1:4e3]
        rotaRegAbxSumLB[i,j,k,] = rotaOutLB[1:4e3]
      }
    }
  }
}

spRegAbxInc = spRegAbxInc*100
spRegAbxSum = spRegAbxSum/1e6
rotaRegAbxInc = rotaRegAbxInc*100
rotaRegAbxSum = rotaRegAbxSum/1e6

spRegAbxIncLB = spRegAbxIncLB*100
spRegAbxSumLB = spRegAbxSumLB/1e6
rotaRegAbxIncLB = rotaRegAbxIncLB*100
rotaRegAbxSumLB = rotaRegAbxSumLB/1e6


maxdirSpInc = spRegAbxInc; maxdirSpSum = spRegAbxSum; maxdirRotaInc = rotaRegAbxInc; maxdirRotaSum = rotaRegAbxSum;
maxdirSpIncLB = spRegAbxIncLB; maxdirSpSumLB = spRegAbxSumLB; maxdirRotaIncLB = rotaRegAbxIncLB; maxdirRotaSumLB = rotaRegAbxSumLB;

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe')
save(maxdirSpInc,file='maxdirSpInc.Rdata'); save(maxdirSpSum,file='maxdirSpSum.Rdata')
save(maxdirRotaInc,file='maxdirRotaInc.Rdata'); save(maxdirRotaSum,file='maxdirRotaSum.Rdata')
save(maxdirSpIncLB,file='maxdirSpIncLB.Rdata'); save(maxdirSpSumLB,file='maxdirSpSumLB.Rdata')
save(maxdirRotaIncLB,file='maxdirRotaIncLB.Rdata'); save(maxdirRotaSumLB,file='maxdirRotaSumLB.Rdata')


function(x,places){
  out = round(x,places)
  if (out==round(x,0)){
    out = paste(out,'.0',sep='')
  }
  return(out)
}

dirSpIncLBout = dirSpSumLBout = maxdirSpIncLBout = maxdirSpSumLBout = c()
dirSpIncout = dirSpSumout = maxdirSpIncout = maxdirSpSumout = c()
for (i in 1:7){
  pos = (i-1)*5
  for (j in 1:4){
    if (mean(is.na(dirSpIncLB[i,1,j,]))!=1){
      dirSpIncLBout[pos+j] = paste(round.fn(quantile(dirSpIncLB[i,1,j,],0.5,na.rm=T),1),' (',
                                   round.fn(quantile(dirSpIncLB[i,1,j,],0.025,na.rm=T),1),', ',
                                   round.fn(quantile(dirSpIncLB[i,1,j,],0.975,na.rm=T),1),')',sep='')  
      dirSpSumLBout[pos+j] = paste(round.fn(quantile(dirSpSumLB[i,1,j,],0.5,na.rm=T),1),' (',
                                   round.fn(quantile(dirSpSumLB[i,1,j,],0.025,na.rm=T),1),', ',
                                   round.fn(quantile(dirSpSumLB[i,1,j,],0.975,na.rm=T),1),')',sep='') 
      dirSpIncout[pos+j] = paste(round.fn(quantile(dirSpInc[i,1,j,],0.5,na.rm=T),1),' (',
                                 round.fn(quantile(dirSpInc[i,1,j,],0.025,na.rm=T),1),', ',
                                 round.fn(quantile(dirSpInc[i,1,j,],0.975,na.rm=T),1),')',sep='')  
      dirSpSumout[pos+j] = paste(round.fn(quantile(dirSpSum[i,1,j,],0.5,na.rm=T),1),' (',
                                 round.fn(quantile(dirSpSum[i,1,j,],0.025,na.rm=T),1),', ',
                                 round.fn(quantile(dirSpSum[i,1,j,],0.975,na.rm=T),1),')',sep='') 
    }
    if (mean(is.na(maxdirSpIncLB[i,3,j,]))!=1){
      maxdirSpIncLBout[pos+j] = paste(round.fn(quantile(maxdirSpIncLB[i,3,j,],0.5,na.rm=T),1),' (',
                                      round.fn(quantile(maxdirSpIncLB[i,3,j,],0.025,na.rm=T),1),', ',
                                      round.fn(quantile(maxdirSpIncLB[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirSpSumLBout[pos+j] = paste(round.fn(quantile(maxdirSpSumLB[i,3,j,],0.5,na.rm=T),1),' (',
                                      round.fn(quantile(maxdirSpSumLB[i,3,j,],0.025,na.rm=T),1),', ',
                                      round.fn(quantile(maxdirSpSumLB[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirSpIncout[pos+j] = paste(round.fn(quantile(maxdirSpInc[i,3,j,],0.5,na.rm=T),1),' (',
                                    round.fn(quantile(maxdirSpInc[i,3,j,],0.025,na.rm=T),1),', ',
                                    round.fn(quantile(maxdirSpInc[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirSpSumout[pos+j] = paste(round.fn(quantile(maxdirSpSum[i,3,j,],0.5,na.rm=T),1),' (',
                                    round.fn(quantile(maxdirSpSum[i,3,j,],0.025,na.rm=T),1),', ',
                                    round.fn(quantile(maxdirSpSum[i,3,j,],0.975,na.rm=T),1),')',sep='')  
    }
  }
}

out = cbind(dirSpIncLBout,dirSpSumLBout,maxdirSpIncLBout,maxdirSpSumLBout,
            dirSpIncout,dirSpSumout,maxdirSpIncout,maxdirSpSumout)

write.csv(out,file='dirSp.csv')

dirRotaIncLBout = dirRotaSumLBout = maxdirRotaIncLBout = maxdirRotaSumLBout = c()
dirRotaIncout = dirRotaSumout = maxdirRotaIncout = maxdirRotaSumout = c()
for (i in 1:7){
  pos = (i-1)*5
  for (j in 1:4){
    if (mean(is.na(dirRotaIncLB[i,1,j,]))!=1){
      dirRotaIncLBout[pos+j] = paste(round.fn(quantile(dirRotaIncLB[i,1,j,],0.5,na.rm=T),1),' (',
                                     round.fn(quantile(dirRotaIncLB[i,1,j,],0.025,na.rm=T),1),', ',
                                     round.fn(quantile(dirRotaIncLB[i,1,j,],0.975,na.rm=T),1),')',sep='')  
      dirRotaSumLBout[pos+j] = paste(round.fn(quantile(dirRotaSumLB[i,1,j,],0.5,na.rm=T),1),' (',
                                     round.fn(quantile(dirRotaSumLB[i,1,j,],0.025,na.rm=T),1),', ',
                                     round.fn(quantile(dirRotaSumLB[i,1,j,],0.975,na.rm=T),1),')',sep='') 
      dirRotaIncout[pos+j] = paste(round.fn(quantile(dirRotaInc[i,1,j,],0.5,na.rm=T),1),' (',
                                   round.fn(quantile(dirRotaInc[i,1,j,],0.025,na.rm=T),1),', ',
                                   round.fn(quantile(dirRotaInc[i,1,j,],0.975,na.rm=T),1),')',sep='')  
      dirRotaSumout[pos+j] = paste(round.fn(quantile(dirRotaSum[i,1,j,],0.5,na.rm=T),1),' (',
                                   round.fn(quantile(dirRotaSum[i,1,j,],0.025,na.rm=T),1),', ',
                                   round.fn(quantile(dirRotaSum[i,1,j,],0.975,na.rm=T),1),')',sep='') 
    }
    if (mean(is.na(maxdirRotaIncLB[i,3,j,]))!=1){
      maxdirRotaIncLBout[pos+j] = paste(round.fn(quantile(maxdirRotaIncLB[i,3,j,],0.5,na.rm=T),1),' (',
                                        round.fn(quantile(maxdirRotaIncLB[i,3,j,],0.025,na.rm=T),1),', ',
                                        round.fn(quantile(maxdirRotaIncLB[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirRotaSumLBout[pos+j] = paste(round.fn(quantile(maxdirRotaSumLB[i,3,j,],0.5,na.rm=T),1),' (',
                                        round.fn(quantile(maxdirRotaSumLB[i,3,j,],0.025,na.rm=T),1),', ',
                                        round.fn(quantile(maxdirRotaSumLB[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirRotaIncout[pos+j] = paste(round.fn(quantile(maxdirRotaInc[i,3,j,],0.5,na.rm=T),1),' (',
                                      round.fn(quantile(maxdirRotaInc[i,3,j,],0.025,na.rm=T),1),', ',
                                      round.fn(quantile(maxdirRotaInc[i,3,j,],0.975,na.rm=T),1),')',sep='')  
      maxdirRotaSumout[pos+j] = paste(round.fn(quantile(maxdirRotaSum[i,3,j,],0.5,na.rm=T),1),' (',
                                      round.fn(quantile(maxdirRotaSum[i,3,j,],0.025,na.rm=T),1),', ',
                                      round.fn(quantile(maxdirRotaSum[i,3,j,],0.975,na.rm=T),1),')',sep='')  
    }
  }
}

out = cbind(dirRotaIncLBout,dirRotaSumLBout,maxdirRotaIncLBout,maxdirRotaSumLBout,
            dirRotaIncout,dirRotaSumout,maxdirRotaIncout,maxdirRotaSumout)

write.csv(out,file='dirRota.csv')
write.csv(cbind(country,ifelse(pcvImplemented==1,'Yes','No'),pcvcov*100,ifelse(rvImplemented==1,'Yes','No'),rvcov*100),
          file='vaxcov.csv')




#save(pop01,file='pop01.Rdata'); save(pop24,file='pop24.Rdata'); save(incomeStat,file='incomeStat.Rdata')
