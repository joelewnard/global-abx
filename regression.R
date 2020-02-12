setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')

load('dhsclean.Rdata')
load('mics6clean.Rdata')
load('mics5clean.Rdata')



dat = rbind(dhsclean[,c(1:37,39,40)],mics6clean[,1:39],mics5clean[,1:39])
dat$samplwt = c(dhsclean$samplwt,rep(1,length(mics6clean[,1])+length(mics5clean[,1])))



newclust = paste(dat$cluster,dat$country,dat$survey)
clusts = unique(newclust)

titles = names(dat)

for (i in 1:length(titles)){
  assign(titles[i],dat[,i])
}

#### diar as is


asxdiar = diar==0
#### trtdiar
trtdiar = dat$trtdiar
trtdiar[diar==0] = 0
#### trtabxdiar
trtabxdiar = rep(NA,length(abx))
trtabxdiar[abxdiar==1|ivabxdiar==1] = 1
trtabxdiar[abxdiar==0&ivabxdiar==0] = 0
trtabxdiar[asxdiar==1] = 0
trtabxdiar[diar==0] = 0


### standardize according to mics5

breath[cough==0] = 0
chest[breath==0] = 0
asxresp = chest==0&cough==0&breath==0&nose==0
lri = chest==1
lri[asxresp==1] = 0
trtresp[asxresp==1] = 0
trtlri = trtresp
trtlri[lri==0] = 0
trtlri[asxresp==1] = 0


trtabxlri = rep(NA,length(abx))
trtabxlri[abx==1|ivabx==1] = 1
trtabxlri[abx==0&ivabx==0] = 0
trtabxlri[lri==0] = 0
trtabxlri[asxresp==1] = 0



final.dat = data.frame(newclust,age,sex,wfh,
                       lri,trtlri,trtabxlri,antimalarial,fever,
                       diar,trtdiar,trtabxdiar,
                       month,year,sin6,cos6,sin12,cos12,
                       san,waterimproved,electric,solidfuel,
                       country,gbdregion,income,urban,
                       rvdoses,pcvdoses,pentadoses,poliodoses,wealth,motheredu,samplwt)



save(final.dat,file='final.dat.Rdata')
#### wealth index

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('final.dat.Rdata')


##### update pcv, rv where prevax
final.dat$pcvdoses[final.dat$country%in%c('Mongolia (Nalaikh District)','Mongolia (Khuvsgul Aimag)',
                                          'Tajikistan','Timor-Leste','Senegal','Myanmar','Maldives',
                                          'Haiti')] = 0
final.dat$rvdoses[final.dat$country%in%c('Afghanistan','Albania','Maldives','Myanmar',
                                         'Nepal','Philippines','Timor-Leste')] = 0
#### philippines: assume 0 among 0-2s. taken out of EPI in 2016. Surveys done mid-2017. Before
#### 2016 it was only being used in lowest wealth quintile and only regionally (Lena Lopez 2018)


########### missing rv vaccine data:
#### pakistan, el salvador, mexico, panama, paraguay, sudan, thailand, mauritania

########## missing pneumococcal vaccine data:
#### Mauritania, Mexico, Palestine, Panama, Paraguay, sudan

#### erroneous zeros on PCV: peru, yemen, senegal, rwanda, nigeria, niger, mozambique, mali, kenya
##################### guatemala, ghana, gambia, DRC
final.dat$pcvdoses[final.dat$country%in%c('Peru','Yemen','Senegal',
                                          'Rwanda','Nigeria','Niger','Mozambique',
                                          'Mali','Kenya','Guatemala','Ghana','Gambia',
                                          'Congo Democratic Republic')] = NA
#### consider honduras, dom rep, benin unvaccinated because w/in <1y and no kids 2-5 vaccinated

#### impute missing vaccination

final.dat$antimalarial[final.dat$country%in%c('Lesotho','Jordan','Albania','Armenia','Kyrgyz Republic','Montenegro','Kosovo','Cuba','Paraguay',
                                              'Mongolia','Mongolia (Khuvsgul Aimag)','Mongolia (Nalaikh District)')] = 0
final.dat$antimalarial[final.dat$fever==0] = 0

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('final.dat.Rdata')


regions = unique(final.dat$gbdregion)

gdpcap = c(569.5,4538,4465.7,3857.2,4132,1602.6,831.1,3412.8,654.9,
           312.5,1379.3,1441.4,810.2,6379.6,787.9,2005.3,478.2,1621.4,
           7477.6,2495,3243,872.8,7373.1,704.7,1662.6,4469.5,790.3,
           4578.2,783.8,2766.3,1976.1,3875.8,4135.6,1695.4,1207.8,1360.6,
           694.2,325.5,12527.2,813.3,426.1,1278.1,5859.5,848.1,438.4,
           1994.7,1545.9,6731.9,2988.9,771.7,1846.7,1331.3,487.6,
           6179.9,800.8,1034.2,2237,611.1,706.5,1043.1,1491.2,
           1185.3,2540.8,4806.5,8433,3894.7,793.6,3944.1,1271.3,
           9318.8,3639.9,3639.9,3639.9,7678.4,3094.7,15089.4,5600.1,
           1123.5,6590.6,6642.5,2353.4)

ddd = c(NA, # Afghanistan 2016
        NA, # Albania 2017
        NA, # Angola 2015
        NA, # Armenia 2016
        NA, # Azerbaijan 2006
        3960, # Bangladesh 2014
        1685, # Benin 2012
        NA, # Bolivia 2008
        1309, # Burkina 2010
        NA, # Burundi 2016
        NA, # Cambodia 2014
        NA, # Cameroon 2011
        NA, # Chad 2015
        2476, # Colombia 2010
        NA, # Comoros 2012
        NA, # Congo 2011
        NA, # DRC 2013
        1685, # Ivory Coast 2012
        2324, # Dominican Rep 2014
        8919, # Egypt 2014
        NA, # Eswatini 2006
        NA, # Ethiopia 2016
        NA, # Gabon 2012
        NA, # Gambia 2013
        NA, # Ghana 2014
        1695, # Guatemala 2015
        1685, # Guinea 2012
        NA, # Guyana 2009
        NA, # Haiti 2017
        1542, # Honduras 2012
        4950, # India 2015
        4112, # Indonesia 2012
        6199, # Jordan 2012
        NA, # Kenya 2014
        NA, # Kyrgyz 2012
        NA, # Lesotho 2014
        NA, # Liberia 2013
        NA, # Malawi 2015
        NA, # Maldives 2017
        1685, # Mali 2012
        NA, # Mozambique 2015
        NA, # Myanmar 2016
        NA, # Namibia 2013
        NA, # Nepal 2017
        2112, # Niger 2017
        NA, # Nigeria 2013
        7138, # Pakistan 2018
        3177, # Peru 2012
        NA, # PHilippines 2017
        NA, # Rwanda 2015
        NA, # Sao Tome 2008
        2112, # Senegal 2017
        NA, # Sierra leone 2015
        9177, # SOuth Af 2016
        NA, # tajikistan 2017
        NA, #Tanzania 2015
        NA, # Timor 2016
        NA, # togo 2014
        NA, # uganda 2016
        NA, # yemen 2013
        NA, # zambia 2013
        NA, # zimbabwe 2015
        NA, # Lao 2017
        1695, # Belize 2015
        NA, # Cuba 2014
        NA, # El salvador 2014
        2149, # Guinea bissau 2014
        NA, # kosovo 2014
        2112, # Mauritania 2015
        NA, # Mexico 2015
        2419, # mongolia 2015
        NA, 
        NA, 
        NA, #mongenegro 2013
        NA, # palestine 2014
        1695, # panama 2013
        NA, #paraguay 2016
        NA, # sudan 2014
        6682, # thailand 2015
        NA, # turkmenistan 2015
        9372) # Vietnam 2014

gdpregion = dddregion = array(0,dim=c(dim(final.dat)[1],length(regions)))
for (i in 1:length(regions)){
  gdpregion[,i] = gdpcap[i]
  gdpregion[which(final.dat$gbdregion!=regions[i]),i] = 0
  dddregion[,i] = ddd[i]
  dddregion[which(final.dat$gbdregion!=regions[i]),i] = 0
}
gdpCap = dddCap = c()
for (i in 1:length(unique(final.dat$country))){
  gdpCap[final.dat$country==unique(final.dat$country)[i]] = gdpcap[i]
  dddCap[final.dat$country==unique(final.dat$country)[i]] = ddd[i]
}



for.impute = data.frame(sex=as.character(final.dat$sex), 
                        lri = as.character(final.dat$lri), 
                        trtlri = as.character(final.dat$trtlri), 
                        trtabxlri = as.character(final.dat$trtabxlri), 
                        diar = as.character(final.dat$diar),
                        trtdiar = as.character(final.dat$trtdiar), 
                        trtabxdiar = as.character(final.dat$trtabxdiar), 
                        san=final.dat$san, 
                        waterimproved=as.character(final.dat$waterimproved), 
                        electric=as.character(final.dat$electric), 
                        solidfuel=as.character(final.dat$solidfuel), 
                        rvdoses=as.character(final.dat$rvdoses), 
                        pcvdoses=as.character(final.dat$pcvdoses), 
                        pentadoses=as.character(final.dat$pentadoses),
                        poliodoses=as.character(final.dat$poliodoses),
                        urban=as.character(final.dat$urban), 
                        gbdregion=as.character(final.dat$gbdregion),
                        motheredu=as.character(final.dat$motheredu),
                        age=final.dat$age, 
                        year=final.dat$year,wfh=final.dat$wfh,
                        antimalarial=as.character(final.dat$antimalarial),
                        fever=as.character(final.dat$fever),
                        wealth=final.dat$wealth,
                        gdp=gdpCap,ddd=dddCap)
                        
countries = unique(final.dat$country)
for.impute$rvcov = for.impute$pcvcov = for.impute$sannatU = for.impute$sannatR = for.impute$waternatU = for.impute$waternatR = NA
for (i in 1:length(countries)){
  for.impute$rvcov[which(final.dat$country==countries[i])] = mean(for.impute$rvdoses[which(final.dat$country==countries[i]&for.impute$age>0.5)]=='2',na.rm=T)
  for.impute$pcvcov[which(final.dat$country==countries[i])] = mean(for.impute$pcvdoses[which(final.dat$country==countries[i]&for.impute$age>0.75)]=='3',na.rm=T)
  for.impute$sannatU[which(final.dat$country==countries[i])] = mean(for.impute$san[which(final.dat$country==countries[i]&for.impute$urban=='1')]%in%c('Open defecation','Unimproved')==F,na.rm=T)
  for.impute$sannatR[which(final.dat$country==countries[i])] = mean(for.impute$san[which(final.dat$country==countries[i]&for.impute$urban=='0')]%in%c('Open defecation','Unimproved')==F,na.rm=T)
  for.impute$waternatU[which(final.dat$country==countries[i])] = mean(for.impute$waterimproved[which(final.dat$country==countries[i]&for.impute$urban=='1')]=='1',na.rm=T)
  for.impute$waternatR[which(final.dat$country==countries[i])] = mean(for.impute$waterimproved[which(final.dat$country==countries[i]&for.impute$urban=='0')]=='1',na.rm=T)
}

for.impute$waterimproved[final.dat$country=='Cambodia'] = NA
for.impute$waternatU[final.dat$country=='Cambodia'] = NA
for.impute$waternatR[final.dat$country=='Cambodia'] = NA



out = c()
for (i in 1:dim(for.impute)[2]){
  out[i] = is.numeric(for.impute[,i])
}

library(Amelia)
library(parallel)
set.seed(1); imputedAll = amelia(for.impute,noms=which(out==0),m=5,parallel='multicore',ncpus=5,p2s=2)

out = c()
for (i in 1:length(countries)){
  out[i] = mean(imputedAll[[1]]$imp1$trtabxlri[final.dat$country==countries[i]&imputedAll[[1]]$imp1$lri=='1']=='1')
}
cbind(out,as.character(countries))
#save(imputedAll,file='imputedAll.Rdata')



###############################################################
####### country by country generate assoc with risk factors ###
###############################################################

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('imputedAll.Rdata')
#imputedExtend = imputedAll; save(imputedExtend,file='imputedExtend.Rdata')

load('imputedExtend.Rdata')
load('final.dat.Rdata')

gdpcap = c(569.5,4538,4465.7,3857.2,4132,1602.6,831.1,3412.8,654.9,
           312.5,1379.3,1441.4,810.2,6379.6,787.9,2005.3,478.2,1621.4,
           7477.6,2495,3243,872.8,7373.1,704.7,1662.6,4469.5,790.3,
           4578.2,783.8,2766.3,1976.1,3875.8,4135.6,1695.4,1207.8,1360.6,
           694.2,325.5,12527.2,813.3,426.1,1278.1,5859.5,848.1,438.4,
           1994.7,1545.9,6731.9,2988.9,771.7,1846.7,1331.3,487.6,
           6179.9,800.8,1034.2,2237,611.1,706.5,1043.1,1491.2,
           1185.3,2540.8,4806.5,8433,3894.7,793.6,3944.1,1271.3,
           9318.8,3639.9,3639.9,3639.9,7678.4,3094.7,15089.4,5600.1,
           1123.5,6590.6,6642.5,2353.4)
countries = as.character(unique(final.dat$country))

gdp = c()
for (i in 1:length(countries)){
  gdp[which(final.dat$country==countries[i])] = gdpcap[i]
}

#imputedExtend[[1]]$imp1$ddd[imputedExtend[[1]]$imp1$ddd<1] = 1
#imputedExtend[[1]]$imp2$ddd[imputedExtend[[1]]$imp2$ddd<1] = 1
#imputedExtend[[1]]$imp3$ddd[imputedExtend[[1]]$imp3$ddd<1] = 1
#imputedExtend[[1]]$imp4$ddd[imputedExtend[[1]]$imp4$ddd<1] = 1
#imputedExtend[[1]]$imp5$ddd[imputedExtend[[1]]$imp5$ddd<1] = 1
#save(imputedExtend,file='imputedExtend.Rdata')
#load('imputedExtend.Rdata')

##### DIAR INC MODEL

data = imputedExtend[[1]]$imp2
data$y=as.numeric(data$diar)-1
data$wealth = data$wealth/1e7

data$gbdregion[data$gbdregion=='CentralEurope'] = 'EasternEurope'; data$gbdregion[data$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
data$gbdregion[data$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
data$gbdregion[data$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
data$gbdregion[data$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
mod = glm(y~gbdregion+wfh+san+urban+waterimproved*log(gdp)+electric+
            age+san*log(gdp)+motheredu*wealth+
            year,
          data=data,family='poisson',
          subset=(age<2&rvdoses=='0'&pentadoses!='0'&poliodoses!='0')); summary(mod)
diarVarNames = names(coef(mod)); save(diarVarNames,file='diarVarNames.Rdata')


##### DIAR TRT MODEL
data = imputedExtend[[1]]$imp2
data$y=as.numeric(data$trtabxdiar)-1
data$wealth = data$wealth/1e7
data$gbdregion[data$gbdregion=='CentralEurope'] = 'EasternEurope'; data$gbdregion[data$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
data$gbdregion[data$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
data$gbdregion[data$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
data$gbdregion[data$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'

mod = glm(y~gbdregion*log(ddd)+wfh+urban*log(gdp)+
            electric+wealth+
            log(gdp)+age+motheredu+
            year,
          data=data,family='poisson',
          subset=(age<2&diar=='1')); summary(mod)
trtdiarVarNames = names(coef(mod)); save(trtdiarVarNames,file='trtdiarVarNames.Rdata')




data = imputedExtend[[1]]$imp2
data$y=(as.numeric(data$lri)-1)
data$wealth = data$wealth/1e7
data$gbdregion[data$gbdregion=='CentralEurope'] = 'EasternEurope'; data$gbdregion[data$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
data$gbdregion[data$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
data$gbdregion[data$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
data$gbdregion[data$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
#data$gdpcap = data$gdpcap + runif(dim(data)[1],-0.02,0.02)
mod = glm(y~gbdregion+wfh+urban+solidfuel+
            log(gdp)+age+motheredu+
            year,
          data=data,family='poisson',
          subset=(age>=2&pcvdoses=='0'&pentadoses=='3'&poliodoses=='3')); summary(mod)
lriVarNames = names(coef(mod)); save(lriVarNames,file='lriVarNames.Rdata')

mod = glm(y~gbdregion+wfh+urban+solidfuel+
            log(gdp)+age+motheredu+
            year,
          data=data,family='poisson',
          subset=(pcvdoses=='0'&pentadoses!='0'&poliodoses!='0')); summary(mod)
lri059VarNames = names(coef(mod)); save(lri059VarNames,file='lri059VarNames.Rdata')


mod = glm(y~gbdregion+wfh+urban+solidfuel+
            log(gdp)+age+motheredu+
            year,
          data=data,family='poisson',
          subset=(pcvdoses=='0'&pentadoses!='0'&poliodoses!='0'&age<2)); summary(mod)
lri024VarNames = names(coef(mod)); save(lri024VarNames,file='lri024VarNames.Rdata')



data = imputedExtend[[1]]$imp3
data$y=(as.numeric(data$trtabxlri)-1)*(data$antimalarial=='0')
data$wealth = data$wealth/1e7
data$gbdregion[data$gbdregion=='CentralEurope'] = 'EasternEurope'; data$gbdregion[data$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
data$gbdregion[data$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
data$gbdregion[data$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
data$gbdregion[data$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
mod = glm(y~gbdregion+urban+
            log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
            year,
          data=data,family='poisson',
          subset=(age>=2&lri=='1')); summary(mod)
trtlriVarNames = names(coef(mod)); save(trtlriVarNames,file='trtlriVarNames.Rdata')

mod = glm(y~gbdregion+urban+
            log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
            year,
          data=data,family='poisson',
          subset=(lri=='1')); summary(mod)
trtlri059VarNames = names(coef(mod)); save(trtlri059VarNames,file='trtlri059VarNames.Rdata')

mod = glm(y~gbdregion+urban+
            log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
            year,
          data=data,family='poisson',
          subset=(age<2&lri=='1')); summary(mod)
length(coef(mod))
trtlri024VarNames = names(coef(mod)); save(trtlri024VarNames,file='trtlri024VarNames.Rdata')



set.seed(1)
parsDiar = array(NA,dim=c(5,length(diarVarNames))); vcovDiar = array(NA,dim=c(5,rep(length(diarVarNames),2)))
parsTrtDiar = array(NA,dim=c(5,length(trtdiarVarNames))); vcovTrtDiar = array(NA,dim=c(5,rep(length(trtdiarVarNames),2)))
parsLri = array(NA,dim=c(5,length(lriVarNames))); vcovLri = array(NA,dim=c(5,rep(length(lriVarNames),2)))
parsTrtLri = array(NA,dim=c(5,length(trtlriVarNames))); vcovTrtLri = array(NA,dim=c(5,rep(length(trtlriVarNames),2)))
parsLri059 = array(NA,dim=c(5,length(lri059VarNames))); vcovLri059 = array(NA,dim=c(5,rep(length(lri059VarNames),2)))
parsTrtLri059 = array(NA,dim=c(5,length(trtlri059VarNames))); vcovTrtLri059 = array(NA,dim=c(5,rep(length(trtlri059VarNames),2)))

parsLri024 = array(NA,dim=c(5,length(lri024VarNames))); vcovLri024 = array(NA,dim=c(5,rep(length(lri024VarNames),2)))
parsTrtLri024 = array(NA,dim=c(5,length(trtlri024VarNames))); vcovTrtLri024 = array(NA,dim=c(5,rep(length(trtlri024VarNames),2)))

for (j in 1:5){
  
  if (j==1){
    data = imputedExtend[[1]]$imp1
  } else{
    if (j==2){
      data = imputedExtend[[1]]$imp2
    } else{
      if (j==3){
        data = imputedExtend[[1]]$imp3
      } else{
        if (j==4){
          data = imputedExtend[[1]]$imp4
        } else{
          if (j==5){
            data = imputedExtend[[1]]$imp5
          }
        }
      }
    }
  }
  
  
  
  data$wealth = data$wealth/1e7
  data$gbdregion[data$gbdregion=='CentralEurope'] = 'EasternEurope'; data$gbdregion[data$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
  data$gbdregion[data$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
  data$gbdregion[data$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
  data$gbdregion[data$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
  data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
  
  ####### incidence
  data$y=as.numeric(data$diar)-1
  mod = glm(y~gbdregion+wfh+san+urban+waterimproved*log(gdp)+electric+
              age+san*log(gdp)+motheredu*wealth+
              year,
            data=data,family='poisson',
            subset=(age<2&rvdoses=='0'&pentadoses!='0'&poliodoses!='0'))
  parsDiar[j,] = coef(mod)
  vcovDiar[j,,] = vcov(mod)
  
  
  ####### antibiotic use
  data$y=as.numeric(data$trtabxdiar)-1
  mod = glm(y~gbdregion*log(ddd)+wfh+urban*log(gdp)+
              electric+wealth+
              log(gdp)+age+motheredu+
              year,
            data=data,family='poisson',
            subset=(age<2&diar=='1')); 
  parsTrtDiar[j,] = coef(mod)
  vcovTrtDiar[j,,] = vcov(mod)
  
  #### lri
  
  ######## incidence
  data$y=as.numeric(data$lri)-1
  mod = glm(y~gbdregion+wfh+urban+solidfuel+
              log(gdp)+age+motheredu+
              year,
            data=data,family='poisson',
            subset=(age>=2&pcvdoses=='0'&pentadoses=='3'&poliodoses=='3'))
  parsLri[j,] = coef(mod)
  vcovLri[j,,] = vcov(mod)
  
  ######### antibiotic use
  data$y=(as.numeric(data$trtabxlri)-1)*(data$antimalarial=='0')
  mod = glm(y~gbdregion+urban+
              log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
              year,
            data=data,family='poisson',
            subset=(age>=2&lri=='1'));
  parsTrtLri[j,] = coef(mod)
  vcovTrtLri[j,,] = vcov(mod)
  
  #### lri ages 0-59m
  
  ######## incidence
  data$y=as.numeric(data$lri)-1
  mod = glm(y~gbdregion+wfh+urban+solidfuel+
              log(gdp)+age+motheredu+
              year,
            data=data,family='poisson',
            subset=(pcvdoses=='0'&pentadoses=='3'&poliodoses=='3'))
  parsLri059[j,] = coef(mod)
  vcovLri059[j,,] = vcov(mod)
  
  ######### antibiotic use
  data$y=(as.numeric(data$trtabxlri)-1)*(data$antimalarial=='0')
  mod = glm(y~gbdregion+urban+
              log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
              year,
            data=data,family='poisson',
            subset=(lri=='1'));
  parsTrtLri059[j,] = coef(mod)
  vcovTrtLri059[j,,] = vcov(mod)
  
  #### lri ages 0-24m
  
  ######## incidence
  data$y=as.numeric(data$lri)-1
  mod = glm(y~gbdregion+wfh+urban+solidfuel+
              log(gdp)+age+motheredu+
              year,
            data=data,family='poisson',
            subset=(pcvdoses=='0'&pentadoses!='0'&poliodoses!='0'))
  parsLri024[j,] = coef(mod)
  vcovLri024[j,,] = vcov(mod)
  
  ######### antibiotic use
  data$y=(as.numeric(data$trtabxlri)-1)*(data$antimalarial=='0')
  mod = glm(y~gbdregion+urban+
              log(gdp)+age+motheredu+wealth+log(ddd)*gbdregion+fever+
              year,
            data=data,family='poisson',
            subset=(lri=='1'));
  parsTrtLri024[j,] = coef(mod)
  vcovTrtLri024[j,,] = vcov(mod)
  
  print(j)
}

save(parsDiar,file='parsDiar.Rdata')
save(parsTrtDiar,file='parsTrtDiar.Rdata')
save(parsLri,file='parsLri.Rdata')
save(parsTrtLri,file='parsTrtLri.Rdata')

save(parsLri059,file='parsLri059.Rdata')
save(parsTrtLri059,file='parsTrtLri059.Rdata')
save(parsLri024,file='parsLri024.Rdata')
save(parsTrtLri024,file='parsTrtLri024.Rdata')

save(vcovDiar,file='vcovDiar.Rdata')
save(vcovTrtDiar,file='vcovTrtDiar.Rdata')
save(vcovLri,file='vcovLri.Rdata')
save(vcovTrtLri,file='vcovTrtLri.Rdata')
save(vcovLri059,file='vcovLri059.Rdata')
save(vcovTrtLri059,file='vcovTrtLri059.Rdata')
save(vcovLri024,file='vcovLri024.Rdata')
save(vcovTrtLri024,file='vcovTrtLri024.Rdata')

###########################################################
###########################################################
#### long thing to run here ###############################
###########################################################
###########################################################

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')

load('parsDiar.Rdata')
load('parsTrtDiar.Rdata')
load('parsLri.Rdata')
load('parsTrtLri.Rdata')

load('parsLri059.Rdata')
load('parsTrtLri059.Rdata')
load('parsLri024.Rdata')
load('parsTrtLri024.Rdata')

load('vcovDiar.Rdata')
load('vcovTrtDiar.Rdata')
load('vcovLri.Rdata')
load('vcovTrtLri.Rdata')
load('vcovLri059.Rdata')
load('vcovTrtLri059.Rdata')
load('vcovLri024.Rdata')
load('vcovTrtLri024.Rdata')



incmodel.fn = function(n){
  
  diarInc = lriInc = lriInc059 = lriInc024 = diarAbx = lriAbx = lriAbx059 = lriAbx024 = array(NA,dim=c(5,length(countries)))
  
  for (i in 1:length(countries)){
    if (i%in%c(72,73,76,78)==F){
      for (m in 1:5){
        data = imputedExtend[[1]]
        dat = data[[m]]
        dat$wealth = dat$wealth/1e7
        dat$gbdregion[dat$gbdregion=='CentralEurope'] = 'EasternEurope'; dat$gbdregion[dat$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
        dat$gbdregion[dat$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
        dat$gbdregion[dat$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
        dat$gbdregion[dat$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
        data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
        
        
           ##### inc of diarrhea
           ind = which(final.dat$country==countries[i]&dat$age<2)
           sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
           xMat = cbind(rep(1,length(ind)),
                        dat$wfh[sels],
                        dat$san[sels]=='Open defecation',
                        dat$san[sels]=='Unimproved',
                        dat$urban[sels]=='1',
                        dat$waterimproved[sels]=='1',
                        log(dat$gdp[sels]),
                        dat$electric[sels]=='1',
                        dat$age[sels],
                        dat$motheredu[sels]%in%c('0','1'), ### mothere
                        dat$motheredu[sels]=='2',
                        dat$motheredu[sels]=='3',
                        dat$wealth[sels],
                        rep(2016,length(ind)), ### year
                        log(dat$gdp[sels])*(dat$waterimproved[sels]=='1'),
                        log(dat$gdp[sels])*(dat$san[sels]=='Open defecation'),
                        log(dat$gdp[sels])*(dat$san[sels]=='Unimproved'),
                        (dat$motheredu[sels]%in%c('0','1'))*dat$wealth[sels],
                        (dat$motheredu[sels]=='2')*dat$wealth[sels],
                        (dat$motheredu[sels]=='3')*dat$wealth[sels])
           diarBeta = mvrnorm(1,parsDiar[m,],vcovDiar[m,,])[c(1,7:25)]
           totDiar = exp(xMat%*%diarBeta)*26
           totDiar = rpois(length(totDiar),totDiar)
           diarInc[m,i] = mean(totDiar)
        
          #### abx treat for diarrhea
          ind = which(final.dat$country==countries[i]&dat$age<2&dat$diar=='1')
          sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
          xMat = cbind(rep(1,length(ind)),
                       dat$gbdregion[sels]=='CentralAsia',
                       dat$gbdregion[sels]=='CentralLatinAmerica',
                       dat$gbdregion[sels]=='CentralSubSaharanAfrica',
                       dat$gbdregion[sels]=='EasternEurope',
                       dat$gbdregion[sels]=='SouthAsia',
                       log(dat$ddd[sels]),
                       dat$wfh[sels],
                       dat$urban[sels]=='1',
                       log(dat$gdp[sels]),
                       dat$electric[sels]=='1',
                       dat$wealth[sels],
                       dat$age[sels],
                       dat$motheredu[sels]=='1',
                       dat$motheredu[sels]=='2',
                       dat$motheredu[sels]=='3',
                       2016,
                       (dat$gbdregion[sels]=='CentralAsia')*log(dat$ddd[sels]),
                       (dat$gbdregion[sels]=='CentralLatinAmerica')*log(dat$ddd[sels]),
                       (dat$gbdregion[sels]=='CentralSubSaharanAfrica')*log(dat$ddd[sels]),
                       (dat$gbdregion[sels]=='EasternEurope')*log(dat$ddd[sels]),
                       (dat$gbdregion[sels]=='SouthAsia')*log(dat$ddd[sels]),
                       log(dat$gdp[sels])*(dat$urban[sels]=='1'))
          trtdiarBeta = mvrnorm(1,parsTrtDiar[m,],vcovTrtDiar[m,,])
          trtDiar = exp(xMat%*%trtdiarBeta); #trtDiar[trtDiar>1] = 1
          trtDiar = runif(length(trtDiar),0,1)<trtDiar
          diarAbx[m,i] = mean(trtDiar)
        
        
            #### lri incidence
            ind = which(final.dat$country==countries[i]&dat$age>=2)
            sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
            xMat = cbind(rep(1,length(ind)),
                         rep(1,length(ind)), ### standardize to CentralAsia, where rep highest (=intercept)
                         dat$wfh[sels],
                         dat$urban[sels]=='1',
                         dat$solidfuel[sels]=='1',
                         log(dat$gdp[sels]),
                         dat$age[sels],
                         (dat$motheredu[sels]%in%c('0','1')),
                         dat$motheredu[sels]=='2',
                         dat$motheredu[sels]=='3',
                         rep(2016,length(ind)))
            lriBeta = mvrnorm(1,parsLri[m,],vcovLri[m,,])[c(1,2,7:15)]
            totLri = exp(xMat%*%lriBeta)*26
            totLri = rpois(length(totLri),totLri)
            lriInc[m,i] = mean(totLri)
        
                #    ##### lri treatment
            ind = which(final.dat$country==countries[i]&dat$age>=2&dat$lri=='1')
            sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
            xMat = cbind(rep(1,length(ind)),
                         dat$gbdregion[sels]=='CentralAsia',
                         dat$gbdregion[sels]=='CentralLatinAmerica',
                         dat$gbdregion[sels]=='CentralSubSaharanAfrica',
                         dat$gbdregion[sels]=='EasternEurope',
                         dat$gbdregion[sels]=='SouthAsia',
                         dat$urban[sels]=='1',
                         log(dat$gdp[sels]),
                         dat$age[sels],
                         dat$motheredu[sels]=='1',
                         dat$motheredu[sels]=='2',
                         dat$motheredu[sels]=='3',
                         dat$wealth[sels],
                         log(dat$ddd[sels]),
                         dat$fever[sels]=='1',
                         rep(2016,length(ind)),
                         (dat$gbdregion[sels]=='CentralAsia')*log(dat$ddd[sels]),
                         (dat$gbdregion[sels]=='CentralLatinAmerica')*log(dat$ddd[sels]),
                         (dat$gbdregion[sels]=='CentralSubSaharanAfrica')*log(dat$ddd[sels]),
                         (dat$gbdregion[sels]=='EasternEurope')*log(dat$ddd[sels]),
                         (dat$gbdregion[sels]=='SouthAsia')*log(dat$ddd[sels]))
            trtlriBeta = mvrnorm(1,parsTrtLri[m,],vcovTrtLri[m,,])
            trtLri = exp(xMat%*%trtlriBeta); #trtLri[trtLri>1] = 1
            trtLri = runif(length(trtLri),0,1)<trtLri
            lriAbx[m,i] = mean(trtLri)
        
        
         #### lri incidence ages 0-59m
         ind = which(final.dat$country==countries[i])
         sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
         xMat = cbind(rep(1,length(ind)),
                      rep(1,length(ind)), ### standardize to CentralAsia, where rep highest (=intercept)
                      dat$wfh[sels],
                      dat$urban[sels]=='1',
                      dat$solidfuel[sels]=='1',
                      log(dat$gdp[sels]),
                      dat$age[sels],
                      (dat$motheredu[sels]%in%c('0','1')),
                      dat$motheredu[sels]=='2',
                      dat$motheredu[sels]=='3',
                      rep(2016,length(ind)))
         lri059Beta = mvrnorm(1,parsLri059[m,],vcovLri059[m,,])[c(1,2,7:15)]
         totLri059 = exp(xMat%*%lri059Beta)*26
         totLri059 = rpois(length(totLri059),totLri059)
         lriInc059[m,i] = mean(totLri059)
        
         ##### lri treatment
         ind = which(final.dat$country==countries[i]&dat$lri=='1')
         sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
         xMat = cbind(rep(1,length(ind)),
                      dat$gbdregion[sels]=='CentralAsia',
                      dat$gbdregion[sels]=='CentralLatinAmerica',
                      dat$gbdregion[sels]=='CentralSubSaharanAfrica',
                      dat$gbdregion[sels]=='EasternEurope',
                      dat$gbdregion[sels]=='SouthAsia',
                      dat$urban[sels]=='1',
                      log(dat$gdp[sels]),
                      dat$age[sels],
                      dat$motheredu[sels]=='1',
                      dat$motheredu[sels]=='2',
                      dat$motheredu[sels]=='3',
                      dat$wealth[sels],
                      log(dat$ddd[sels]),
                      dat$fever[sels]=='1',
                      rep(2016,length(ind)),
                      (dat$gbdregion[sels]=='CentralAsia')*log(dat$ddd[sels]),
                      (dat$gbdregion[sels]=='CentralLatinAmerica')*log(dat$ddd[sels]),
                      (dat$gbdregion[sels]=='CentralSubSaharanAfrica')*log(dat$ddd[sels]),
                      (dat$gbdregion[sels]=='EasternEurope')*log(dat$ddd[sels]),
                      (dat$gbdregion[sels]=='SouthAsia')*log(dat$ddd[sels]))
         trtlri059Beta = mvrnorm(1,parsTrtLri059[m,],vcovTrtLri059[m,,])
         trtLri059 = exp(xMat%*%trtlri059Beta); #trtLri[trtLri>1] = 1
         trtLri059 = runif(length(trtLri059),0,1)<trtLri059
         lriAbx059[m,i] = mean(trtLri059)
       
         #### lri incidence ages 0-24m
         ind = which(final.dat$country==countries[i]&dat$age<2)
         sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
         xMat = cbind(rep(1,length(ind)),
                      rep(1,length(ind)), ### standardize to CentralAsia, where rep highest (=intercept)
                      dat$wfh[sels],
                      dat$urban[sels]=='1',
                      dat$solidfuel[sels]=='1',
                      log(dat$gdp[sels]),
                      dat$age[sels],
                      (dat$motheredu[sels]%in%c('0','1')),
                      dat$motheredu[sels]=='2',
                      dat$motheredu[sels]=='3',
                      rep(2016,length(ind)))
         lri024Beta = mvrnorm(1,parsLri024[m,],vcovLri024[m,,])[c(1,2,7:15)]
         totLri024 = exp(xMat%*%lri024Beta)*26
         totLri024 = rpois(length(totLri024),totLri024)
         lriInc024[m,i] = mean(totLri024)
        
         ##### lri treatment ages 0-24m 
         ind = which(final.dat$country==countries[i]&dat$lri=='1'&dat$age<2)
         if (length(ind)>1){
           sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
           xMat = cbind(rep(1,length(ind)),
                        dat$gbdregion[sels]=='CentralAsia',
                        dat$gbdregion[sels]=='CentralLatinAmerica',
                        dat$gbdregion[sels]=='CentralSubSaharanAfrica',
                        dat$gbdregion[sels]=='EasternEurope',
                        dat$gbdregion[sels]=='SouthAsia',
                        dat$urban[sels]=='1',
                        log(dat$gdp[sels]),
                        dat$age[sels],
                        dat$motheredu[sels]=='1',
                        dat$motheredu[sels]=='2',
                        dat$motheredu[sels]=='3',
                        dat$wealth[sels],
                        log(dat$ddd[sels]),
                        dat$fever[sels]=='1',
                        rep(2016,length(ind)),
                        (dat$gbdregion[sels]=='CentralAsia')*log(dat$ddd[sels]),
                        (dat$gbdregion[sels]=='CentralLatinAmerica')*log(dat$ddd[sels]),
                        (dat$gbdregion[sels]=='CentralSubSaharanAfrica')*log(dat$ddd[sels]),
                        (dat$gbdregion[sels]=='EasternEurope')*log(dat$ddd[sels]),
                        (dat$gbdregion[sels]=='SouthAsia')*log(dat$ddd[sels]))
           trtlri024Beta = mvrnorm(1,parsTrtLri024[m,],vcovTrtLri024[m,,])
           trtLri024 = exp(xMat%*%trtlri024Beta); #trtLri[trtLri>1] = 1
           trtLri024 = runif(length(trtLri024),0,1)<trtLri024
           lriAbx024[m,i] = mean(trtLri024) 
         }
        
        print(c(m,i))
      } 
    }
  }
  return(list(diarInc,diarAbx,lriInc,lriAbx,lriInc059,lriAbx059,lriInc024,lriAbx024))
}

library(parallel)

tryout = incmodel.fn(1)

set.seed(1)
diarInc = diarAbx = lriInc = lriAbx = lriInc059 = lriAbx059 = array(NA,dim=c(1e3,5,81))
lriInc059new = lriAbx059new = array(NA,dim=c(1e3,5,81))
lriInc024 = lriAbx024 = array(NA,dim=c(1e3,5,81))
num = 1:10
for (i in 1:100){
  
  out = mclapply(num, function(num) incmodel.fn(num),mc.cores=10)
  
  for (j in 1:10){
    diarInc[(i-1)*10+j,,] = out[[j]][[1]]
    diarAbx[(i-1)*10+j,,] = out[[j]][[2]]
    lriInc[(i-1)*10+j,,] = out[[j]][[3]]
    lriAbx[(i-1)*10+j,,] = out[[j]][[4]]
    lriInc059[(i-1)*10+j,,] = out[[j]][[5]]
    lriAbx059[(i-1)*10+j,,] = out[[j]][[6]]
    lriInc024[(i-1)*10+j,,] = out[[j]][[7]]
    lriAbx024[(i-1)*10+j,,] = out[[j]][[8]]
  }
  
  save(diarInc,file='diarInc.Rdata')
  save(diarAbx,file='diarAbx.Rdata')
  save(lriInc,file='lriInc.Rdata')
  save(lriAbx,file='lriAbx.Rdata')
  save(lriInc059,file='lriInc059.Rdata')
  save(lriAbx059,file='lriAbx059.Rdata')
  save(lriInc024,file='lriInc024.Rdata')
  save(lriAbx024,file='lriAbx024.Rdata')
  print(i)
}

######### estimates from above are UPPER BOUND based on highest intercept (for reporting rate)
######### below, estimates are LOWER BOUND based on lowest intercept (for reporting rate)

load('parsDiar.Rdata'); load('vcovDiar.Rdata')
load('parsLri.Rdata'); load('vcovLri.Rdata')
load('parsLri059.Rdata'); load('vcovLri059.Rdata')
load('parsLri024.Rdata'); load('vcovLri024.Rdata')

incmodel.fn = function(n){
  
  diarInc = lriInc = lriInc059 = lriInc024 = array(NA,dim=c(5,length(countries)))
  
  for (i in 1:length(countries)){
    if (i%in%c(72,73,76,78)==F){
      for (m in 1:5){
        data = imputedExtend[[1]]
        dat = data[[m]]
        dat$wealth = dat$wealth/1e7
        dat$gbdregion[dat$gbdregion=='CentralEurope'] = 'EasternEurope'; dat$gbdregion[dat$gbdregion=='TropicalLatinAmerica'] = 'AndeanLatinAmerica'
        dat$gbdregion[dat$gbdregion%in%c('CentralSubSaharanAfrica','WesternSubSaharanAfrica','EasternSubSaharanAfrica','SouthernSubSaharanAfrica')] = 'CentralSubSaharanAfrica'
        dat$gbdregion[dat$gbdregion%in%c('CentralAsia','NorthAfricaMiddleEast')] = 'CentralAsia'
        dat$gbdregion[dat$gbdregion%in%c('Caribbean','CentralLatinAmerica')] = 'CentralLatinAmerica'
        data$gbdregion[data$gbdregion=='SoutheastAsia'] = 'SouthAsia'
        
        
        ##### inc of diarrhea
        ind = which(final.dat$country==countries[i]&dat$age<2)
        sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
        xMat = cbind(rep(1,length(ind)),rep(1,length(ind)),
                     dat$wfh[sels],
                    dat$san[sels]=='Open defecation',
                     dat$san[sels]=='Unimproved',
                     dat$urban[sels]=='1',
                     dat$waterimproved[sels]=='1',
                     log(dat$gdp[sels]),
                     dat$electric[sels]=='1',
                     dat$age[sels],
                     dat$motheredu[sels]%in%c('0','1'), ### mothere
                     dat$motheredu[sels]=='2',
                     dat$motheredu[sels]=='3',
                     dat$wealth[sels],
                     rep(2016,length(ind)), ### year
                     log(dat$gdp[sels])*(dat$waterimproved[sels]=='1'),
                     log(dat$gdp[sels])*(dat$san[sels]=='Open defecation'),
                     log(dat$gdp[sels])*(dat$san[sels]=='Unimproved'),
                     (dat$motheredu[sels]%in%c('0','1'))*dat$wealth[sels],
                     (dat$motheredu[sels]=='2')*dat$wealth[sels],
                     (dat$motheredu[sels]=='3')*dat$wealth[sels])
        diarBeta = mvrnorm(1,parsDiar[m,],vcovDiar[m,,])[c(1,5,7:25)]
        totDiar = exp(xMat%*%diarBeta)*26
        totDiar = rpois(length(totDiar),totDiar)
        diarInc[m,i] = mean(totDiar)
        
        #### lri incidence
        ind = which(final.dat$country==countries[i]&dat$age>=2)
        sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
        xMat = cbind(rep(1,length(ind)),
                     rep(1,length(ind)), ### standardize to SouthAsia, where rep lowest (=intercept)
                     dat$wfh[sels],
                     dat$urban[sels]=='1',
                     dat$solidfuel[sels]=='1',
                     log(dat$gdp[sels]),
                     dat$age[sels],
                     (dat$motheredu[sels]%in%c('0','1')),
                     dat$motheredu[sels]=='2',
                     dat$motheredu[sels]=='3',
                     rep(2016,length(ind)))
        lriBeta = mvrnorm(1,parsLri[m,],vcovLri[m,,])[c(1,6,7:15)]
        totLri = exp(xMat%*%lriBeta)*26
        totLri = rpois(length(totLri),totLri)
        lriInc[m,i] = mean(totLri)
        
        #### lri incidence ages 0-59m
        ind = which(final.dat$country==countries[i])
        sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
        xMat = cbind(rep(1,length(ind)),
                     rep(1,length(ind)), ### standardize to SouthAsia, where rep lowest (=intercept)
                     dat$wfh[sels],
                     dat$urban[sels]=='1',
                     dat$solidfuel[sels]=='1',
                     log(dat$gdp[sels]),
                     log(dat$age[sels]+1),
                     (dat$motheredu[sels]%in%c('0','1')),
                     dat$motheredu[sels]=='2',
                     dat$motheredu[sels]=='3',
                     rep(2016,length(ind)))
        lriBeta059 = mvrnorm(1,parsLri059[m,],vcovLri059[m,,])[c(1,6,7:15)]
        totLri059 = exp(xMat%*%lriBeta059)*26
        totLri059 = rpois(length(totLri059),totLri059)
        lriInc059[m,i] = mean(totLri059)
        
        #### lri incidence ages 0-24m
        ind = which(final.dat$country==countries[i]&dat$age<2)
        sels = sample(ind,length(ind),prob=final.dat$samplwt[ind],replace=T)
        xMat = cbind(rep(1,length(ind)),
                     rep(1,length(ind)), ### standardize to SouthAsia, where rep lowest (=intercept)
                     dat$wfh[sels],
                     dat$urban[sels]=='1',
                     dat$solidfuel[sels]=='1',
                     log(dat$gdp[sels]),
                     dat$age[sels],
                     (dat$motheredu[sels]%in%c('0','1')),
                     dat$motheredu[sels]=='2',
                     dat$motheredu[sels]=='3',
                     rep(2016,length(ind)))
        lriBeta024 = mvrnorm(1,parsLri024[m,],vcovLri024[m,,])[c(1,6,7:15)]
        totLri024 = exp(xMat%*%lriBeta024)*26
        totLri024 = rpois(length(totLri024),totLri024)
        lriInc024[m,i] = mean(totLri024)
        
      } 
    }
  }
  return(list(diarInc,lriInc,lriInc059,lriInc024))
}

set.seed(1)
library(parallel); library(MASS)
diarIncLB = lriIncLB = lriInc059LB = array(NA,dim=c(1e3,5,81))
lriInc059LBnew = array(NA,dim=c(1e3,5,81))
lriInc024LB = array(NA,dim=c(1e3,5,81))
num = 1:10
for (i in 1:100){
  
  out = mclapply(num, function(num) incmodel.fn(num),mc.cores=10)
  
  for (j in 1:10){
    diarIncLB[(i-1)*10+j,,] = out[[j]][[1]]
    lriIncLB[(i-1)*10+j,,] = out[[j]][[2]]
    lriInc059LB[(i-1)*10+j,,] = out[[j]][[3]]
    lriInc024LB[(i-1)*10+j,,] = out[[j]][[4]]
  }
  
  save(diarIncLB,file='diarIncLB.Rdata')
  save(lriIncLB,file='lriIncLB.Rdata')
  save(lriInc059LB,file='lriInc059LB.Rdata')
  save(lriInc024LB,file='lriInc024LB.Rdata')
  print(i)
}






setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('diarInc.Rdata'); load('diarAbx.Rdata'); load('lriInc.Rdata'); load('lriAbx.Rdata'); load('lriInc059.Rdata'); load('lriAbx059.Rdata')
load('diarIncLB.Rdata'); load('lriIncLB.Rdata'); load('lriInc059LB.Rdata')

load('final.dat.Rdata')

countries = unique(final.dat$country)
ic = c()
for (i in 1:length(countries)){
  ic[i] = as.character(final.dat$income[which(final.dat$country==countries[i])][1])
}

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/safe pcv')
load('efflri25.Rdata')
load('efftrtabxlri25.Rdata')
load('proptrtabxlri25.Rdata')
load('proplri25.Rdata')
load('efflri05.Rdata')
load('efftrtabxlri05.Rdata')
load('proptrtabxlri05.Rdata')
load('proplri05.Rdata')
load('veIPD.Rdata')
load('veAOM.Rdata')

load('proplriSens25.Rdata')
load('proptrtabxlriSens25.Rdata')
load('proplriSens05.Rdata')
load('proptrtabxlriSens05.Rdata')

setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/datasets/rota results')
load('effdiar02.Rdata')
load('efftrtdiar02.Rdata')
load('efftrtabxdiar02.Rdata')
load('proptrtabxdiar02ic.Rdata')
load('propdiar02ic.Rdata')
load('velic.Rdata'); load('vemic.Rdata')

lriIncMed = diarIncMed = lri059IncMed = array(NA,dim=dim(lriInc))
for (i in 1:5){
  for (j in 1:81){
    if (mean(is.na(lriInc[,i,j]))!=1){
      lbSort = sort(lriInc[,i,j])
      ubSort = sort(lriIncLB[,i,j])
      lriIncMed[,i,j] = exp(apply(log(cbind(lbSort,ubSort)),1,mean))
      
      lbSort = sort(diarInc[,i,j])
      ubSort = sort(diarIncLB[,i,j])
      diarIncMed[,i,j] = exp(apply(log(cbind(lbSort,ubSort)),1,mean))
      
      lbSort = sort(lriInc059[,i,j])
      ubSort = sort(lriInc059LB[,i,j])
      lri059IncMed[,i,j] = exp(apply(log(cbind(lbSort,ubSort)),1,mean))
    }
  }
}
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
save(diarIncMed,file='diarIncMed.Rdata')
save(lriIncMed,file='lriIncMed.Rdata')
save(lri059IncMed,file='lri059IncMed.Rdata')

lriAvert = lriAbxAvert = diarAvert = diarAbxAvert = lriAbxAvertElim = diarAbxAvertElim = lriAvertElim = diarAvertElim = array(NA,dim=c(1000,5,81))
lriAvertLB = lriAbxAvertLB = diarAvertLB = diarAbxAvertLB = lriAbxAvertElimLB = diarAbxAvertElimLB = lriAvertElimLB = diarAvertElimLB = array(NA,dim=c(1000,5,81))
lriAvertMed = lriAbxAvertMed = diarAvertMed = diarAbxAvertMed = lriAbxAvertElimMed = diarAbxAvertElimMed = lriAvertElimMed = diarAvertElimMed = array(NA,dim=c(1000,5,81))
lriAvertMedsens = lriAbxAvertMedsens = lriAbxAvertElimMedsens = lriAvertElimMedsens = array(NA,dim=c(1000,5,81))

lri059Avert = lriAbx059Avert = lri059AvertElim = lriAbx059AvertElim = array(NA,dim=c(1000,5,81))
lri059AvertLB = lriAbx059AvertLB = lri059AvertElimLB = lriAbx059AvertElimLB = array(NA,dim=c(1000,5,81))
lri059AvertMed = lriAbx059AvertMed = lri059AvertElimMed = lriAbx059AvertElimMed = array(NA,dim=c(1000,5,81))
lri059AvertMedsens = lriAbx059AvertMedsens = lri059AvertElimMedsens = lriAbx059AvertElimMedsens = array(NA,dim=c(1000,5,81))


set.seed(1)
effdiarLIC = matrix(sample(effdiar02[,3],5e3,replace=T),1e3,5); effdiarabxLIC = matrix(sample(efftrtabxdiar02[,3],5e3,replace=T),1e3,5);
effdiarMIC = matrix(sample(effdiar02[,2],5e3,replace=T),1e3,5); effdiarabxMIC = matrix(sample(efftrtabxdiar02[,2],5e3,replace=T),1e3,5)
efflri = matrix(sample(efflri25,5e3,replace=T),1e3,5); efflriabx = matrix(sample(efftrtabxlri25,5e3,replace=T),1e3,5)
efflri05 = matrix(sample(efflri05,5e3,replace=T),1e3,5); efflriabx05 = matrix(sample(efftrtabxlri05,5e3,replace=T),1e3,5)

propdiarabxLIC = matrix(sample(proptrtabxdiar02ic[,,3],5e3,replace=T),1e3,5)
propdiarabxMIC = matrix(sample(proptrtabxdiar02ic[,,2],5e3,replace=T),1e3,5)
propdiarLIC = matrix(sample(propdiar02ic[,,3],5e3,replace=T),1e3,5)
propdiarMIC = matrix(sample(propdiar02ic[,,2],5e3,replace=T),1e3,5)

proplriabx = matrix(sample(proptrtabxlri25,5e3,replace=T),1e3,5)
proplriabx05 = matrix(sample(proptrtabxlri05,5e3,replace=T),1e3,5)
proplri = matrix(sample(proplri25,5e3,replace=T),1e3,5)
proplri05 = matrix(sample(proplri05,5e3,replace=T),1e3,5)
proplriabxSens = matrix(sample(proptrtabxlriSens25,5e3,replace=T),1e3,5)
proplriabxSens05 = matrix(sample(proptrtabxlriSens05,5e3,replace=T),1e3,5)
proplriSens = matrix(sample(proplriSens25,5e3,replace=T),1e3,5)
proplriSens05 = matrix(sample(proplriSens05,5e3,replace=T),1e3,5)

velic = 1-matrix(sample(velic,5e3,replace=T),1e3,5); vemic = 1-matrix(sample(vemic,5e3,replace=T),1e3,5)
veIPD = 1-matrix(sample(veIPD,5e3,replace=T),1e3,5)
veAOM = 1-matrix(sample(veAOM,5e3,replace=T),1e3,5)

for (j in 1:81){
  if (ic[j]=='lic'){
    diarAvert[,,j] = diarInc[,,j]*(1-exp(effdiarLIC))
    diarAbxAvert[,,j] = diarAbx[,,j]*diarInc[,,j]*(1-exp(effdiarabxLIC))
    diarAbxAvertElim[,,j] = diarAbxAvert[,,j]*(1/velic)
    diarAvertElim[,,j] = diarAvert[,,j]*(1/velic)
    
    diarAvertLB[,,j] = diarIncLB[,,j]*(1-exp(effdiarLIC))
    diarAbxAvertLB[,,j] = diarAbx[,,j]*diarIncLB[,,j]*(1-exp(effdiarabxLIC))
    diarAbxAvertElimLB[,,j] = diarAbxAvertLB[,,j]*(1/velic)
    diarAvertElimLB[,,j] = diarAvertLB[,,j]*(1/velic)
    
    diarAvertMed[,,j] = diarIncMed[,,j]*(1-exp(effdiarLIC))
    diarAbxAvertMed[,,j] = diarAbx[,,j]*diarIncMed[,,j]*(1-exp(effdiarabxLIC))
    diarAbxAvertElimMed[,,j] = diarAbxAvertMed[,,j]*(1/velic)
    diarAvertElimMed[,,j] = diarAvertMed[,,j]*(1/velic)
  } else{
    diarAvert[,,j] = diarInc[,,j]*(1-exp(effdiarMIC))
    diarAbxAvert[,,j] = diarAbx[,,j]*diarInc[,,j]*(1-exp(effdiarabxMIC))
    diarAbxAvertElim[,,j] = diarAbxAvert[,,j]*(1/vemic)
    diarAvertElim[,,j] = diarAvert[,,j]*(1/vemic)
    
    diarAvertLB[,,j] = diarIncLB[,,j]*(1-exp(effdiarMIC))
    diarAbxAvertLB[,,j] = diarAbx[,,j]*diarIncLB[,,j]*(1-exp(effdiarabxMIC))
    diarAbxAvertElimLB[,,j] = diarAbxAvertLB[,,j]*(1/vemic)
    diarAvertElimLB[,,j] = diarAvertLB[,,j]*(1/vemic)
    
    diarAvertMed[,,j] = diarIncMed[,,j]*(1-exp(effdiarMIC))
    diarAbxAvertMed[,,j] = diarAbx[,,j]*diarIncMed[,,j]*(1-exp(effdiarabxMIC))
    diarAbxAvertElimMed[,,j] = diarAbxAvertMed[,,j]*(1/vemic)
    diarAvertElimMed[,,j] = diarAvertMed[,,j]*(1/vemic)
  }
  lriAvert[,,j] = lriInc[,,j]*(1-exp(efflri))
  lriAbxAvert[,,j] = lriAbx[,,j]*lriInc[,,j]*(1-exp(efflriabx)) 
  lriAbxAvertElim[,,j] = lriAbxAvert[,,j]*(1/veIPD)
  lriAvertElim[,,j] = lriAvert[,,j]*(1/veIPD)
  
  lriAvertLB[,,j] = lriIncLB[,,j]*(1-exp(efflri))
  lriAbxAvertLB[,,j] = lriAbx[,,j]*lriIncLB[,,j]*(1-exp(efflriabx)) 
  lriAbxAvertElimLB[,,j] = lriAbxAvertLB[,,j]*(1/veIPD)
  lriAvertElimLB[,,j] = lriAvertLB[,,j]*(1/veIPD)
  
  lriAvertMed[,,j] = lriIncMed[,,j]*(1-exp(efflri))
  lriAbxAvertMed[,,j] = lriAbx[,,j]*lriIncMed[,,j]*(1-exp(efflriabx)) 
  lriAbxAvertElimMed[,,j] = lriAbxAvertMed[,,j]*(1/veIPD)
  lriAvertElimMed[,,j] = lriAvertMed[,,j]*(1/veIPD)
  
  lriAvertMedsens[,,j] = lriIncMed[,,j]*(1-exp(efflri))
  lriAbxAvertMedsens[,,j] = lriAbx[,,j]*lriIncMed[,,j]*(1-exp(efflriabx)) 
  lriAbxAvertElimMedsens[,,j] = lriAbxAvertMedsens[,,j]*(1/veAOM)
  lriAvertElimMedsens[,,j] = lriAvertMedsens[,,j]*(1/veAOM)
  
  lri059Avert[,,j] = lriInc059[,,j]*(1-exp(efflri05))
  lriAbx059Avert[,,j] = lriAbx059[,,j]*lriInc059[,,j]*(1-exp(efflriabx05)) 
  lriAbx059AvertElim[,,j] = lriAbx059Avert[,,j]*(1/veIPD)
  lri059AvertElim[,,j] = lri059Avert[,,j]*(1/veIPD)
  
  lri059AvertLB[,,j] = lriInc059LB[,,j]*(1-exp(efflri05))
  lriAbx059AvertLB[,,j] = lriAbx059[,,j]*lriInc059LB[,,j]*(1-exp(efflriabx05)) 
  lriAbx059AvertElimLB[,,j] = lriAbx059AvertLB[,,j]*(1/veIPD)
  lri059AvertElimLB[,,j] = lri059AvertLB[,,j]*(1/veIPD)
  
  lri059AvertMed[,,j] = lri059IncMed[,,j]*(1-exp(efflri05))
  lriAbx059AvertMed[,,j] = lriAbx059[,,j]*lri059IncMed[,,j]*(1-exp(efflriabx05)) 
  lriAbx059AvertElimMed[,,j] = lriAbx059AvertMed[,,j]*(1/veIPD)
  lri059AvertElimMed[,,j] = lri059AvertMed[,,j]*(1/veIPD)
  
  lri059AvertMedsens[,,j] = lri059IncMed[,,j]*(1-exp(efflri05))
  lriAbx059AvertMedsens[,,j] = lriAbx059[,,j]*lri059IncMed[,,j]*(1-exp(efflriabx05)) 
  lriAbx059AvertElimMedsens[,,j] = lriAbx059AvertMedsens[,,j]*(1/veAOM)
  lri059AvertElimMedsens[,,j] = lri059AvertMedsens[,,j]*(1/veAOM)
  print(j)
}



diarAbxAvert[diarAvert<diarAbxAvert&is.na(diarAvert<diarAbxAvert)==F] = diarAvert[diarAvert<diarAbxAvert&is.na(diarAvert<diarAbxAvert)==F]
diarAbxAvertLB[diarAvertLB<diarAbxAvertLB&is.na(diarAvertLB<diarAbxAvertLB)==F] = diarAvertLB[diarAvertLB<diarAbxAvertLB&is.na(diarAvertLB<diarAbxAvertLB)==F]
diarAbxAvertMed[diarAvertMed<diarAbxAvertMed&is.na(diarAvertMed<diarAbxAvertMed)==F] = diarAvertMed[diarAvertMed<diarAbxAvertMed&is.na(diarAvertMed<diarAbxAvertMed)==F]

lriAbxAvert[lriAvert<lriAbxAvert&is.na(lriAvert<lriAbxAvert)==F] = lriAvert[lriAvert<lriAbxAvert&is.na(lriAvert<lriAbxAvert)==F]
lriAbxAvertLB[lriAvertLB<lriAbxAvertLB&is.na(lriAvertLB<lriAbxAvertLB)==F] = lriAvertLB[lriAvertLB<lriAbxAvertLB&is.na(lriAvertLB<lriAbxAvertLB)==F]
lriAbxAvertMed[lriAvertMed<lriAbxAvertMed&is.na(lriAvertMed<lriAbxAvertMed)==F] = lriAvertMed[lriAvertMed<lriAbxAvertMed&is.na(lriAvertMed<lriAbxAvertMed)==F]
lriAbxAvertMedsens[lriAvertMedsens<lriAbxAvertMedsens&is.na(lriAvertMedsens<lriAbxAvertMedsens)==F] = lriAvertMedsens[lriAvertMedsens<lriAbxAvertMedsens&is.na(lriAvertMedsens<lriAbxAvertMedsens)==F]

lriAbx059Avert[lri059Avert<lriAbx059Avert&is.na(lri059Avert<lriAbx059Avert)==F] = lri059Avert[lri059Avert<lriAbx059Avert&is.na(lri059Avert<lriAbx059Avert)==F]
lriAbx059AvertLB[lri059AvertLB<lriAbx059AvertLB&is.na(lri059AvertLB<lriAbx059AvertLB)==F] = lri059AvertLB[lri059AvertLB<lriAbx059AvertLB&is.na(lri059AvertLB<lriAbx059AvertLB)==F]
lriAbx059AvertMed[lri059AvertMed<lriAbx059AvertMed&is.na(lri059AvertMed<lriAbx059AvertMed)==F] = lri059AvertMed[lri059AvertMed<lriAbx059AvertMed&is.na(lri059AvertMed<lriAbx059AvertMed)==F]
lriAbx059AvertMedsens[lri059AvertMedsens<lriAbx059AvertMedsens&is.na(lri059AvertMedsens<lriAbx059AvertMedsens)==F] = lri059AvertMedsens[lri059AvertMedsens<lriAbx059AvertMedsens&is.na(lri059AvertMedsens<lriAbx059AvertMedsens)==F]



##### inc avertible by elimination of pathogen



setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
save(diarAvert,file='diarAvert.Rdata')
save(diarAbxAvert,file='diarAbxAvert.Rdata')
save(lriAvert,file='lriAvert.Rdata')
save(lri059Avert,file='lri059Avert.Rdata')
save(lriAbx059Avert,file='lriAbx059Avert.Rdata')
save(lriAbxAvertElim,file='lriAbxAvertElim.Rdata')
save(lriAbx059AvertElim,file='lriAbx059AvertElim.Rdata')
save(diarAbxAvertElim,file='diarAbxAvertElim.Rdata')
save(lriAvertElim,file='lriAvertElim.Rdata')
save(lri059AvertElim,file='lri059AvertElim.Rdata')
save(diarAvertElim,file='diarAvertElim.Rdata')

save(diarAvertLB,file='diarAvertLB.Rdata')
save(diarAbxAvertLB,file='diarAbxAvertLB.Rdata')
save(lriAvertLB,file='lriAvertLB.Rdata')
save(lri059AvertLB,file='lri059AvertLB.Rdata')
save(lriAbx059AvertLB,file='lriAbx059AvertLB.Rdata')
save(lriAbxAvertElimLB,file='lriAbxAvertElimLB.Rdata')
save(lriAbx059AvertElimLB,file='lriAbx059AvertElimLB.Rdata')
save(diarAbxAvertElimLB,file='diarAbxAvertElimLB.Rdata')
save(lriAvertElimLB,file='lriAvertElimLB.Rdata')
save(lri059AvertElimLB,file='lri059AvertElimLB.Rdata')
save(diarAvertElimLB,file='diarAvertElimLB.Rdata')

save(diarAvertMed,file='diarAvertMed.Rdata')
save(diarAbxAvertMed,file='diarAbxAvertMed.Rdata')
save(lriAvertMed,file='lriAvertMed.Rdata')
save(lri059AvertMed,file='lri059AvertMed.Rdata')
save(lriAbx059AvertMed,file='lriAbx059AvertMed.Rdata')
save(lriAbxAvertElimMed,file='lriAbxAvertElimMed.Rdata')
save(lriAbx059AvertElimMed,file='lriAbx059AvertElimMed.Rdata')
save(diarAbxAvertElimMed,file='diarAbxAvertElimMed.Rdata')
save(lriAvertElimMed,file='lriAvertElimMed.Rdata')
save(lri059AvertElimMed,file='lri059AvertElimMed.Rdata')
save(diarAvertElimMed,file='diarAvertElimMed.Rdata')

save(lriAvertMedsens,file='lriAvertMedsens.Rdata')
save(lriAbxAvertMedsens,file='lriAbxAvertMedsens.Rdata')
save(lriAbxAvertElimMedsens,file='lriAbxAvertElimMedsens.Rdata')
save(lriAvertElimMedsens,file='lriAvertElimMedsens.Rdata')

save(lri059AvertMedsens,file='lri059AvertMedsens.Rdata')
save(lriAbx059AvertMedsens,file='lriAbx059AvertMedsens.Rdata')
save(lriAbx059AvertElimMedsens,file='lriAbx059AvertElimMedsens.Rdata')
save(lri059AvertElimMedsens,file='lri059AvertElimMedsens.Rdata')


 load('diarAvert.Rdata')
 load('diarAbxAvert.Rdata')
 load('lriAvert.Rdata')
 load('lriAbxAvert.Rdata')
 load('lriAbxAvertElim.Rdata')
 load('diarAbxAvertElim.Rdata')
 load('lriAvertElim.Rdata')
 load('diarAvertElim.Rdata')

########## table output
whogroup = rep(NA,length(countries))
whogroup[countries%in%c('Angola','Benin','Burkina Faso','Burundi',
                        'Cameroon','Chad','Comoros','Congo','Congo Democratic Republic',"Cote d'Ivoire",
                        'Eswatini','Ethiopia','Gabon','Gambia','Ghana','Guinea','Kenya','Lesotho',
                        'Liberia','Malawi','Mali','Mozambique','Namibia','Niger','Nigeria','Rwanda','Senegal',
                        'Sierra Leone','South Africa','Tanzania','Togo','Uganda','Zambia','Zimbabwe','Guinea Bissau',
                        'Mauritania','Sao Tome and Principe')] = 'AFR'
whogroup[countries%in%c('Bolivia','Colombia','Dominican Republic','Guatemala','Guyana','Haiti','Honduras','Peru',
                        'Belize','Cuba','El Salvador','Mexico','Paraguay')] = 'AMR' # 'Panama' out because income too high
whogroup[countries%in%c('Albania','Armenia','Azerbaijan','Tajikistan','Kosovo','Turkmenistan','Kyrgyz Republic',
                        'Montenegro')] = 'EUR'
whogroup[countries%in%c('Afghanistan','Egypt','Jordan','Pakistan','Yemen','Palestine')] = 'EMR'
whogroup[countries%in%c('Bangladesh','India','Maldives','Myanmar','Nepal','Timor-Leste','Indonesia','Thailand')] = 'SEAR'
whogroup[countries%in%c('Cambodia','Philippines','Lao PDR','Mongolia','Vietnam')] = 'WPR'

### lri

print.fn = function(x,place){
  paste(round(mean(x*1e2),place),' (',round(quantile(x*1e2,0.025),place),', ',round(quantile(x*1e2,0.975),place),')',sep='')
}

groups = c('AFR','AMR','EMR','EUR','SEAR','WPR')

pos = c()
vec = diar = lri = list()
for (i in 1:length(groups)){
  vec[[i]] = sort(countries[whogroup==groups[i]])
  diar[[i]] = lri[[i]] = matrix(NA,length(vec[[i]]),6)
  for (j in which(whogroup==groups[i])){
    sel = which(vec[[i]]==countries[j])
    diar[[i]][sel,] = c(as.character(countries[j]),
                        print.fn(diarInc[,,j],0),
                        print.fn(diarAbx[,,j]*diarInc[,,j],0),
                        print.fn(diarAbx[,,j],1),
                        print.fn(diarAvert[,,j],0),
                        # print.fn(diarAvertElim[,,j],0),
                        print.fn(diarAbxAvert[,,j],0))
    # print.fn(diarAbxAvertElim[,,j],0))
    lri[[i]][sel,] = c(as.character(countries[j]),
                       print.fn(lriInc[,,j],0),
                       print.fn(lriAbx[,,j]*lriInc[,,j],0),
                       print.fn(lriAbx[,,j],1),
                       print.fn(lriAvert[,,j],0),
                       # print.fn(lriAvertElim[,,j],0),
                       print.fn(lriAbxAvert[,,j],0))
    # print.fn(lriAbxAvertElim[,,j],0))
  }
}

diarout = lriout = c()
for (i in 1:6){
  diarout = rbind(diarout,c(groups[i],rep(NA,5)),diar[[i]])
  lriout = rbind(lriout,c(groups[i],rep(NA,5)),lri[[i]])
}

write.csv(diarout,file='diarout.csv')
write.csv(lriout,file='lriout.csv')



################################################
################################################
### Generating regression output for tables ####
################################################
################################################

### lri
setwd('~/Google drive (jlewnard@berkeley.edu)/DHS/diar inc')
load('lriVarNames.Rdata')
load('parsLri.Rdata')
load('vcovLri.Rdata')
load('trtlriVarNames.Rdata')
load('parsTrtLri.Rdata')
load('vcovTrtLri.Rdata')

load('lri059VarNames.Rdata')
load('parsLri059.Rdata')
load('vcovLri059.Rdata')
load('trtlri059VarNames.Rdata')
load('parsTrtLri059.Rdata')
load('vcovTrtLri059.Rdata')

load('diarVarNames.Rdata')
load('parsDiar.Rdata')
load('vcovDiar.Rdata')
load('trtdiarVarNames.Rdata')
load('parsTrtDiar.Rdata')
load('vcovTrtDiar.Rdata')


library(MASS)
set.seed(1)
diarPars = array(NA,dim=c(5,1e4,length(diarVarNames)))
trtdiarPars = array(NA,dim=c(5,1e4,length(trtdiarVarNames)))
lriPars = array(NA,dim=c(5,1e4,length(lriVarNames)))
trtlriPars = array(NA,dim=c(5,1e4,length(trtlriVarNames)))
lri059Pars = array(NA,dim=c(5,1e4,length(lri059VarNames)))
trtlri059Pars = array(NA,dim=c(5,1e4,length(trtlri059VarNames)))
for (m in 1:5){
  diarPars[m,,] = mvrnorm(1e4,parsDiar[m,],vcovDiar[m,,])
  trtdiarPars[m,,] = mvrnorm(1e4,parsTrtDiar[m,],vcovTrtDiar[m,,])
  lriPars[m,,] = mvrnorm(1e4,parsLri[m,],vcovLri[m,,])
  trtlriPars[m,,] = mvrnorm(1e4,parsTrtLri[m,],vcovTrtLri[m,,])
  lri059Pars[m,,] = mvrnorm(1e4,parsLri059[m,],vcovLri059[m,,])
  trtlri059Pars[m,,] = mvrnorm(1e4,parsTrtLri059[m,],vcovTrtLri059[m,,])
}

for (i in 1:length(trtdiarVarNames)){
  print(trtdiarVarNames[i])
  print(quantile(exp(trtdiarPars[,,i]),c(0.5,0.025,0.975)))
}
