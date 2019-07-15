# Make a RData for github analysis
rm(list=ls())  # remove all lists in environment
graphics.off()  # shut down all open graphics devices

## prepare ppt dataset knowns
#dataset <- read.csv("DBallCore2016_nospec.csv", header=T,na.strings = "", stringsAsFactors = FALSE)

#setwd("~/Dropbox/projects/Historical data/full_2018/Severe_Malaria_database_")
library(readstata13)
library(haven)
require(plyr)
require(dplyr)
dataset = read_dta('../../../../Datasets/Malaria Core/DBallCore2016_V5.dta')

#Studies
table(dataset$studyID,useNA = 'ifany')
dataset$studyID=mapvalues(dataset$studyID, from = 'AQ (the Gambia)','QC')
dataset = filter(dataset, studyID != 'AQGambia')
dataset$studyID = as.factor(as.character(dataset$studyID))

## add glucose, transfusion

dataset$hypoglycaemia = dataset$hyglycemiaCri

# transfusion
dataset$transfusion= dataset$bloodtransfusion
dataset$transfusion[is.na(dataset$transfusion)]= dataset$bloodtran[is.na(dataset$transfusion)]

# year 
dataset$year = as.factor(dataset$year)
table(dataset$year,useNA = 'ifany')

#Country or site
table(dataset$country)
dataset$country[dataset$country=="Gambia"] = "The Gambia"
dataset$country = as.factor(dataset$country)


# Treatment
table(dataset$StudyDrug1, useNA = 'ifany')
dataset$drug = dataset$StudyDrug1


dataset$drug[dataset$drug == "ARTEMETHER" ] = "Artemether"
dataset$drug[dataset$drug == "ARTESUNATE" ] = "Artesunate"
dataset$drug[dataset$drug == "AMODIAQUINE" ] = "Amodiaquine"
dataset$drug[dataset$drug == "NAC(N-ACETYLCYSTEINE)" ] = "NAC"
dataset$drug[dataset$drug == "LUMEFANTRINE" ] = "Lumefantrine"
dataset$drug[dataset$drug == "CHLOROQUINE" ] = "Chloroquine"
dataset$drug[dataset$drug == "MEFLOQUINE" ] = "Mefloquine"
dataset$drug[dataset$drug == "QUININE" ] = "Quinine"

dataset$drug = as.factor(dataset$drug)
table(dataset$drug, useNA = 'ifany')

dataset$drug_class = NA
dataset$drug_class[dataset$drug == "Artesunate" ] = "artemisinin"
dataset$drug_class[dataset$drug == "Artemether" ] = "artemisinin"
dataset$drug_class[dataset$drug == "Amodiaquine" ] = "artemisinin"
dataset$drug_class[dataset$drug == "Chloroquine" & dataset$studyID != 'QC'] = "artemisinin"
dataset$drug_class[dataset$drug == "Chloroquine" & dataset$studyID == 'QC'] = "non-artemisinin"
dataset$drug_class[dataset$drug == "Lumefantrine" ] = "non-artemisinin"
dataset$drug_class[dataset$drug == "Mefloquine" ] = "non-artemisinin"
dataset$drug_class[dataset$drug == "NAC" ] = "non-artemisinin"
dataset$drug_class[dataset$drug == "Quinine" ] = "non-artemisinin"
dataset$drug_class = as.factor(dataset$drug_class)
table(dataset$drug_class, useNA = 'ifany')

# Dichotomous outcomes 
dataset$outcome = dataset$died
dataset$outcome[dataset$died=="Yes"] = 1
dataset$outcome[dataset$died=="No"] = 0
table(dataset$outcome, useNA = 'ifany')/nrow(dataset)

## Hct 
dataset$HCT = dataset$lbhct
dataset$HCT[is.na(dataset$lbhct)] = dataset$hctadm[is.na(dataset$lbhct)]
dataset$HCT[is.na(dataset$lbhct) & is.na(dataset$hctadm) ] = dataset$lbihct[is.na(dataset$lbhct) & is.na(dataset$hctadm) ] 
dataset$HCT = as.numeric(as.character(dataset$HCT))

# parasitaemia/ul
dataset$paraul = as.numeric(as.character(dataset$paraul))
dataset$paraul[dataset$paraul<10] = NA

dataset$paraul_s = as.numeric(as.character(dataset$paraul))
dataset$paraul_s[dataset$paraul_s<1000] = NA
dataset$LPAR = log10(dataset$paraul_s)
summary(dataset$LPAR)

# parasitaemia %
## impute MCVs
dataset$lbhmcv = as.numeric(as.character(dataset$lbhmcv))
quantile(dataset$lbhmcv, na.rm=T) ## 82
dataset$MCV = dataset$lbhmcv
dataset$MCV[is.na(dataset$lbhmcv)] = 82

## calculate no. of RBC per microliter based on imputed MCVs
dataset$rbc_ul = ((dataset$HCT/100) * 10^9) / dataset$MCV
hist(dataset$rbc_ul)

## calculate no. of parasites per microliter and divide by red cell count to get % for thick films
dataset$parc_wbc = NA
dataset$parc_wbc = 100* ((dataset$pfalciparum500WBCs * 40) / dataset$rbc_ul)
dataset$parc_wbc[is.na(dataset$pfalciparum500WBCs)] = 100*((dataset$pfalciparum500WBCs[is.na(dataset$pfalciparum500WBCs)]
                                                       * 40) / dataset$rbc_ul[is.na(dataset$pfalciparum500WBCs)])
hist(dataset$parc_wbc)
quantile(dataset$parc_wbc, na.rm=T)

## create parasitaemia percentage variable
summary(dataset$paraperc)
hist(dataset$paraperc)
dataset$paraperc[dataset$paraperc>60] = NA
summary(dataset$paraperc)
summary(dataset$pfalciparum1000RBCs)
hist(dataset$pfalciparum1000RBCs)

dataset$parc = NA
dataset$parc = dataset$paraperc
dataset$parc[is.na(dataset$parc)] = 100* (dataset$pfalciparum1000RBCs[is.na(dataset$parc)] / 1000)
summary(dataset$parc)
dataset$parc[!is.na(dataset$pfalciparum500WBCs) | !is.na(dataset$pfalciparum200WBCs)] = dataset$parc_wbc[!is.na(dataset$pfalciparum500WBCs) | !is.na(dataset$pfalciparum200WBCs)]

hist(dataset$parc)
summary(dataset$parc)

#dataset$parc[is.na(dataset$parc)] = 
dataset$paraul_to_paraperc = NA
dataset$paraul_to_paraperc = ( (dataset$paraul) / (dataset$HCT*125.6) ) / 10

dataset$paraul_to_paraperc[dataset$paraul_to_paraperc<1] = NA  ## or impute by using 0.0016
summary(dataset$paraul_to_paraperc >1)

dataset$paraul_to_paraperc[dataset$paraul_to_paraperc<1] = NA  ## or impute by using 0.0016
sum(is.na(dataset$paraul_to_paraperc))

dataset$parc[is.na(dataset$parc)] = dataset$paraul_to_paraperc[is.na(dataset$parc)]

dataset$LPAR_pct = log10(dataset$parc)
hist(dataset$LPAR_pct)
summary(dataset$LPAR_pct)

#Base excess 
dataset$BD = -(dataset$lbibe)
dataset$BD  = as.numeric(as.character(dataset$BD))

#Bicarbonate
dataset$bicarbonate = NA
dataset$bicarbonate = dataset$lbbbic
dataset$bicarbonate[is.na(dataset$bicarbonate)] = dataset$lbihco3[is.na(dataset$bicarbonate)]
dataset$bicarbonate = as.numeric(as.character(dataset$bicarbonate))

#Lactate
dataset$lactate = NA
dataset$lactate = dataset$lbilact
dataset$lactate[is.na(dataset$lactate)] = dataset$lbblact[is.na(dataset$lactate)]
dataset$lactate[is.na(dataset$lactate)] = dataset$lbilactST[is.na(dataset$lactate)]
dataset$lactate = as.numeric(as.character(dataset$lactate))

## BUN
dataset$BUN = NA
dataset$BUN = dataset$lbibun
dataset$BUN[is.na(dataset$lbibun)] = dataset$lbbbun[is.na(dataset$lbibun)]
dataset$BUN = as.numeric(as.character(dataset$BUN))

## creatinine
dataset$creatinine = NA
dataset$creatinine = dataset$lbbcrea
dataset$creatinine[is.na(dataset$creatinine)] = dataset$lbicrea[is.na(dataset$creatinine)]
dataset$creatinine[is.na(dataset$creatinine)] = dataset$crea[is.na(dataset$creatinine)]
dataset$creatinine = as.numeric(as.character(dataset$creatinine))
dataset$creatinine[dataset$creatinine>25 & !is.na(dataset$creatinine)] = dataset$creatinine[dataset$creatinine>25 & !is.na(dataset$creatinine)] / 88.42
dataset$creatinine = dataset$creatinine*88.42
hist(log10(dataset$creatinine))

## pulmonary oedema
dataset$poedema = NA
dataset$poedema = as.character(dataset$puloedemaCri)
dataset$poedema[is.na(dataset$poedema)] = dataset$Aedema[is.na(dataset$poedema)]
dataset$poedema[dataset$poedema =='No'] = 0
dataset$poedema[dataset$poedema =='Yes'] = 1
dataset$poedema[is.na(dataset$poedema)] = 0
dataset$poedema = as.factor(dataset$poedema) 
summary(dataset$poedema)
table(dataset$poedema,dataset$studyID,useNA = 'ifany')

#shock
dataset$shock =NA
dataset$shock = dataset$systolicbpCri
dataset$shock = as.character(dataset$shock)
str(dataset$shock)
dataset$shock[dataset$shock =='Yes'] <- 1
dataset$shock[dataset$shock =='No'] =0
dataset$shock[is.na(dataset$shock)] = 0
dataset$shock = as.factor(dataset$shock) 
table(dataset$shock,useNA = 'ifany')


#shock
dataset$coma =NA
dataset$coma[dataset$gcstotal<11] =1 
dataset$coma[dataset$gcstotal<11] =1 

dataset$gcse = as.numeric(as.character(dataset$gcse))
dataset$gcsv = as.numeric(as.character(dataset$gcsv))
dataset$gcsm = as.numeric(as.character(dataset$gcsm))

dataset$GCS = NA
dataset$GCS = dataset$gcstotal
dataset$GCS <- dataset$gcse + dataset$gcsv + dataset$gcsm
hist(dataset$GCS)

dataset$coma[dataset$GCS <11] =1 
dataset$coma[dataset$GCS >= 11] =0  

dataset$coma[dataset$bcstotal <3] =1 
dataset$coma[dataset$bcstotal >= 3] =0 

dataset$coma[dataset$cerebralCri=="Yes"] = 1 
dataset$coma[dataset$cerebralCri=="No"] = 0 

table(dataset$coma,useNA = 'ifany')

##shock
dataset$shock = dataset$systolicbpCri
dataset$shock = as.character(dataset$shock)
str(dataset$shock)
dataset$shock[dataset$shock =='Yes'] <- 1
dataset$shock[dataset$shock =='No'] =0
dataset$shock[is.na(dataset$shock)] = 0
dataset$shock = as.factor(dataset$shock) 
table(dataset$shock,useNA = 'ifany')

##shock
dataset$convulsions = dataset$convulCri
table(dataset$convulsions)
dataset$convulsions = as.character(dataset$convulsions)
dataset$convulsions[dataset$convulsions =='Yes'] <- 1
dataset$convulsions[dataset$convulsions =='No'] =0
dataset$convulsions[is.na(dataset$convulsions)] = 0
dataset$convulsions = as.factor(dataset$convulsions) 
table(dataset$convulsions,useNA = 'ifany')

#Age
dataset$AgeInYear = as.numeric(dataset$AgeInYear)

## Shock
dataset$systolicbpCri = as.factor(dataset$systolicbpCri) 
summary(dataset$systolicbpCri)
dataset$SYS_BP_NUMERIC = dataset$systolicbpCri
dataset$SYS_BP_NUMERIC[as.character(dataset$systolicbpCri)=='Yes']=1
dataset$SYS_BP_NUMERIC[as.character(dataset$systolicbpCri)=='No']=0
dataset$AgeInYear = as.numeric(dataset$AgeInYear)

m = subset(dataset, 
           select=c(StudyNumber,year, country, studyID, 
                    drug_class, drug, shock,convulsions, poedema,  
                    outcome,  AgeInYear, coma, HCT, paraul, parc, 
                    LPAR, LPAR_pct, BD, bicarbonate, rr, lactate, BUN, 
                    creatinine, hypoglycaemia, transfusion,Timetodeathhrs))

m$drug_class = as.factor(m$drug_class)
rm_ind = is.na(m$outcome) | is.na(m$studyID) | is.na(m$AgeInYear)
m = m[!rm_ind, ]
m$BUN[m$BUN > 140] = 140
str(m)

m$coma[is.na(m$coma) & m$studyID=='AQ'] = 0

m$country[m$studyID=='QC'] = 'The Gambia'

table(m$studyID)


Africa = c('The Gambia','Mozambique','Ghana','Kenya','Nigeria','Tanzania','Uganda','Rwanda','Congo')
Asia = c('Thailand','Vietnam','Bangladesh','Myanmar','India','Indonesia')

m$continent = NA
m$continent[m$country%in%Asia] = 'Asia'
m$continent[m$country%in%Africa] = 'Africa'
m$continent = as.factor(m$continent)


m = m[,c("StudyNumber","year","country","studyID","drug_class",
      "shock","convulsions","poedema","outcome","AgeInYear",
      "coma","HCT","LPAR_pct","BD","bicarbonate","rr","lactate",
      "BUN","creatinine","hypoglycaemia","transfusion","Timetodeathhrs","continent")] 

m$drug_AS = 0
m$drug_AS[m$drug_class=='artemisinin']=1
m$LPAR_pct[is.infinite(m$LPAR_pct)] = 0

upplim = quantile(m$Timetodeathhrs,probs = .95,na.rm = T)
hist(m$Timetodeathhrs[!is.na(m$Timetodeathhrs)&m$Timetodeathhrs<=upplim],
     breaks = seq(0,upplim,length.out = 20), main = 'Time to death',xlab = 'hours')
hist(m$Timetodeathhrs[!is.na(m$Timetodeathhrs)&m$Timetodeathhrs<=upplim&m$AgeInYear<15],
     breaks = seq(0,upplim,length.out = 20), add=T, col =adjustcolor('red',alpha.f = .3))
hist(m$Timetodeathhrs[!is.na(m$Timetodeathhrs)&m$Timetodeathhrs<=upplim&m$AgeInYear>=15],
     breaks = seq(0,upplim,length.out = 20), add=T, col =adjustcolor('blue',alpha.f = .3))

m$Unique_ID = apply(m, 1, function(x) paste(x['StudyNumber'], x['studyID'],sep='_'))
seaqmat = read_sav("~/Dropbox/Datasets/SEAQUAMAT/seaquamat stage 3.sav")
seaqmat$Unique_ID = apply(seaqmat, 1, function(x) paste(x['studyno'], 'SEAQUAMAT',sep='_'))

m = merge(m, seaqmat[,c('Unique_ID','transfusion')], by='Unique_ID', all = T)
m$transfusion = m$transfusion.x
# transfusion was not reliable recorded in SEAQUAMAT
m$transfusion[m$studyID=='SEAQUAMAT'] = NA #m$transfusion.y[m$studyID=='SEAQUAMAT']

table(m$studyID,m$transfusion,useNA = 'ifany')
m = m[,!colnames(m) %in% c('transfusion.x','transfusion.y')]
save(m, file = '~/Dropbox/MORU/Causality/SevereMalariaAnalysis/RData/Data.RData')

