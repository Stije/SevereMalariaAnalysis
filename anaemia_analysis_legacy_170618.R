
# Historical data MORU 1986-2016
## version
rm(list=ls())  # remove all lists in environment
load('RData/Data.R')
## Model 1
library(mgcv)
complete_cases = apply(m, 1, function(x) sum(is.na(x))) == 0
mod_pars_0<-gam(outcome ~ s(HCT, AgeInYear) + s(BUN, BD) + LPAR + SYS_BP_NUMERIC + s(studyID, bs='re'),
                data=m[complete_cases,], family=binomial)

plot(NA,NA, xlim=c(4,45), ylim=c(0,40),ylab='mortality', xlab='Haematocrit')
for(HCT in 4:45){
  mydata = m[complete_cases,]
  mydata$HCT=HCT
  ys = 100*predict(mod_pars_0, newdata = mydata, exclude="s(studyID)", type='response')
  
  points(HCT,mean(ys), pch=18)
  points(rep(HCT,2), quantile(ys, probs=c(0.1,0.9)), pch='-', col='red')
}

All_patients = expand.grid(HCT=4:45, BUN=seq(0,80,length.out = 10), BD=seq(-4,25, length.out = 15), 
                           SYS_BP_NUMERIC=0:1, LPAR=3:7, AgeInYear=c(1,2,3,4,5,10,20,50))
All_patients$studyID = m$studyID[1]
ys = 100*predict(mod_pars_0, newdata = All_patients, exclude="s(studyID)", type='response')
hist(ys, xlab='Probability of death', freq=F, yaxt='n', ylab='', main='')
abline(v=10, lwd=3, col='red')
