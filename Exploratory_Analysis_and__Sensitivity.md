---
title: "Charactersing effect of anaemia on mortality in severe malaria"
output:
  html_document:
    keep_md: yes
    fig_caption: yes
---

# Preliminaries








# Exploratory analysis


This looks at the severe malaria legacy dataset from MORU.





First we look at the mortality rates across the different studies.


```
## [1] "Core Malaria , mortality of: 22 %"
## [1] "AAV , mortality of: 10 %"
## [1] "SEAQUAMAT , mortality of: 19 %"
## [1] "AQUAMAT , mortality of: 10 %"
## [1] "AQ , mortality of: 15 %"
```

We look at the quantiles of the ages in the different studies:

```
## [1] "Core Malaria, ages:1 Core Malaria, ages:26 Core Malaria, ages:80"
## [1] "AAV, ages:15 AAV, ages:32 AAV, ages:77"
## [1] "SEAQUAMAT, ages:2 SEAQUAMAT, ages:25 SEAQUAMAT, ages:87"
## [1] "AQUAMAT, ages:0 AQUAMAT, ages:2 AQUAMAT, ages:78"
## [1] "AQ, ages:15 AQ, ages:30 AQ, ages:79"
```

```
## [1] "Core Malaria"
## 
##  Amodiaquine   Artemether   Artesunate  Chloroquine Lumefantrine 
##           10           11          461            4            1 
##   Mefloquine          NAC      Quinine 
##           10            6          613 
## [1] "AAV"
## 
##  Amodiaquine   Artemether   Artesunate  Chloroquine Lumefantrine 
##            0          184          186            0            0 
##   Mefloquine          NAC      Quinine 
##            0            0            0 
## [1] "SEAQUAMAT"
## 
##  Amodiaquine   Artemether   Artesunate  Chloroquine Lumefantrine 
##            0            0          730            0            0 
##   Mefloquine          NAC      Quinine 
##            0            0          731 
## [1] "AQUAMAT"
## 
##  Amodiaquine   Artemether   Artesunate  Chloroquine Lumefantrine 
##            0            0         2746            0            0 
##   Mefloquine          NAC      Quinine 
##            0            0         2748 
## [1] "AQ"
## 
##  Amodiaquine   Artemether   Artesunate  Chloroquine Lumefantrine 
##            0          284            0            0            0 
##   Mefloquine          NAC      Quinine 
##            0            0          276
```


Let's look at the linear associations between the key baseline variables. We use mixed effects linear models to estimate these associations (random effect terms for both country and study).




```r
mycols = brewer.pal(length(unique(m$studyID)), 'Set1')
Leg_data_complete$color = as.character(revalue(Leg_data_complete$studyID, 
                                               replace = c('Core Malaria'=mycols[1],
                                                           'AAV'=mycols[2],
                                                           'AQ'=mycols[3],
                                                           'SEAQUAMAT' = mycols[4],
                                                           'AQUAMAT' = mycols[5])))
```

```
## The following `from` values were not present in `x`: AAV
```

```r
par(las=1, mfrow=c(2,2), mar=c(4,4,1,1), bty='n')
## Base Excess and HCT
plot(jitter(Leg_data_complete$HCT,amount=1), 
     jitter(Leg_data_complete$BD), 
     col=Leg_data_complete$color, pch='*', xlab='Haematocrit (%)', 
     ylab='Base deficit (mEq/L)')

ys = predict(object = mod_HCT_BD, 
             newdata = data.frame(HCT=8:50,
                                  studyID='AQ',
                                  country='Vietnam'), 
             exclude = c("s(country)","s(studyID)"))
lines(8:50, ys, lwd=3, col='black')

## Parasitaemia and Anaemia
plot(jitter(Leg_data_complete$LPAR_pct,amount=1), 
     Leg_data_complete$BD, 
     col=Leg_data_complete$color, pch='*', 
     xlab='Log10 % parasitised RBCs', 
     ylab='Base deficit (mEq/L)')
ys = predict(object = mod_BD_LPAR, 
             newdata = data.frame(LPAR_pct=seq(-3,3,by=.1),
                                  studyID='AQ',
                                  country='Vietnam'), 
             exclude = c("s(country)","s(studyID)"))
lines(seq(-3,3,by=.1), ys, lwd=3, col='black')

## BUN and BD
plot(jitter(log10(Leg_data_complete$BUN),amount=.1), 
     jitter(Leg_data_complete$BD), 
     col=Leg_data_complete$color, pch='*', 
     xlab='Blood urea nitrogen (mmol/L)', 
     ylab='Base deficit (mEq/L)', xaxt='n')
axis(1, at=c(log10(2), 1, 2), labels = c(2,10,100))

summary(mod_BUN_BD)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## BD ~ s(log10(BUN)) + s(studyID, bs = "re") + s(country, bs = "re")
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    4.607      1.931   2.386   0.0171 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                  edf Ref.df       F  p-value    
## s(log10(BUN))  4.172  5.164  247.75  < 2e-16 ***
## s(studyID)     2.783  3.000 2505.47 9.06e-14 ***
## s(country)    11.351 14.000   32.52  0.00877 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.283   Deviance explained = 28.6%
## GCV = 38.543  Scale est. = 38.4      n = 5233
```

```r
ys = predict(object = mod_BUN_BD, 
             newdata = data.frame(BUN=2:166,
                                  studyID='AQ',
                                  country='Vietnam'), 
             exclude = c("s(country)","s(studyID)"))
lines(log10(2:166), ys, lwd=3, col='black')

## Parasitaemia and Anaemia
plot(jitter(Leg_data_complete$AgeInYear,amount=1), 
     Leg_data_complete$HCT, 
     col=Leg_data_complete$color, pch='*', 
     xlab='Age (years)', 
     ylab='Haematocrit (%)')

summary(mod_Age_HCT)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## HCT ~ s(AgeInYear) + s(studyID, bs = "re") + s(country, bs = "re")
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  23.7273     0.9093   26.09   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                 edf Ref.df     F p-value    
## s(AgeInYear)  7.522  8.332 43.22 < 2e-16 ***
## s(studyID)    1.403  3.000 14.73   0.042 *  
## s(country)   12.506 14.000 10.98 1.4e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.261   Deviance explained = 26.4%
## GCV = 69.149  Scale est. = 68.853    n = 5233
```

```r
ys = predict(object = mod_Age_HCT, 
             newdata = data.frame(AgeInYear=0:80,
                                  studyID='AQ',
                                  country='Vietnam'), 
             exclude = c("s(country)","s(studyID)"))
lines(0:80, ys, lwd=3, col='black')
legend('topright',col=mycols,legend = c('Core Malaria',
                                        'AAV',
                                        'AQ',
                                        'SEAQUAMAT',
                                        'AQUAMAT'),pch='*')
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ExploratoryPlots-1.png)<!-- -->

Effect on survival 


```r
modHCT=gam(outcome ~ s(HCT) + s(studyID, bs='re') + s(country, bs='re'),
           data = Leg_data_complete, family='binomial')

modcoma=gam(outcome ~ coma + s(studyID, bs='re') + s(country, bs='re'),
            data = Leg_data_complete, family='binomial')

modBD=gam(outcome ~ s(BD) + s(studyID, bs='re') + s(country, bs='re'),
          data = Leg_data_complete, family='binomial')

modpoedema=gam(outcome ~ poedema + s(studyID, bs='re') + s(country, bs='re'),
               data = Leg_data_complete, family='binomial')

modconv=gam(outcome ~ convulsions + s(studyID, bs='re') + s(country, bs='re'),
            data = Leg_data_complete, family='binomial')

modBUN=gam(outcome ~ s(log10(BUN)) + s(studyID, bs='re') + s(country, bs='re'),
           data = Leg_data_complete, family='binomial')
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/UnadjustedPlots-1.png)<!-- -->

# Sensitivity Analysis

## Imputation of missing variables

Quite a lot of the important covariates are missing in the older studies. We use linear regression to estimate these unknown variables. This section shows the results for single imputation - when fitting the final models we use multiple imputation.

* Mising base deficit is imputed using bicarbonate (if available) else using respiratory rate
* Missing Blood urea nitrogen is imputed using creatinine

Impute base deficit from bicarbonate

```r
BD_and_bicarbonate = !is.na(m$BD) & !is.na(m$bicarbonate)
print(paste('We have ', sum(BD_and_bicarbonate), 'observations for both bicarbonate and base deficit'))
```

```
## [1] "We have  5066 observations for both bicarbonate and base deficit"
```

```r
mod_impute1 = lmer(BD ~ bicarbonate + (1 | studyID) + (1 | country), data= m[BD_and_bicarbonate,])
missing_BD = is.na(m$BD)
Available_Bicarbonate = !is.na(m$bicarbonate)
print(paste(sum(missing_BD & Available_Bicarbonate), 'observations will now be imputed'))
```

```
## [1] "307 observations will now be imputed"
```

```r
# impute with model
m$BD[missing_BD & Available_Bicarbonate] = predict(mod_impute1,newdata=m[missing_BD & Available_Bicarbonate,], re.form=NA)
```

Impute base deficit from lactate

```r
BD_and_lactate = !is.na(m$BD) & !is.na(m$lactate)
print(paste('We have ', sum(BD_and_lactate), 'observations for both lactate and base deficit'))
```

```
## [1] "We have  630 observations for both lactate and base deficit"
```

```r
if(length(unique(m$studyID[BD_and_lactate]))==1){
  mod_impute2 = lm(BD ~ lactate, data= m[BD_and_lactate,])
} else {
  mod_impute2 = lmer(BD ~ lactate + (1 | studyID), data= m[BD_and_lactate,])
}
missing_BD = is.na(m$BD)
Available_Lactate = !is.na(m$lactate)
print(paste(sum(missing_BD & Available_Lactate), 'observations will now be imputed'))
```

```
## [1] "431 observations will now be imputed"
```

```r
# impute with model
m$BD[missing_BD & Available_Lactate] = predict(mod_impute2,newdata=m[missing_BD & Available_Lactate,], re.form=NA)
```

Impute base deficit from respiratory rate

```r
BD_and_rr = !is.na(m$BD) & !is.na(m$rr)
print(paste('We have ', sum(BD_and_rr), 'observations for both resp rate and base deficit'))
```

```
## [1] "We have  7282 observations for both resp rate and base deficit"
```

```r
mod_impute3 = lmer(BD ~ rr + (1 | studyID), data= m[BD_and_rr,])
missing_BD = is.na(m$BD)
Available_rr = !is.na(m$rr)
print(paste(sum(missing_BD & Available_rr), 'observations will now be imputed'))
```

```
## [1] "1361 observations will now be imputed"
```

```r
m$BD[missing_BD & Available_rr] = predict(mod_impute3,newdata=m[missing_BD & Available_rr,], re.form=NA)
```


Impute blood urea nitrogen from creatinine:

```r
BUN_and_cr = !is.na(m$BUN) & !is.na(m$creatinine)
print(paste('We have ', sum(BUN_and_cr), 'observations for both blood urea nitrogen and creatinine'))
```

```
## [1] "We have  1448 observations for both blood urea nitrogen and creatinine"
```

```r
mod_impute4 = lmer(BUN ~ creatinine + (1 | studyID), data= m[BUN_and_cr,])
missing_BUN = is.na(m$BUN)
Available_cr = !is.na(m$creatinine)
print(paste(sum(missing_BUN & Available_cr), 'observations will now be imputed'))
```

```
## [1] "360 observations will now be imputed"
```

```r
m$BUN[missing_BUN & Available_cr] = predict(mod_impute4,newdata=m[missing_BUN & Available_cr,], re.form=NA)
```

Resulting data we can now use:
The contributions of the different studies:

```r
vars_interest = c('outcome','HCT','LPAR_pct','BD','BUN','poedema',
                  'convulsions','coma','AgeInYear','drug_class')
complete_cases = apply(m[,vars_interest], 1, function(x) sum(is.na(x))) == 0
Complete_Leg_data = m[complete_cases,] # for the model fitting
Complete_Leg_data$studyID = as.factor(as.character(Complete_Leg_data$studyID))
# Whole dataset
table(m$studyID)
```

```
## 
##          AAV           AQ      AQUAMAT Core Malaria    SEAQUAMAT 
##          370          560         5494         1116         1461
```

```r
# in the complete dataset (all variables recorded)
table(Complete_Leg_data$studyID)
```

```
## 
##          AAV           AQ      AQUAMAT Core Malaria    SEAQUAMAT 
##          214          150         3666          657         1333
```

```r
Complete_Leg_data$drug_AS = 0
Complete_Leg_data$drug_AS[Complete_Leg_data$drug_class=='artemisinin']=1

# remove infinite log parasitaemias
ind_keep = !(is.infinite(Complete_Leg_data$LPAR_pct) | is.nan(Complete_Leg_data$LPAR_pct))
Complete_Leg_data = Complete_Leg_data[ind_keep,]
```

Data summaries

```r
Africa = c('The Gambia','Mozambique','Ghana','Kenya','Nigeria','Tanzania','Uganda','Rwanda','Congo')
Asia = c('Thailand','Vietnam','Bangladesh','Myanmar','India','Indonesia')
writeLines(paste('Children in Africa:',
                 sum(m$AgeInYear < 15 & m$country %in% Africa)))
```

```
## Children in Africa: 5426
```

```r
writeLines(paste('Adults in Africa:',
                 sum(m$AgeInYear >= 15 & m$country %in% Africa)))
```

```
## Adults in Africa: 68
```

```r
writeLines(paste('Children in Asia:',
                 sum(m$AgeInYear < 15 & m$country %in% Asia)))
```

```
## Children in Asia: 282
```

```r
writeLines(paste('Adults in Asia:',
                 sum(m$AgeInYear >= 15 & m$country %in% Asia)))
```

```
## Adults in Asia: 3225
```



```r
mod_full_GAM = gam(outcome ~ s(HCT,AgeInYear) + LPAR_pct  + coma + convulsions +
                     poedema + log10(BUN) + BD + drug_AS + 
                     s(studyID, bs='re') + s(country, bs='re'),
                   data=Complete_Leg_data, family=binomial)
summary(mod_full_GAM)
```

```
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## outcome ~ s(HCT, AgeInYear) + LPAR_pct + coma + convulsions + 
##     poedema + log10(BUN) + BD + drug_AS + s(studyID, bs = "re") + 
##     s(country, bs = "re")
## 
## Parametric coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -6.329109   0.271587 -23.304  < 2e-16 ***
## LPAR_pct     -0.006194   0.060809  -0.102 0.918869    
## coma          1.332485   0.101673  13.106  < 2e-16 ***
## convulsions1  0.527219   0.118296   4.457 8.32e-06 ***
## poedema1      0.554431   0.384323   1.443 0.149129    
## log10(BUN)    1.712847   0.171026  10.015  < 2e-16 ***
## BD            0.123154   0.007423  16.592  < 2e-16 ***
## drug_AS      -0.347898   0.091972  -3.783 0.000155 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                       edf Ref.df Chi.sq  p-value    
## s(HCT,AgeInYear) 5.235025  7.296 32.947 3.47e-05 ***
## s(studyID)       0.003429  4.000  0.003    0.403    
## s(country)       9.951536 14.000 76.078 1.11e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.277   Deviance explained = 29.9%
## UBRE = -0.44222  Scale est. = 1         n = 5948
```

