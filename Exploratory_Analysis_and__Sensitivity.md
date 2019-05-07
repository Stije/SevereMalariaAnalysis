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
## [1] "AAV , mortality of: 10 %"
## [1] "AQ , mortality of: 15 %"
## [1] "Core Malaria , mortality of: 22 %"
## [1] "AQUAMAT , mortality of: 10 %"
## [1] "SEAQUAMAT , mortality of: 19 %"
## [1] "QC , mortality of: 17 %"
```

We look at the quantiles of the ages in the different studies:

```
## [1] "AAV, ages:15 AAV, ages:32 AAV, ages:77"
## [1] "AQ, ages:15 AQ, ages:30 AQ, ages:79"
## [1] "Core Malaria, ages:1 Core Malaria, ages:26 Core Malaria, ages:80"
## [1] "AQUAMAT, ages:0 AQUAMAT, ages:2 AQUAMAT, ages:78"
## [1] "SEAQUAMAT, ages:2 SEAQUAMAT, ages:25 SEAQUAMAT, ages:87"
## [1] "QC, ages:1 QC, ages:4 QC, ages:10"
```

```
## [1] "AAV"
## 
##     artemisinin non-artemisinin 
##             370               0 
## [1] "AQ"
## 
##     artemisinin non-artemisinin 
##             284             276 
## [1] "Core Malaria"
## 
##     artemisinin non-artemisinin 
##             483             624 
## [1] "AQUAMAT"
## 
##     artemisinin non-artemisinin 
##            2746            2748 
## [1] "SEAQUAMAT"
## 
##     artemisinin non-artemisinin 
##             730             731 
## [1] "QC"
## 
##     artemisinin non-artemisinin 
##               0              48
```


Let's look at the linear associations between the key baseline variables. We use mixed effects linear models to estimate these associations (random effect terms for both country and study).




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
## (Intercept)    4.623      1.924   2.402   0.0163 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                  edf Ref.df       F  p-value    
## s(log10(BUN))  4.212  5.209  251.52  < 2e-16 ***
## s(studyID)     2.782  3.000 2643.35 9.98e-14 ***
## s(country)    11.355 14.000   31.69  0.00835 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.285   Deviance explained = 28.7%
## GCV = 38.563  Scale est. = 38.422    n = 5288
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
## (Intercept)  23.7556     0.9751   24.36   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                 edf Ref.df     F  p-value    
## s(AgeInYear)  7.494  8.312 42.06  < 2e-16 ***
## s(studyID)    1.492  3.000 19.43   0.0317 *  
## s(country)   12.508 14.000 11.70 1.25e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.256   Deviance explained = 25.9%
## GCV = 70.143  Scale est. = 69.845    n = 5288
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ExploratoryPlots-1.pdf)<!-- -->

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

modshock=gam(outcome ~ shock + s(studyID, bs='re') + s(country, bs='re'),
             data = Leg_data_complete, family='binomial')

modBUN=gam(outcome ~ s(log10(BUN)) + s(studyID, bs='re') + s(country, bs='re'),
           data = Leg_data_complete, family='binomial')

modLPAR_pct=gam(outcome ~ s(LPAR_pct) + s(studyID, bs='re') + s(country, bs='re'),
                data = Leg_data_complete, family='binomial')
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/UnadjustedPlots-1.pdf)<!-- -->

Compute risk ratios with confidence intervals for the binary predictors:

```r
writeLines('\nComa risk ratio calculations:\n')
```

```
## 
## Coma risk ratio calculations:
```

```r
riskratio(x = table(Leg_data_complete$coma,Leg_data_complete$outcome),method = 'wald')
```

```
## $data
##        
##            0   1 Total
##   0     3188 196  3384
##   1     1414 480  1894
##   Total 4602 676  5278
## 
## $measure
##    risk ratio with 95% C.I.
##     estimate    lower    upper
##   0 1.000000       NA       NA
##   1 4.375579 3.742334 5.115977
## 
## $p.value
##    two-sided
##     midp.exact fisher.exact   chi.square
##   0         NA           NA           NA
##   1          0 1.895411e-88 2.152294e-92
## 
## $correction
## [1] FALSE
## 
## attr(,"method")
## [1] "Unconditional MLE & normal approximation (Wald) CI"
```

```r
writeLines('\nShock risk ratio calculations:\n')
```

```
## 
## Shock risk ratio calculations:
```

```r
riskratio(x = table(Leg_data_complete$shock,Leg_data_complete$outcome),method = 'wald')
```

```
## $data
##        
##            0   1 Total
##   0     4382 624  5006
##   1      227  55   282
##   Total 4609 679  5288
## 
## $measure
##    risk ratio with 95% C.I.
##     estimate    lower    upper
##   0 1.000000       NA       NA
##   1 1.564659 1.220734 2.005482
## 
## $p.value
##    two-sided
##      midp.exact fisher.exact   chi.square
##   0          NA           NA           NA
##   1 0.001157578  0.001267247 0.0005868513
## 
## $correction
## [1] FALSE
## 
## attr(,"method")
## [1] "Unconditional MLE & normal approximation (Wald) CI"
```

```r
writeLines('\nConvulsions risk ratio calculations:\n')
```

```
## 
## Convulsions risk ratio calculations:
```

```r
riskratio(x = table(Leg_data_complete$convulsions,Leg_data_complete$outcome),method = 'wald')
```

```
## $data
##        
##            0   1 Total
##   0     3701 483  4184
##   1      908 196  1104
##   Total 4609 679  5288
## 
## $measure
##    risk ratio with 95% C.I.
##     estimate    lower    upper
##   0 1.000000       NA       NA
##   1 1.537912 1.320828 1.790676
## 
## $p.value
##    two-sided
##       midp.exact fisher.exact   chi.square
##   0           NA           NA           NA
##   1 1.141867e-07 1.277466e-07 4.111691e-08
## 
## $correction
## [1] FALSE
## 
## attr(,"method")
## [1] "Unconditional MLE & normal approximation (Wald) CI"
```

```r
writeLines('\nPulmonary oedema risk ratio calculations:\n')
```

```
## 
## Pulmonary oedema risk ratio calculations:
```

```r
riskratio(x = table(Leg_data_complete$poedema,Leg_data_complete$outcome),method = 'wald')
```

```
## $data
##        
##            0   1 Total
##   0     4466 649  5115
##   1      143  30   173
##   Total 4609 679  5288
## 
## $measure
##    risk ratio with 95% C.I.
##     estimate    lower    upper
##   0 1.000000       NA       NA
##   1 1.366709 0.979436 1.907112
## 
## $p.value
##    two-sided
##     midp.exact fisher.exact chi.square
##   0         NA           NA         NA
##   1 0.08207342   0.08212745 0.07199053
## 
## $correction
## [1] FALSE
## 
## attr(,"method")
## [1] "Unconditional MLE & normal approximation (Wald) CI"
```


## Haematocrit distributions


```r
par(las=1, bty='n')
f=ecdf(m$HCT[m$continent=="Africa"])
f2=ecdf(m$HCT[m$continent=="Asia"])
plot(5:50, 100*f(5:50), xlim=c(5,50), main='', lwd=2,xlab='Haematocrit (%)',
     xaxt='n',ylab='Cumulative proportion (%)',type='l',col='blue')
lines(5:50, 100*f2(5:50), col='green',lwd=2)
axis(1, at = c(5,10,15,20,25,35,45))

f_inverse = approxfun(x = f(1:50),y = 1:50)
f2_inverse = approxfun(x = f2(1:50),y = 1:50)

lines(c(-10, f2_inverse(.1)), c(10,10), lty=2)
lines(c(-10, f2_inverse(.5)), c(50,50), lty=2)
lines(c(f2_inverse(.5),f2_inverse(.5)), c(-10,50), lty=2)
lines(c(f_inverse(.5),f_inverse(.5)), c(-10,50), lty=2)
lines(c(f2_inverse(.1),f2_inverse(.1)), c(-10,10), lty=2)
lines(c(f_inverse(.1),f_inverse(.1)), c(-10,10), lty=2)

legend('bottomright',col=c('green','blue'),inset = 0.01,
       legend = c('Asian data','African data'),lwd=3)
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/haematocrit_distributions-1.pdf)<!-- -->


# Sensitivity Analysis

## Only using complete records


```r
vars_interest = c('outcome','HCT','LPAR_pct','coma','convulsions','poedema',
                  'BUN','BD','shock','hypoglycaemia',
                  'studyID','country')
complete_cases = apply(m[,vars_interest], 1, function(x) sum(is.na(x))) == 0
Complete_Leg_data = m[complete_cases,] 
Complete_Leg_data$studyID = as.factor(as.character(Complete_Leg_data$studyID))
```


```r
DAG_fmla = "outcome ~ HCT + LPAR_pct + coma + convulsions + poedema + 
              log2(BUN) + BD + shock + hypoglycaemia + 
              drug_AS + (1 | studyID) + (1 | country)"
# fit the model to each dataset
mod1 = glmer(formula = DAG_fmla, data = Complete_Leg_data, 
             family=binomial) 
summary(mod1)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## outcome ~ HCT + LPAR_pct + coma + convulsions + poedema + log2(BUN) +  
##     BD + shock + hypoglycaemia + drug_AS + (1 | studyID) + (1 |  
##     country)
##    Data: Complete_Leg_data
## 
##      AIC      BIC   logLik deviance df.resid 
##   2849.9   2935.4  -1412.0   2823.9     5258 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.3612 -0.3118 -0.1772 -0.1036 17.8396 
## 
## Random effects:
##  Groups  Name        Variance Std.Dev.
##  country (Intercept) 0.235273 0.48505 
##  studyID (Intercept) 0.004596 0.06779 
## Number of obs: 5271, groups:  country, 15; studyID, 4
## 
## Fixed effects:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -6.930507   0.349185 -19.848  < 2e-16 ***
## HCT            0.012967   0.005811   2.231 0.025651 *  
## LPAR_pct      -0.016817   0.066500  -0.253 0.800352    
## coma           1.303101   0.110001  11.846  < 2e-16 ***
## convulsions1   0.493248   0.122671   4.021  5.8e-05 ***
## poedema1       0.225026   0.281040   0.801 0.423311    
## log2(BUN)      0.608771   0.053665  11.344  < 2e-16 ***
## BD             0.105341   0.007833  13.449  < 2e-16 ***
## shock1        -0.152711   0.204050  -0.748 0.454221    
## hypoglycaemia  0.553260   0.155038   3.569 0.000359 ***
## drug_AS       -0.386722   0.100201  -3.859 0.000114 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) HCT    LPAR_p coma   cnvls1 poedm1 l2(BUN BD     shock1
## HCT         -0.471                                                        
## LPAR_pct     0.028  0.005                                                 
## coma        -0.130 -0.055  0.044                                          
## convulsins1 -0.105 -0.084  0.028 -0.246                                   
## poedema1    -0.031 -0.009 -0.030 -0.015  0.006                            
## log2(BUN)   -0.709  0.037 -0.091  0.003  0.112 -0.009                     
## BD          -0.082  0.183 -0.146 -0.036  0.023 -0.024 -0.275              
## shock1      -0.016 -0.009 -0.054  0.013 -0.022 -0.020  0.014 -0.137       
## hypoglycaem -0.006  0.018 -0.025 -0.047  0.070  0.018  0.018 -0.240 -0.007
## drug_AS     -0.057 -0.019  0.008 -0.025  0.026  0.016 -0.066 -0.014 -0.024
##             hypgly
## HCT               
## LPAR_pct          
## coma              
## convulsins1       
## poedema1          
## log2(BUN)         
## BD                
## shock1            
## hypoglycaem       
## drug_AS     -0.005
```


```r
# The scalar multiples to put the AORs on the correct scales
# Compute 95% CIs
FixedEffs = as.data.frame(summary(mod1)$coefficients)
FixedEffs$Scalar_f = c(1,10, 1, 1, 1, 1,log2(3), 10, 1, 1,1)
Results = data.frame(lowerCI = exp(FixedEffs$Scalar_f*(FixedEffs$Estimate -
                                                         1.96*FixedEffs$`Std. Error`)),
                     mean = exp(FixedEffs$Scalar_f*(FixedEffs$Estimate)),
                     upperCI = exp(FixedEffs$Scalar_f*(FixedEffs$Estimate +
                                                         1.96*FixedEffs$`Std. Error`)))
rownames(Results) = rownames(FixedEffs)
```

Make the 'forest' plot:

```r
plotting_ind = rownames(Results) %in% c('BD','coma','convulsions1','LPAR_pct',
                                        'drug_AS','HCT','log2(BUN)','poedema1','shock1')
Results = Results[plotting_ind,]
Results$Y_Labels = c('-10 % points\nabsolute haematocrit\non admission',
                     'Tenfold increase\nin parasitaemia',
                     'Coma\non admission',
                     'Seizures\non admission',
                     'Pulmonary\nOedema\non admission',
                     '3 fold increase\nin blood urea\nnitrogen (mmol/L)',
                     '+10 mEq/L\nbase deficit',
                     'Shock\non admission',
                     'Artemisinin drug\nversus\nnon Artemisinin drug'
)

Results['HCT',1:3] = 1/Results['HCT',1:3]
x_ind = sort.int(Results$mean, index.return = T)$ix
Results = Results[x_ind,]
par(bty='n', las=1, mar = c(3,9,2,2))

print(Results)
```

```
##                lowerCI      mean   upperCI
## drug_AS      0.5581564 0.6792801 0.8266885
## shock1       0.5754231 0.8583779 1.2804711
## HCT          0.9843492 0.8783824 0.7838232
## LPAR_pct     0.8631577 0.9833232 1.1202178
## poedema1     0.7219408 1.2523550 2.1724676
## convulsions1 1.2876414 1.6376271 2.0827401
## log2(BUN)    2.2214697 2.6244721 3.1005843
## BD           2.4593147 2.8673981 3.3431963
## coma         2.9668460 3.6806934 4.5662985
##                                                       Y_Labels
## drug_AS         Artemisinin drug\nversus\nnon Artemisinin drug
## shock1                                     Shock\non admission
## HCT           -10 % points\nabsolute haematocrit\non admission
## LPAR_pct                     Tenfold increase\nin parasitaemia
## poedema1                       Pulmonary\nOedema\non admission
## convulsions1                            Seizures\non admission
## log2(BUN)    3 fold increase\nin blood urea\nnitrogen (mmol/L)
## BD                                     +10 mEq/L\nbase deficit
## coma                                        Coma\non admission
```

```r
xlims = c(0.5, 8.5)
plot(NA,NA, xlim= log2(xlims), ylim = c(0,1),xaxt='n',
     xlab='', ylab='', yaxt='n')
axis(1, at = log2(c(0.5,1, 2,4)), labels = c(0.5,1, 2,4))
abline(v=0, lty=2, lwd=3, col='red')
yindex =1
ypos = seq(0,1,length.out = sum(plotting_ind))


for(i in 1:nrow(Results)){
  arrows(log2(Results[i,'lowerCI']),ypos[yindex],
         log2(Results[i,'upperCI']),ypos[yindex],
         length=0.0, angle=90, code=3, 
         col = 'black',lwd=3)
  points(log2(Results[i,'mean']),ypos[yindex],pch=18,cex=2)
  yindex=yindex+1
  
}
abline(h=ypos, lty=3)
axis(side = 2, at = ypos, labels = Results$Y_Labels,tick=FALSE)
mtext(side=1, line = 2, text = 'Adjusted odds ratio')
mtext(side = 3, line = 1, text = 'Increased survival',adj = 0)
mtext(side = 3, line = 1, text = 'Decreased survival',adj = 1)
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ForestPlot_CompleteData-1.pdf)<!-- -->

# Trim very low haematocrits


```r
load('RData/Multiple_Imputed_Datasets.RData')
for (i in 1:length(SM_Impute_List)){
  SM_Impute_List[[i]] = filter(SM_Impute_List[[i]], HCT > 9)
}
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```





```r
# extract the fixed and random effects from all the model fits
# These functions then compute the overall estimates
FixedEffs = modelFixedEff(modList)
RandEffs = modelRandEffStats(modList)
print(FixedEffs)
```

```
##             term    estimate   std.error   statistic           df
## 1    (Intercept) -6.79067862 0.313048839 -21.6920741 5.207770e+06
## 2             BD  0.10481662 0.006608695  15.8604118 1.364988e+10
## 3           coma  1.27104557 0.083136179  15.2887177 3.873128e+06
## 4   convulsions1  0.31008122 0.098057022   3.1622541 3.403551e+08
## 5        drug_AS -0.43529030 0.077078324  -5.6473763 2.091954e+09
## 6            HCT  0.01840701 0.004745283   3.8790118 6.090729e+10
## 7  hypoglycaemia  0.55675179 0.128222168   4.3420869 5.791476e+07
## 8      log2(BUN)  0.57502987 0.044026176  13.0610904 1.163475e+08
## 9       LPAR_pct  0.04531736 0.056097644   0.8078299 1.848633e+08
## 10      poedema1  0.26474452 0.117872863   2.2460175 3.513052e+04
## 11        shock1  0.40923796 0.148934327   2.7477746 8.518852e+08
```

```r
print(RandEffs)
```

```
##                               term             group  estimate   std.error
## 1         sd_(Intercept).continent         continent 0.1292687 0.012132853
## 2 sd_(Intercept).country:continent country:continent 0.4738147 0.003625118
```

```r
FixedEffs$Scalar_f = as.numeric(mapvalues(x = FixedEffs$term, 
                                          from = c("(Intercept)",
                                                   "BD",
                                                   "coma",
                                                   "convulsions1",
                                                   "drug_AS",
                                                   "HCT",
                                                   "hypoglycaemia",
                                                   "log2(BUN)",
                                                   "LPAR_pct",
                                                   "shock1",
                                                   "poedema1"),
                                          to = as.numeric(c(1, 7, 1, 1, 1, 10, 1,2,log10(6),1, 1))))
FixedEffs$std.error=as.numeric(FixedEffs$std.error)
FixedEffs$estimate=as.numeric(FixedEffs$estimate)

# Compute 95% CIs
Results = data.frame(lowerCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate -
                                                         1.96*FixedEffs$std.error)),
                     mean = exp(FixedEffs$Scalar_f*(FixedEffs$estimate)),
                     upperCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate +
                                                         1.96*FixedEffs$std.error)))
rownames(Results) = FixedEffs$term
```

Aggregate results for plotting:

```r
writeLines(sprintf('Standard deviation of base deficit in patient population is %s',
                   round(sd(m$BD,na.rm=T))))
```

```
## Standard deviation of base deficit in patient population is 7
```

```r
writeLines(sprintf('Standard deviation of blood urea nitrogen in patient population is %s',
                   round(2^sd(log2(m$BUN),na.rm=T))))
```

```
## Standard deviation of blood urea nitrogen in patient population is 2
```

```r
writeLines(sprintf('Standard deviation of parasitised RBCs in patient population is %s',
                   round(10^sd(m$LPAR_pct,na.rm=T))))
```

```
## Standard deviation of parasitised RBCs in patient population is 6
```

```r
writeLines(sprintf('Standard deviation of haematocrits in patient population is %s',
                   round(sd(m$HCT,na.rm=T))))
```

```
## Standard deviation of haematocrits in patient population is 10
```



Make the 'forest' plot:

```r
Results = Results[rownames(Results) %in% c('BD','coma','convulsions1','drug_AS','HCT',
                                           'log2(BUN)','poedema1','LPAR_pct','shock1'),]
Results$Names =mapvalues(rownames(Results),
                         from = c("drug_AS",
                                  "HCT",
                                  "LPAR_pct",
                                  "convulsions1",
                                  "shock1",
                                  "poedema1",
                                  "log2(BUN)",
                                  "BD", 
                                  "coma"),
                         to = c('Artemisinin drug\nversus\nnon Artemisinin drug',
                                '-10 % points\nabsolute haematocrit\non admission',
                                'Six fold increase\n in parasitised\nred blood cells',
                                'Seizures\non admission',
                                'Shock\non admission',
                                'Pulmonary\noedema\non admission',
                                'Two fold increase\nin blood urea\nnitrogen (mmol/L)',
                                '+7 mEq/L\nbase deficit',
                                'Coma\non admission'))
Results['HCT',c("lowerCI","mean","upperCI")] = 1/Results['HCT',c("lowerCI","mean","upperCI")]
Results = arrange(Results, mean)
print(cbind(Results[,'Names'],round(Results[,c("lowerCI","mean","upperCI")],2)))
```

```
##                                    Results[, "Names"] lowerCI mean upperCI
## 1      Artemisinin drug\nversus\nnon Artemisinin drug    0.56 0.65    0.75
## 2    -10 % points\nabsolute haematocrit\non admission    0.91 0.83    0.76
## 3 Six fold increase\n in parasitised\nred blood cells    0.95 1.04    1.13
## 4                     Pulmonary\noedema\non admission    1.03 1.30    1.64
## 5                              Seizures\non admission    1.13 1.36    1.65
## 6                                 Shock\non admission    1.12 1.51    2.02
## 7                              +7 mEq/L\nbase deficit    1.90 2.08    2.28
## 8 Two fold increase\nin blood urea\nnitrogen (mmol/L)    2.66 3.16    3.75
## 9                                  Coma\non admission    3.03 3.56    4.20
```

```r
par(bty='n', las=1, mar = c(4,9,2,2))

xlims = c(0.5, 4.5)
plot(NA,NA, xlim= log2(xlims), ylim = c(0,1),xaxt='n',
     xlab='', ylab='', yaxt='n')
abline(v= seq(-1,3,by=1),col = "lightgray", lty = "dotted",lwd = par("lwd"))
axis(1, at = log2(c(0.5,1, 2,4)), labels = c(0.5,1, 2,4))
abline(v=0, lty=2, lwd=3, col='red')
yindex =1
ypos = seq(0,1,length.out = nrow(Results))


for(i in 1:nrow(Results)){
  arrows(log2(Results[i,'lowerCI']),ypos[yindex],
         log2(Results[i,'upperCI']),ypos[yindex],
         length=0.0, angle=90, code=3, 
         col = 'black',lwd=3)
  points(log2(Results[i,'mean']),ypos[yindex],pch=18,cex=2)
  yindex=yindex+1
  
}
abline(h=ypos, lty=3)
axis(side = 2, at = ypos, labels = Results$Names,tick=FALSE)
mtext(side=1, line = 2.5, text = 'Adjusted odds ratio')
mtext(side = 3, line = 1, text = 'Increased survival',adj = 0)
mtext(side = 3, line = 1, text = 'Decreased survival',adj = 1)
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ForestPlot_SM_trimmedVerySMA-1.pdf)<!-- -->

## The effect of Transfusion: AAV, AQ and AQUAMAT

For these three studies we have detailed transfusion data, with over half of patients being transfused:

```r
table(m$transfusion,m$studyID,useNA = 'ifany')
```

```
##       
##         AAV   AQ AQUAMAT Core Malaria   QC SEAQUAMAT
##   0     232  420    2498          189   15      1454
##   1     138  140    2996           64    6         7
##   <NA>    0    0       0          854   27         0
```

Select data from AQUAMAT, AAV and AQ:

```r
load('RData/Multiple_Imputed_Datasets.RData')
for(i in 1:length(SM_Impute_List)){
  SM_Impute_List[[i]] = filter(SM_Impute_List[[i]], studyID%in%c('AQUAMAT','AAV','AQ'))
}
```

### The naive analysis: no stratification by time of death



```r
DAG_fmla = "outcome ~ HCT + LPAR_pct + coma + convulsions + poedema + 
              log2(BUN) + BD + shock + hypoglycaemia + transfusion +
              drug_AS + (1 | country)"
if(RUN_MODELS){
  # fit the model to each dataset
  modList = glmerModList(DAG_fmla, data = SM_Impute_List,
                         family=binomial,parallel = T) 
  # save the output
  save(modList, file = 'RData/ModelList_transfusion_Naive.RData')
} else {
  load('RData/ModelList_transfusion_Naive.RData')
}

FixedEffs = modelFixedEff(modList)
print(FixedEffs)
```

```
##             term    estimate   std.error   statistic           df
## 1    (Intercept) -6.08950816 0.345605333 -17.6198327 5.151661e+06
## 2             BD  0.10704355 0.007554864  14.1688265 1.184679e+10
## 3           coma  1.28259223 0.104405541  12.2847142 2.210427e+08
## 4   convulsions1  0.37072597 0.111036654   3.3387711 1.386594e+09
## 5        drug_AS -0.35488827 0.093914306  -3.7788521 3.046264e+09
## 6            HCT -0.00240425 0.006732129  -0.3571307 8.066231e+10
## 7  hypoglycaemia  0.65292283 0.127710110   5.1125383 7.642439e+08
## 8      log2(BUN)  0.52162593 0.055543370   9.3913266 8.162220e+07
## 9       LPAR_pct -0.04475890 0.085763143  -0.5218897 5.068625e+07
## 10      poedema1  0.21093126 0.128824419   1.6373546 1.956197e+04
## 11        shock1  0.73521912 0.189296788   3.8839493 5.112656e+08
## 12   transfusion -0.48778589 0.119334302  -4.0875580 4.249039e+08
```

```r
FixedEffs$Scalar_f = as.numeric(mapvalues(x = FixedEffs$term, 
                                          from = c("(Intercept)", "BD","coma", "convulsions1",
                                                   "drug_AS","HCT","hypoglycaemia", "log2(BUN)",
                                                   "LPAR_pct","shock1","poedema1", "transfusion"),
                                          to = as.numeric(c(1, 7, 1, 1, 1, 
                                                            10, 1,2,log10(6),1, 1,1))))
FixedEffs$std.error=as.numeric(FixedEffs$std.error)
FixedEffs$estimate=as.numeric(FixedEffs$estimate)

# Compute 95% CIs
Results = data.frame(lowerCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate -
                                                         1.96*FixedEffs$std.error)),
                     mean = exp(FixedEffs$Scalar_f*(FixedEffs$estimate)),
                     upperCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate +
                                                         1.96*FixedEffs$std.error)))
rownames(Results) = FixedEffs$term
Results['HCT',1:3] = 1/Results['HCT',1:3]
Results$Names =mapvalues(rownames(Results),
                         from = c("drug_AS","HCT","LPAR_pct","convulsions1","shock1",
                                  "poedema1","log2(BUN)","BD", "coma","transfusion"),
                         to = c('Artemisinin drug\nversus\nnon Artemisinin drug',
                                '-10 % points\nabsolute haematocrit\non admission',
                                'Six fold increase\n in parasitised\nred blood cells',
                                'Seizures\non admission',
                                'Shock\non admission',
                                'Pulmonary\noedema\non admission',
                                'Two fold increase\nin blood urea\nnitrogen (mmol/L)',
                                '+7 mEq/L\nbase deficit',
                                'Coma\non admission','Transfusion'))
```


Make the 'forest' plot:

```r
plotting_ind = rownames(Results) %in% c('BD','coma','convulsions1','LPAR_pct','transfusion',
                                        'drug_AS','HCT','log2(BUN)','poedema1','shock1')
Results = Results[plotting_ind,]



x_ind = sort.int(Results$mean, index.return = T)$ix
Results = Results[x_ind,]
par(bty='n', las=1, mar = c(3,9,2,2))

print(Results)
```

```
##                lowerCI      mean   upperCI
## transfusion  0.4859347 0.6139843 0.7757766
## drug_AS      0.5833544 0.7012518 0.8429765
## LPAR_pct     0.8473571 0.9657704 1.1007312
## HCT          1.1688171 1.0243338 0.8977109
## poedema1     0.9592874 1.2348275 1.5895121
## convulsions1 1.1654347 1.4487860 1.8010284
## shock1       1.4393583 2.0859390 3.0229731
## BD           1.9072304 2.1155290 2.3465769
## log2(BUN)    2.2830719 2.8384322 3.5288847
## coma         2.9386708 3.6059751 4.4248089
##                                                            Names
## transfusion                                          Transfusion
## drug_AS           Artemisinin drug\nversus\nnon Artemisinin drug
## LPAR_pct     Six fold increase\n in parasitised\nred blood cells
## HCT             -10 % points\nabsolute haematocrit\non admission
## poedema1                         Pulmonary\noedema\non admission
## convulsions1                              Seizures\non admission
## shock1                                       Shock\non admission
## BD                                        +7 mEq/L\nbase deficit
## log2(BUN)    Two fold increase\nin blood urea\nnitrogen (mmol/L)
## coma                                          Coma\non admission
```

```r
xlims = c(0.5, 8.5)
plot(NA,NA, xlim= log2(xlims), ylim = c(0,1),xaxt='n',
     xlab='', ylab='', yaxt='n')
axis(1, at = log2(c(0.5,1, 2,4)), labels = c(0.5,1, 2,4))
abline(v=0, lty=2, lwd=3, col='red')
yindex =1
ypos = seq(0,1,length.out = sum(plotting_ind))


for(i in 1:nrow(Results)){
  arrows(log2(Results[i,'lowerCI']),ypos[yindex],
         log2(Results[i,'upperCI']),ypos[yindex],
         length=0.0, angle=90, code=3, 
         col = 'black',lwd=3)
  points(log2(Results[i,'mean']),ypos[yindex],pch=18,cex=2)
  yindex=yindex+1
  
}
abline(h=ypos, lty=3)
axis(side = 2, at = ypos, labels = Results$Names,tick=FALSE)
mtext(side=1, line = 2, text = 'Adjusted odds ratio')
mtext(side = 3, line = 1, text = 'Increased survival',adj = 0)
mtext(side = 3, line = 1, text = 'Decreased survival',adj = 1)
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ForestPlot_Transfusion_Naive-1.pdf)<!-- -->

We see that transfusion is associated with a lower odds of death.
However, this is a biased estimate as transfusion is a collider (see paper).

### Stratification by time to death

We stratify by time to death, to remove collider bias.
Select the patients who survive past 4 hours

```r
#### We're just looking at the those who didn't die before 4 hours post admission
m$Outcome4hours = 0
m$Outcome4hours[!is.na(m$Timetodeathhrs) & m$Timetodeathhrs < 4] = 1
# We only look at individuals who survive past 4 hours
m = filter(m, Outcome4hours==0)

Transfusion_Data = m[m$studyID%in%c('AQUAMAT','AAV','AQ'),]
round(100*table(Transfusion_Data$transfusion)/nrow(Transfusion_Data),1)
```

```
## 
##    0    1 
## 48.5 51.5
```

```r
unique_ids_Transfusion = Transfusion_Data$Unique_ID
```

We load the imputed datasets and select patients whose transfusion information was recorded (AQ,AAV and AQUAMAT), and who did not die in the first 4 hours

```r
for(i in 1:length(SM_Impute_List)){
  ind = SM_Impute_List[[i]]$Unique_ID %in% unique_ids_Transfusion
  SM_Impute_List[[i]] = SM_Impute_List[[i]][ind,]
}

SM_Impute_List = SM_Impute_List[1:50]
```

Fit the model to these data:


Compute the overall parameter estimates:

```r
# extract the fixed effects from all the model fits
# These functions then compute the overall estimates
FixedEffs = modelFixedEff(modList)
print(FixedEffs)
```

```
##             term     estimate   std.error   statistic          df
## 1    (Intercept) -6.686903277 0.362571156 -18.4430095     5161374
## 2             BD  0.102097056 0.008021074  12.7286012 11337639328
## 3           coma  1.174346771 0.110349632  10.6420543  7453424089
## 4   convulsions1  0.506167702 0.117144543   4.3208816 12740852443
## 5        drug_AS -0.371407858 0.099780032  -3.7222664 11039322616
## 6            HCT  0.009978276 0.007212356   1.3834974 90092186638
## 7  hypoglycaemia  0.562891717 0.138301417   4.0700358  4020246409
## 8      log2(BUN)  0.551148271 0.057928207   9.5143333    36804020
## 9       LPAR_pct -0.020949238 0.091181755  -0.2297525    17521450
## 10        shock1  0.347770972 0.218596071   1.5909297   786959778
## 11   transfusion -0.101392735 0.129164052  -0.7849919  1830262028
```


```r
# The scalar multiples to put the AORs on the correct scales
# Compute 95% CIs
FixedEffs$Scalar_f = as.numeric(mapvalues(FixedEffs$term, 
                                          from = c('(Intercept)',
                                                   "hypoglycaemia",
                                                   'BD',
                                                   'coma',
                                                   'convulsions1',
                                                   'LPAR_pct',
                                                   'transfusion',
                                                   'drug_AS',
                                                   'HCT',
                                                   'log2(BUN)',
                                                   'poedema1',
                                                   'shock1'),
                                          to = c(1,1,10, 1, 1, 1, 1,1,10,log2(3),1,1)))
```

```
## The following `from` values were not present in `x`: poedema1
```

```r
Results = data.frame(lowerCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate -
                                                         1.96*FixedEffs$`std.error`)),
                     mean = exp(FixedEffs$Scalar_f*(FixedEffs$estimate)),
                     upperCI = exp(FixedEffs$Scalar_f*(FixedEffs$estimate +
                                                         1.96*FixedEffs$`std.error`)))
Results$term = FixedEffs$term
plotting_ind = Results$term %in% c('BD','coma','convulsions1','LPAR_pct','transfusion',
                                   'drug_AS','HCT','log2(BUN)','poedema1','shock1')
Results = Results[plotting_ind,]
Results$Y_Labels = mapvalues(Results$term, 
                             from = c('BD',
                                      'coma',
                                      'convulsions1',
                                      'LPAR_pct',
                                      'transfusion',
                                      'drug_AS',
                                      'HCT',
                                      'log2(BUN)',
                                      'poedema1',
                                      'shock1'),
                             to = c('+10 mEq/L\nbase deficit',
                                    'Coma\non admission',
                                    'Seizures\non admission',
                                    'Tenfold increase\nin parasitaemia',
                                    'Transfusion',
                                    'Artemisinin drug\nversus\nnon Artemisinin drug',
                                    '-10 % points\nabsolute haematocrit\non admission',
                                    '3 fold increase\nin blood urea\nnitrogen (mmol/L)',
                                    'Pulmonary\nOedema\non admission',
                                    'Shock\non admission'))
```

```
## The following `from` values were not present in `x`: poedema1
```


Make the 'forest' plot:

```r
Results[Results$term=='HCT',1:3] = 1/Results[Results$term=='HCT',1:3]
x_ind = sort.int(Results$mean, index.return = T)$ix
Results = Results[x_ind,]
par(bty='n', las=1, mar = c(3,9,2,2))

print(Results)
```

```
##      lowerCI      mean   upperCI         term
## 5  0.5672378 0.6897626 0.8387530      drug_AS
## 11 0.7014860 0.9035781 1.1638911  transfusion
## 6  1.0424560 0.9050340 0.7857277          HCT
## 9  0.8190047 0.9792687 1.1708933     LPAR_pct
## 10 0.9224913 1.4159079 2.1732403       shock1
## 4  1.3185919 1.6589215 2.0870905 convulsions1
## 8  2.0008935 2.3953979 2.8676843    log2(BUN)
## 2  2.3720570 2.7758876 3.2484684           BD
## 3  2.6066386 3.2360284 4.0173884         coma
##                                             Y_Labels
## 5     Artemisinin drug\nversus\nnon Artemisinin drug
## 11                                       Transfusion
## 6   -10 % points\nabsolute haematocrit\non admission
## 9                  Tenfold increase\nin parasitaemia
## 10                               Shock\non admission
## 4                             Seizures\non admission
## 8  3 fold increase\nin blood urea\nnitrogen (mmol/L)
## 2                            +10 mEq/L\nbase deficit
## 3                                 Coma\non admission
```

```r
xlims = c(0.5, 4)
plot(NA,NA, xlim= log2(xlims), ylim = c(0,1),xaxt='n',
     xlab='', ylab='', yaxt='n')
axis(1, at = log2(c(0.5,1, 2,4)), labels = c(0.5,1, 2,4))
abline(v=0, lty=2, lwd=3, col='red')
yindex =1
ypos = seq(0,1,length.out = sum(plotting_ind))


for(i in 1:nrow(Results)){
  arrows(log2(Results[i,'lowerCI']),ypos[yindex],
         log2(Results[i,'upperCI']),ypos[yindex],
         length=0.0, angle=90, code=3, 
         col = 'black',lwd=3)
  points(log2(Results[i,'mean']),ypos[yindex],pch=18,cex=2)
  yindex=yindex+1
  
}
abline(h=ypos, lty=3)
axis(side = 2, at = ypos, labels = Results$Y_Labels,tick=FALSE)
mtext(side=1, line = 2, text = 'Adjusted odds ratio')
mtext(side = 3, line = 1, text = 'Increased survival',adj = 0)
mtext(side = 3, line = 1, text = 'Decreased survival',adj = 1)
```

![](Exploratory_Analysis_and__Sensitivity_files/figure-html/ForestPlot_Transfusion_TD_stratified-1.pdf)<!-- -->




