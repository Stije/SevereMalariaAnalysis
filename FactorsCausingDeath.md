---
title: "Charactersing effect of anaemia on mortality in severe malaria"
output:
  html_document:
    keep_md: yes
    fig_caption: yes
---



# Background

This looks at the severe malaria legacy dataset from MORU










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

### Multiple imputation using linear relationships

The number of missing variables in the pooled data:

```r
apply(m,2, function(x) sum(is.na(x)))
```

```
##   StudyNumber          year       country       studyID    drug_class 
##             0           560             0             0             0 
##          drug         shock   convulsions       poedema       outcome 
##             0             0             0             0             0 
##     AgeInYear          coma           HCT        paraul          parc 
##             0           228           867          1321          1871 
##          LPAR      LPAR_pct            BD   bicarbonate            rr 
##          2027          1871          2325          3628           358 
##       lactate           BUN    creatinine hypoglycaemia   transfusion 
##          7940          1494          7193            67          2323 
##         study       drug_AS 
##             0             0
```

We make a few data adjustments for the model imputation and fitting:

```r
m$LPAR_pct[is.infinite(m$LPAR_pct)] = 0
# m$drug_class = as.factor(m$drug_class)
# m$poedema = as.factor(m$poedema)
# m$coma = as.factor(m$coma)
# m$convulsions = as.factor(m$convulsions)
# m$transfusion = as.factor(m$transfusion)
```

We run the multiple imputation using sequential linear models:

Setup the models.





```r
K_imputations = 200

SM_Impute_List = list()

for (k in 1:K_imputations){
  Imp_data = m
  
  # BD from bicarbonate
  coefs1 = summary(mod_impute1)$coefficients
  Imp_data$BD[ind1] = rnorm(n = sum(ind1), 
                            mean = predict(mod_impute1, newdata = m[ind1,]),
                            sd = coefs1[1,'Std. Error'] + 
                              coefs1[2,'Std. Error']*m$bicarbonate[ind1])
  
  # BD from lactate
  coefs2 = summary(mod_impute2)$coefficients
  Imp_data$BD[ind2] = rnorm(n = sum(ind2), 
                            mean = predict(mod_impute2, newdata = m[ind2,],allow.new.levels=T),
                            sd = coefs2[1,'Std. Error'] + 
                              coefs2[2,'Std. Error']*m$lactate[ind2])
  
  # BD from respiratory rate
  coefs3 = summary(mod_impute3)$coefficients
  Imp_data$BD[ind3] = rnorm(n = sum(ind3), 
                            mean = predict(mod_impute3, newdata = m[ind3,],allow.new.levels=T),
                            sd = coefs3[1,'Std. Error'] + 
                              coefs3[2,'Std. Error']*m$rr[ind3])
  
  # BUN from creatinine
  coefs4 = summary(mod_impute4)$coefficients
  Imp_data$BUN[ind4] = exp(rnorm(n = sum(ind4), 
                                 mean = predict(mod_impute4, newdata = m[ind4,],allow.new.levels=T),
                                 sd = coefs4[1,'Std. Error'] + 
                                   coefs4[2,'Std. Error']*m$creatinine[ind4]))
  
  # HCT from Age
  coefs5 = summary(mod_impute5)$coefficients
  Imp_data$HCT[ind5] = rnorm(n = sum(ind5), 
                             mean = predict(mod_impute5, newdata = m[ind5,],allow.new.levels=T),
                             sd = coefs5[1,'Std. Error'] + 
                               coefs5[2,'Std. Error']*m$AgeInYear[ind5])
  
  # BD from age
  coefs6 = summary(mod_impute6)$coefficients
  Imp_data$BD[ind6] = rnorm(n = sum(ind6), 
                            mean = predict(mod_impute6, newdata = m[ind6,],allow.new.levels=T),
                            sd = coefs6[1,'Std. Error'] + 
                              coefs6[2,'Std. Error']*m$AgeInYear[ind6])
  
  # Coma from hypoglycaemia
  coefs7 = summary(mod_impute7)$coefficients
  Imp_data$coma[ind7] = rbinom(n = sum(ind7), size = 1,
                               predict(mod_impute7, newdata = m[ind7,],
                                       allow.new.levels=T, type='response'))
  
  # Parasitaemia from age
  coefs8 = summary(mod_impute8)$coefficients
  Imp_data$LPAR_pct[ind8] = rnorm(n = sum(ind8), 
                                  mean = predict(mod_impute8, 
                                                 newdata = m[ind8,],
                                                 allow.new.levels=T),
                                  sd = coefs8[1,'Std. Error'] + 
                                    coefs8[2,'Std. Error']*m$AgeInYear[ind8])
  
  # Hypoglycaemia: marginal
  coefs9 = summary(mod_impute9)$coefficients
  Imp_data$hypoglycaemia[ind9] = rbinom(n = sum(ind9),size = 1, 
                                        prob = predict(mod_impute9, 
                                                       newdata = m[ind9,],
                                                       allow.new.levels=T,
                                                       type='response'))
  
  # BUN: marginal
  coefs10 = summary(mod_impute10)$coefficients
  Imp_data$BUN[ind10] = exp(rnorm(n = sum(ind10), 
                                  mean = predict(mod_impute10, newdata = m[ind10,],
                                                 allow.new.levels=T),
                                  sd = coefs10[1,'Std. Error']))
  
  SM_Impute_List[[k]] = Imp_data
  
}
save(SM_Impute_List, file = 'RData/Multiple_Imputed_Datasets.RData')

vars_explanatory = c('HCT','LPAR_pct','coma' ,'convulsions',
                     'poedema','BUN','BD' ,'shock','hypoglycaemia',
                     'drug_AS','studyID','country')
apply(Imp_data[,vars_explanatory], 2, function(x) sum(is.na(x)))
```

```
##           HCT      LPAR_pct          coma   convulsions       poedema 
##             0             0             0             0             0 
##           BUN            BD         shock hypoglycaemia       drug_AS 
##             0             0             0             0             0 
##       studyID       country 
##             0             0
```


# Logistic regression model

We fit the full model with adjustments as specified in the Methods section:

```r
if(RUN_MODELS){
  # the model formula
  DAG_fmla = "outcome ~ HCT + LPAR_pct + coma + convulsions + poedema + 
                        log2(BUN) + BD + shock + hypoglycaemia + 
                        drug_AS + (1 | studyID) + (1 | country)"
  # fit the model to each dataset
  modList= glmerModList(DAG_fmla, data = SM_Impute_List, family=binomial,parallel = T) 
  # save the output
  save(modList, file = 'RData/Models_List.RData')
} else {
  load('RData/Models_List.RData')
}
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00122495
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00104054
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00206283
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00132113
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00223533
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00104231
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00258299
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00174551
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00111835
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00435156
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00314414
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00104378
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00135104
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00157002
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00110502
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00116649
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00131938
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.0013207
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.0011478
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00169498
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00126448
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00131231
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.0018216
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00107625
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00108615
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00132836
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00247112
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00206565
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.0021987
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00129936
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00122412
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00104606
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00336323
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00281532
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.0011571
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00296874
## (tol = 0.001, component 1)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 0.00170862
## (tol = 0.001, component 1)
```

Compute the overall parameter estimates:

```r
# extract the fixed and random effects from all the model fits
# These functions then compute the overall estimates
FixedEffs = modelFixedEff(modList)
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

```r
RandEffs = modelRandEffStats(modList)
print(FixedEffs)
```

```
##             term   estimate   std.error   statistic           df
## 1    (Intercept) -6.4910463 0.279266437 -23.2432024     23346173
## 2             BD  0.1034804 0.006178008  16.7497967  35243698118
## 3           coma  1.2886589 0.080598339  15.9886533     18257365
## 4   convulsions1  0.3298298 0.094506778   3.4900121   2235311626
## 5        drug_AS -0.4558470 0.075646399  -6.0260241  28342018660
## 6            HCT  0.0135997 0.004381368   3.1039861 200679298533
## 7  hypoglycaemia  0.6381758 0.118392377   5.3903451    940463781
## 8      log2(BUN)  0.5493967 0.040871390  13.4420837    362864719
## 9       LPAR_pct  0.0319340 0.055252601   0.5779638    267974934
## 10      poedema1  0.7113365 0.312786441   2.2741922    342683036
## 11        shock1  0.4245032 0.143177193   2.9648798   4868671023
```

```r
print(RandEffs)
```

```
##                     term   group   estimate   std.error
## 1 sd_(Intercept).country country 0.51921586 0.002635667
## 2 sd_(Intercept).studyID studyID 0.04351246 0.015375415
```

Aggregate results for plotting:

```r
# The scalar multiples to put the AORs on the correct scales
Scalar_f = c(1, 10, 1, 1, 1, 10, 1, log2(3), 1, 1, 1)
# Compute 95% CIs
Results = data.frame(lowerCI = exp(Scalar_f*(FixedEffs$estimate -
                                               1.96*FixedEffs$std.error)),
                     mean = exp(Scalar_f*(FixedEffs$estimate)),
                     upperCI = exp(Scalar_f*(FixedEffs$estimate +
                                               1.96*FixedEffs$std.error)))
rownames(Results) = FixedEffs$term
```

Make the 'forest' plot:

```r
plotting_ind = rownames(Results) %in% c('BD','coma','convulsions1','drug_AS','HCT','log2(BUN)','poedema1')
Results = Results[plotting_ind,]
x_ind = sort.int(Results$mean, index.return = T)$ix
Results = Results[x_ind,]
par(bty='n', las=1, mar = c(3,9,2,2))

Y_Labels = c('+10 mEq/L\nbase deficit',
             'Coma\non admission',
             'Seizures\non admission',
             'Artemisinin drug\nversus\nnon Artemisinin drug',
             '-10 % points\nabsolute haematocrit\non admission',
             '3 fold increase\nin blood urea\nnitrogen (mmol/L)',
             'Pulmonary\nOedema\non admission')
Y_Labels = Y_Labels[x_ind]

xlims = c(0.5, 4.5)
plot(NA,NA, xlim= log2(xlims), ylim = c(0,1),xaxt='n',
     xlab='', ylab='', yaxt='n')
axis(1, at = log2(c(0.5,1, 2,4)), labels = c(0.5,1, 2,4))
abline(v=0, lty=2, lwd=3, col='red')
yindex =1
ypos = seq(0,1,length.out = sum(plotting_ind))

Results['HCT',] = 1/Results['HCT',]
for(i in 1:nrow(Results)){
  arrows(log2(Results[i,'lowerCI']),ypos[yindex],
         log2(Results[i,'upperCI']),ypos[yindex],
         length=0.05, angle=90, code=3, 
         col = 'black',lwd=3)
  points(log2(Results[i,'mean']),ypos[yindex],pch=18,cex=2)
  yindex=yindex+1
  
}
abline(h=ypos, lty=3)
axis(side = 2, at = ypos, labels = Y_Labels,tick=FALSE)
mtext(side=1, line = 2, text = 'Adjusted odds ratio')
mtext(side = 3, line = 1, text = 'Increased survival',adj = 0)
mtext(side = 3, line = 1, text = 'Decreased survival',adj = 1)
```

![](FactorsCausingDeath_files/figure-html/ForestPlot_SM-1.png)<!-- -->



