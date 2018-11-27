---
title: "Selection bias in severe malaria studies"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
---




```r
require(RColorBrewer)
```

```
## Loading required package: RColorBrewer
```

# Outline

This code goes through some very simple simulations that demonstrate selection bias in a severe malaria type study.

The idea is to see whether the type of results reported in Clark et al (2017) whereby they see less coma in G6PD deficients and more anaemia in G6PD deficients can be reproduced with an extremely simple simulation model that accounts for selection bias inherent in a severe malaria study.

Reminder on the methodology in the Clark study: They have about 11000 patients from severe malaria studies (already selection bias here). They then only look at differences in G6PD allele frequencies in patients who either have coma, anaemia, or both. This discards about half the data. We're going to do the same.
Outcomes don't appear to be analysed in the Clark study (unless I'm mistaken), e.g. probability of death given covariates is not calculated nor is an analysis endpoint.

# Base model

Let us suppose there are three variables: G6PD status (normal or deficient); Anaemia (yes/no); Coma (yes/no).
let's assume that G6PD deficiency increases the probability of anaemia. 
In the most simple model, let's assume that coma and G6PD status are independent.
Let's also assume that we are only dealing with males, so no complications in the model due to heterozygote women.

Therefore to summarise: the only assumption is that G6PD deficiency increases your risk of anaemia. We assume that G6PD status does not change your risk of coma.


```r
# The `free' parameter:
# The Probability of being anaemic if you are G6PD deficient
P_anaemia_def = 0.3 

# These are taken from Clark
P_anaemia = 0.24 # the overall (marginal) probability of being anaemic in Clark data
P_G6PDdef = 0.15 # the probability of being deficient
P_coma = 0.34 # the overall (marginal) probability of having coma in Clark data

# We solve the equation to work out the Probability of being anaemic if you are G6PD normal
# This is dependent on the previous probabilities (simple algebra)
P_anaemia_norm = (P_anaemia - P_G6PDdef*P_anaemia_def)/(1-P_G6PDdef)
writeLines(paste('Probability of anaemia if G6PD normal:',round(P_anaemia_norm,2)))
```

```
## Probability of anaemia if G6PD normal: 0.23
```

```r
writeLines(paste('Probability of anaemia if G6PD deficient:',P_anaemia_def))
```

```
## Probability of anaemia if G6PD deficient: 0.3
```

`P_anaemia` is just the weighted average of these two quantities.

# Simulate some patients

Making N large: don't need to calculate p-values as any difference for very large numbers is a significant difference...


```r
# The number of malaria patients
N = 10^4
# We assume that G6PD status and Coma status are independent
G6PDstatus = sample(c('Normal','Def'), size = N, replace = T, prob = c(1-P_G6PDdef, P_G6PDdef))
Comastatus = sample(c('No Coma','Coma'), size = N, replace = T, prob = c(1-P_coma, P_coma))
# Generate anaemia status dependent on G6PD status
Anaemiastatus = unlist(sapply(G6PDstatus, function(x){
  if(x=='Normal') {
    y=sample(x = c('No Anaemia','Anaemia'), size = 1, 
             replace = T, prob = c(1-P_anaemia_norm,P_anaemia_norm))
  } 
  if(x=='Def') {
    y=sample(x = c('No Anaemia','Anaemia'), size = 1, 
             replace = T, prob = c(1-P_anaemia_def,P_anaemia_def))
  }
  return(y)}
))
```

As done in the Clark study, we look at the patients who either have anaemia or coma or both: we only select these patients:

```r
study_patients = which(Comastatus=='Coma' | Anaemiastatus == 'Anaemia')
Study_dat = data.frame(Coma = Comastatus[study_patients],
                       G6PD = G6PDstatus[study_patients],
                       Anaemia = Anaemiastatus[study_patients])
Nstudy = nrow(Study_dat)
writeLines(paste('This selects:', round(100*Nstudy/N),'% of individuals'))
```

```
## This selects: 50 % of individuals
```

In the Clark study they select 52% of individuals (3359: coma only; 2184: anaemia only; 714: both; 11871 total number of patients).

## Overall G6PD deficiency
Prevalence of G6PD deficiency in this group:

```
## Overall we see (%):
```

```
## 
##    Def Normal 
##     15     85
```

There is a slightly increased number of G6PD deficients seen because they are being selected for by anaemia.

Now we dig into the different groups and see whether the probabilities correspond to the true data generating probabilities:

## Coma between normals and deficients

First for Coma (in this model coma and G6PD are independent so there should be no differences between the groups!):

```
## 
## In the G6PD normal group we see (%):
```

```
## 
##    Coma No Coma 
##      70      30
```

```
## 
## In the G6PD deficient group we see (%):
```

```
## 
##    Coma No Coma 
##      64      36
```

Odds ratio for Coma:

```r
O1 = sum(Study_dat$Coma=='Coma' & Study_dat$G6PD=='Normal')/sum(Study_dat$Coma=='No Coma' & Study_dat$G6PD=='Normal')
O2 = sum(Study_dat$Coma=='Coma' & Study_dat$G6PD=='Def')/sum(Study_dat$Coma=='No Coma' & Study_dat$G6PD=='Def')
O2/O1
```

```
## [1] 0.7721956
```

## Anaemia in the normals and deficients

Anaemia in the G6PD normal group

```
## 
## In the G6PD normal group we see (%):
```

```
## 
##    Anaemia No Anaemia 
##         46         54
```

```
## 
## The true proportions are (%): 23 and 77
```

Anaemia in the G6PD deficient group


```
## 
## In the G6PD deficient group we see (%):
```

```
## 
##    Anaemia No Anaemia 
##         56         44
```

```
## 
## The true proportions are (%): 30 and 70
```

Odds ratio for Anaemia:

```r
O1 = sum(Study_dat$Anaemia=='Anaemia' & Study_dat$G6PD=='Normal')/sum(Study_dat$Anaemia=='No Anaemia' & Study_dat$G6PD=='Normal')
O2 = sum(Study_dat$Anaemia=='Anaemia' & Study_dat$G6PD=='Def')/sum(Study_dat$Anaemia=='No Anaemia' & Study_dat$G6PD=='Def')
O2/O1
```

```
## [1] 1.509612
```

# Conclusion

Due to the selection bias, you're seeing higher rates of anaemia in the G6PD deficient group (which is expected and corresponds to the model), and you're seeing lower rates of coma.

Therefore you can get this `balancing selection' illusion just from a simple selection bias along with an effect of G6PDd on anaemia.

# Graphical visualisation of bias


We run the model for values of pi from P_anaemia up to 0.35 (50\% increase). This is converted into odds ratio for anaemia in G6PDd (x-axis).


```r
# The number of malaria patients
N = 10^5
PIs = seq(P_anaemia, 0.35, length.out = 40)
ORcoma = ORanaemia = array(dim=length(PIs))
TrueOR_anaemia = array(dim=length(PIs))
for(i in 1:length(PIs)){
  P_anaemia_def = PIs[i] 
  P_anaemia_norm = (P_anaemia - P_G6PDdef*P_anaemia_def)/(1-P_G6PDdef)
  
  # We assume that G6PD status and Coma status are independent
  G6PDstatus = sample(c('Normal','Def'), size = N, replace = T, 
                      prob = c(1-P_G6PDdef, P_G6PDdef))
  Comastatus = sample(c('No Coma','Coma'), size = N, replace = T, 
                      prob = c(1-P_coma, P_coma))
  # Generate anaemia status dependent on G6PD status
  Anaemiastatus = unlist(sapply(G6PDstatus, function(x){
    if(x=='Normal') {
      y=sample(x = c('No Anaemia','Anaemia'), size = 1, 
               replace = T, prob = c(1-P_anaemia_norm,P_anaemia_norm))
    } 
    if(x=='Def') {
      y=sample(x = c('No Anaemia','Anaemia'), size = 1, 
               replace = T, prob = c(1-P_anaemia_def,P_anaemia_def))
    }
    return(y)}
  ))
  study_patients = which(Comastatus=='Coma' | Anaemiastatus == 'Anaemia')
  Study_dat = data.frame(Coma = Comastatus[study_patients],
                         G6PD = G6PDstatus[study_patients],
                         Anaemia = Anaemiastatus[study_patients])
  Study_dat$Coma = as.factor(Study_dat$Coma)
  Study_dat$G6PD = as.factor(Study_dat$G6PD)
  Study_dat$Anaemia = as.factor(Study_dat$Anaemia)
  mod = glm(Coma ~ G6PD, data = Study_dat, family = 'binomial')
  summary(mod)
  # odds ratio for coma
  O1 = sum(Study_dat$Coma=='Coma' & Study_dat$G6PD=='Normal')/sum(Study_dat$Coma=='No Coma' & Study_dat$G6PD=='Normal')
  O2 = sum(Study_dat$Coma=='Coma' & Study_dat$G6PD=='Def')/sum(Study_dat$Coma=='No Coma' & Study_dat$G6PD=='Def')
  ORcoma[i] = O2/O1
  
  # odds ratio for anaemia
  O1 = sum(Study_dat$Anaemia=='Anaemia' & Study_dat$G6PD=='Normal')/sum(Study_dat$Anaemia=='No Anaemia' & Study_dat$G6PD=='Normal')
  O2 = sum(Study_dat$Anaemia=='Anaemia' & Study_dat$G6PD=='Def')/sum(Study_dat$Anaemia=='No Anaemia' & Study_dat$G6PD=='Def')
  O2/O1
  ORanaemia[i] = O2/O1
  
  TrueOR_anaemia[i] = (P_anaemia_def/(1-P_anaemia_def))/(P_anaemia_norm/(1-P_anaemia_norm))
}
```


The following plot shows how varying the odds ratio for anaemia changes the observed odds ratio for coma:

![](SelectionBiasSimulation_files/figure-html/ModelSimulation-1.png)<!-- -->


