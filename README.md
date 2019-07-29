---
title: "Causal inference and severe malaria"
author: James Watson and Stije Leopold
---

## Overview

This repository provides code underlying the results of two separate publications:

1. Selection bias when estimating the causal effect of G6PD deficiency on cerebral malaria (being recruited into a study with cerebral malaria). The paper is: Watson, Leopold et al. [Collider bias and the apparent protective effect of glucose-6-phosphate dehydrogenase deficiency on cerebral malaria](https://elifesciences.org/articles/43154) eLife (2019).

2. Estimating the contribution of key clinical variables on death in severe malaria. The paper is Leopold, Watson et al. *Investigating causal pathways in severe falciparum malaria: a pooled retrospective analysis of clinical studies* In Press, PLoS Medicine (2019).

## Collider bias: G6PDd and coma

A very simple simulation model is given in `SelectionBiasSimulation.md` and corresponding RMarkdown. This shows that the expected effect between G6PDd and anaemia in malaria is large enough to explain all the estimated effect between coma and G6PDd observed in two previous publications: [Nature Genetics 2015](https://www.nature.com/articles/ng.3107) and [eLife 2017](https://elifesciences.org/articles/15085).

## Estimating causal effects in severe malaria.

Here we report data on over 9000 patients with severe falciparum malaria, recruited over 35 years in multiple studies across Asia and Africa.
Our main finding is that moderate anaemia has a protective effect (or at least is a risk factor associated with a lower risk of death). Overall we get the following effect sizes (note that the scale chosen for the continuous variables is equal to one standard deviation in the population):
![fig1](FactorsCausingDeath_files/figure-html/Results_Comparison-1.png)

The RMarkdown notebooks are arranged as follows:

* `FactorsCausingDeath` runs the main analysis looking at the contribution of the main factors in severe malaria to survival

* `Exploratory_Analysis_and_Sensitivity.md` runs some basic data exploration and a sensitivity analysis to the main analysis

* `InverseProbabilityWeighting.md` shows an extra set on analyses where we look at the effect of transfusion on death by doing inverse probability weighting.


For any questions please contact James jwatowatson@gmail.com or Stije stijeleopold@gmail.com

