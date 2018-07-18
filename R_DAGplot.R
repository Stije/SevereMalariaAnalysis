
library(dagitty)

SM = dagitty( "dag {
Concomitant_sepsis [latent]
  Included_in_study [adjusted]
  Pulmonary_Oedema 
  AKI
  Acidosis 
  Age 
  Anaemia 
  Coma 
  Convulsions 
  DEATH 
  Hyperlactataemia 
  Hypoglycaemia
  Immunity [latent]
  Parasitaemia 
  Shock 
  Transmission [latent]
  Concomitant_sepsis -> DEATH 
  Concomitant_sepsis -> Shock
  Pulmonary_Oedema -> Included_in_study
  Pulmonary_Oedema -> DEATH 
  AKI -> Included_in_study
  AKI -> Pulmonary_Oedema
  AKI -> Acidosis 
  AKI -> DEATH 
  Acidosis -> DEATH
  Age -> AKI
  Age -> Anaemia
  Age -> Convulsions
  Age -> Hypoglycaemia
  Age -> Immunity
  Age -> Parasitaemia
  Anaemia -> Included_in_study
  Anaemia -> DEATH
  Anaemia -> Hyperlactataemia
  Coma -> Included_in_study
  Coma -> DEATH 
  Convulsions -> Included_in_study
  Convulsions -> Coma
  Convulsions -> DEATH 
  Hyperlactataemia -> Included_in_study
  Hyperlactataemia -> Acidosis
  Hypoglycaemia -> Included_in_study
  Hypoglycaemia -> Coma
  Hypoglycaemia -> Convulsions
  Immunity -> Parasitaemia
  Parasitaemia -> Concomitant_sepsis
  Parasitaemia -> Included_in_study
  Parasitaemia -> Pulmonary_Oedema
  Parasitaemia -> AKI
  Parasitaemia -> Anaemia
  Parasitaemia -> Coma 
  Parasitaemia -> Convulsions
  Parasitaemia -> DEATH 
  Parasitaemia -> Hyperlactataemia
  Parasitaemia -> Hypoglycaemia
  Parasitaemia -> Shock
  Shock -> Included_in_study
  Shock -> DEATH 
  Shock -> Hyperlactataemia
  Transmission -> Immunity
}")
set.seed(76578)
plot(graphLayout(SM))
