###############################################################################
#                                                                             #
#                             RECODAGE DES MODALITES                          #
#                                                                             #
###############################################################################

# Import des scripts
source("baro2025_import_rename_var.R")

# Conversion de toutes les variables character en factor
baro2025 <- baro2025 %>%
  mutate(across(where(is.character), as.factor))


# QT2 : Reconnaissance----
baro2025$QT2_Ressentir_Reconnaissance_et_respect<-fct_collapse(factor(baro2025$QT2_Ressentir_Reconnaissance_et_respect),
                                      "Oui"=c("Oui","Plutôt oui"),
                                      "Non"=c("Non","Plutôt non"))

# QT3 : Amour pour la profession----
baro2025$QT3_Aimer_profession<-fct_collapse(factor(baro2025$QT3_Aimer_profession),
                                        "Oui"=c("Oui","Plutôt oui"),
                                        "Non"=c("Non","Plutôt non"))

# QT4 : Heureux dans la profession----
baro2025$QT4_Heureux_dans_profession<-fct_collapse(factor(baro2025$QT4_Heureux_dans_profession),
                               "Oui"=c("Oui","Plutôt oui"),
                               "Non"=c("Non","Plutôt non"))

#QT5 : Sens pour les missions----
baro2025$QT5_Les_missions_confiées_ont_du_sens<-fct_collapse(factor(baro2025$QT5_Les_missions_confiées_ont_du_sens),
                            "Oui"=c("Oui","Plutôt oui"),
                            "Non"=c("Non","Plutôt non"))

#QT7 : Satisfaction CT----
baro2025$QT7_Cond_travail_satisfaisantes<-fct_collapse(factor(baro2025$QT7_Cond_travail_satisfaisantes),
                                          "Oui"=c("Oui","Plutôt oui"),
                                          "Non"=c("Non","Plutôt non"))

#QT11 : Changement de métier (public)----
baro2025$QT11_Changement_métier_dans_le_public<-fct_collapse(factor(baro2025$QT11_Changement_métier_dans_le_public),
                                          "Oui"=c("Oui","Plutôt oui"),
                                          "Non"=c("Non","Plutôt non"))

#QT12 : Changement de métier (privé)----
baro2025$QT12_Changement_métier_dans_le_privé<-fct_collapse(factor(baro2025$QT12_Changement_métier_dans_le_privé),
                                           "Oui"=c("Oui","Plutôt oui"),
                                           "Non"=c("Non","Plutôt non"))

#QT14 : Choix politiques ----
baro2025$QT14_Accord_avec_les_choix_politiques<-fct_collapse(factor(baro2025$QT14_Accord_avec_les_choix_politiques),
                                                "Oui"=c("Oui","Plutôt oui"),
                                                "Non"=c("Non","Plutôt non"))

#QT15 : Conseil métier ----
baro2025$QT15_Conseiller_métier_aux_jeunes<-fct_collapse(factor(baro2025$QT15_Conseiller_métier_aux_jeunes),
                                       "Oui"=c("Oui","Plutôt oui"),
                                       "Non"=c("Non","Plutôt non"))

#QA7_ACTION_SYND_EFFICACE----
baro2025$QA7_ACTION_SYND_EFFICACE<-fct_collapse(factor(baro2025$QA7_ACTION_SYND_EFFICACE),
                                            "Oui"=c("Oui, plutôt","Oui, tout à fait"),
                                            "Non"=c("Non, pas vraiment","Non, pas du tout"),
                                            "Ce n'est pas son rôle"="Ce n'est pas son rôle")

#QA10_AVENIR_EDUC_VIVRE_ENS----
baro2025$QA10_AVENIR_EDUC_VIVRE_ENS<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_VIVRE_ENS),
                                              "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                              "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_LAICITE_PRINCIPES----
baro2025$QA10_AVENIR_EDUC_LAICITE_PRINCIPE<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_LAICITE_PRINCIPE),
                                                     "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                     "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_MISSION_PUB----
baro2025$QA10_AVENIR_EDUC_MISSION_PUB<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_MISSION_PUB),
                                                "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE----
baro2025$QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE),
                                                        "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                        "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_ARGENT_PUB----
baro2025$QA10_AVENIR_EDUC_ARGENT_PUB<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_ARGENT_PUB),
                                               "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                               "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_REUSSI_TOUS----
baro2025$QA10_AVENIR_EDUC_REUSSI_TOUS<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_REUSSI_TOUS),
                                                "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_REALITE_TERRITOIRE----
baro2025$QA10_AVENIR_EDUC_REALITE_TERRITOIRE<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_REALITE_TERRITOIRE),
                                                       "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                       "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))


#QA10_AVENIR_EDUC_INCLUSION----
baro2025$QA10_AVENIR_EDUC_INCLUSION<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_INCLUSION),
                                              "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                              "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_CITOYENS_LIBRES----
baro2025$QA10_AVENIR_EDUC_CITOYENS_LIBRES<-fct_collapse(factor(baro2025$QA10_AVENIR_EDUC_CITOYENS_LIBRES),
                                                    "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                    "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))


# Ordonner les modalités----

## Réordonne en mettant les oui, d'accord, non, pas d'accord devant et tous les "NSPP" en dernière position ----

baro2025 <- baro2025 %>%
  mutate(
    across(
      where(is.factor),
      ~ fct_relevel(., 
                    "Oui", "D'accord", 
                    "Non", "Pas d'accord",
                    after = 0 # 0 pour indiquer début
      ) |> 
        fct_relevel("Autres métiers", "Autre métier", "Autre pers.", "Pas en éducation prioritaire", "NSPP", after = Inf) # Inf pour indiquer la fin
    )
  )


## Sexe----

baro2025$SEXE <- factor(baro2025$SEXE, 
                    levels = c("Féminin", "Masculin", "Je me définis autrement"))

## AGE----

baro2025$AGE <- factor(baro2025$AGE, 
                   levels = c("- de 35 ans", "de 35 à 45 ans", "de 46 à 55 ans", "+ de 55 ans"))

## ANCIENNETE----

baro2025$ANCIENNETE <- factor(baro2025$ANCIENNETE, 
                   levels = c("0 - 4 ans", "5 - 9 ans", "10 - 20 ans", "+ de 20 ans"))


## QA3 : Augmentation de salaire----

baro2025$QA3_AGMENTATION_SALAIRE <- factor(baro2025$QA3_AGMENTATION_SALAIRE, 
                                       levels = c("5 %", "10 %", "20 %", "30 %","50 % ou plus", "NSPP"))


## ADHESION_SYNDICALE----

baro2025$ADHESION_SYNDICALE <- factor(baro2025$ADHESION_SYNDICALE, 
                                  levels = c("de l'UNSA", "d'une autre organisation syndicale", "d'aucune organisation syndicale"))

# Niveau des modalités-----

ordre_moda_baro2025 <- setNames(
  lapply(names(baro2025), function(v) levels(baro2025[[v]])),
  names(baro2025)
)

