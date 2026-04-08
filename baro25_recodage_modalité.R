###############################################################################
#                                                                             #
#                             RECODAGE DES MODALITES                          #
#                                                                             #
###############################################################################

#Transformation des NA en NSPP----

baro <- baro %>%
  mutate(across(everything(), ~ {
    if (is.factor(.)) {
      fct_na_value_to_level(., level = "NSPP")
    } else {
      replace(., is.na(.), "NSPP")
    }
  }))

# QT2 : Reconnaissance----
baro$QT2_RECONNAISSANCE<-fct_collapse(factor(baro$QT2_RECONNAISSANCE),
                                      "Oui"=c("Oui","Plutôt oui"),
                                      "Non"=c("Non","Plutôt non"))

# QT3 : Amour pour la profession----
baro$QT3_AMOUR_PROFESSION<-fct_collapse(factor(baro$QT3_AMOUR_PROFESSION),
                                        "Oui"=c("Oui","Plutôt oui"),
                                        "Non"=c("Non","Plutôt non"))

# QT4 : Heureux dans la profession----
baro$QT4_HEUREUX<-fct_collapse(factor(baro$QT4_HEUREUX),
                               "Oui"=c("Oui","Plutôt oui"),
                               "Non"=c("Non","Plutôt non"))

#QT5 : Sens pour les missions----
baro$QT5_SENS<-fct_collapse(factor(baro$QT5_SENS),
                            "Oui"=c("Oui","Plutôt oui"),
                            "Non"=c("Non","Plutôt non"))

#QT7 : Satisfaction CT----
baro$QT7_CT_SATISTFAISANTES<-fct_collapse(factor(baro$QT7_CT_SATISTFAISANTES),
                                          "Oui"=c("Oui","Plutôt oui"),
                                          "Non"=c("Non","Plutôt non"))

#QT11 : Changement de métier (public)----
baro$QT11_CHANGE_METIER_PUB<-fct_collapse(factor(baro$QT11_CHANGE_METIER_PUB),
                                          "Oui"=c("Oui","Plutôt oui"),
                                          "Non"=c("Non","Plutôt non"))

#QT12 : Changement de métier (privé)----
baro$QT12_CHANGE_METIER_PRIV<-fct_collapse(factor(baro$QT12_CHANGE_METIER_PRIV),
                                           "Oui"=c("Oui","Plutôt oui"),
                                           "Non"=c("Non","Plutôt non"))

#QT14 : Choix politiques ----
baro$QT14_ACCORD_CHOIX_POLITIQUES<-fct_collapse(factor(baro$QT14_ACCORD_CHOIX_POLITIQUES),
                                                "Oui"=c("Oui","Plutôt oui"),
                                                "Non"=c("Non","Plutôt non"))

#QT15 : Conseil métier ----
baro$QT15_CONSEIL_METIER<-fct_collapse(factor(baro$QT15_CONSEIL_METIER),
                                       "Oui"=c("Oui","Plutôt oui"),
                                       "Non"=c("Non","Plutôt non"))

#QA7_ACTION_SYND_EFFICACE----
baro$QA7_ACTION_SYND_EFFICACE<-fct_collapse(factor(baro$QA7_ACTION_SYND_EFFICACE),
                                            "Oui"=c("Oui, plutôt","Oui, tout à fait"),
                                            "Non"=c("Non, pas vraiment","Non, pas du tout"),
                                            "Ce n'est pas son rôle"="Ce n'est pas son rôle")

#QA10_AVENIR_EDUC_VIVRE_ENS----
baro$QA10_AVENIR_EDUC_VIVRE_ENS<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_VIVRE_ENS),
                                              "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                              "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_LAICITE_PRINCIPES----
baro$QA10_AVENIR_EDUC_LAICITE_PRINCIPE<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_LAICITE_PRINCIPE),
                                                     "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                     "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_MISSION_PUB----
baro$QA10_AVENIR_EDUC_MISSION_PUB<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_MISSION_PUB),
                                                "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE----
baro$QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE),
                                                        "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                        "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_ARGENT_PUB----
baro$QA10_AVENIR_EDUC_ARGENT_PUB<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_ARGENT_PUB),
                                               "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                               "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_REUSSI_TOUS----
baro$QA10_AVENIR_EDUC_REUSSI_TOUS<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_REUSSI_TOUS),
                                                "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_REALITE_TERRITOIRE----
baro$QA10_AVENIR_EDUC_REALITE_TERRITOIRE<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_REALITE_TERRITOIRE),
                                                       "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                       "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))


#QA10_AVENIR_EDUC_INCLUSION----
baro$QA10_AVENIR_EDUC_INCLUSION<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_INCLUSION),
                                              "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                              "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))

#QA10_AVENIR_EDUC_CITOYENS_LIBRES----
baro$QA10_AVENIR_EDUC_CITOYENS_LIBRES<-fct_collapse(factor(baro$QA10_AVENIR_EDUC_CITOYENS_LIBRES),
                                                    "D'accord"=c("Tout à fait d'accord","Plutôt d'accord"),
                                                    "Pas d'accord"=c("Pas du tout d'accord","Plutôt pas d'accord"))


# Ordonner les modalités----

## Réordonne en mettant les oui, d'accord, non, pas d'accord devant et tous les "NSPP" en dernière position ----

baro <- baro %>%
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

baro$Sexe <- factor(baro$Sexe, 
                    levels = c("Féminin", "Masculin", "Je me définis autrement"))

## AGE----

baro$AGE <- factor(baro$AGE, 
                   levels = c("- de 35 ans", "de 35 à 45 ans", "de 46 à 55 ans", "+ de 55 ans"))

## ANCIENNETE----

baro$ANCIENNETE <- factor(baro$ANCIENNETE, 
                   levels = c("0 - 4 ans", "5 - 9 ans", "10 - 20 ans", "+ de 20 ans"))

## ZONE4----

baro$ZONE4 <- factor(baro$ZONE4, 
                     levels = c("Reste des départements", "Outre-mer", "Départements très ruraux", "Banlieue-Paris", "Paris"))

## SYNDICAT_SE_AGE----

baro$SYNDICAT_SE_AGE <- factor(baro$SYNDICAT_SE_AGE, 
                               levels = c("Moins de 35 ans", "35 - 45 ans", "46 - 55 ans", "Plus de 55 ans", "Autre syndicat"))


## QA3 : Augmentation de salaire----

baro$QA3_AGMENTATION_SALAIRE <- factor(baro$QA3_AGMENTATION_SALAIRE, 
                                       levels = c("5 %", "10 %", "20 %", "30 %","50 % ou plus", "NSPP"))


## ADHESION_SYNDICALE----

baro$ADHESION_SYNDICALE <- factor(baro$ADHESION_SYNDICALE, 
                                  levels = c("de l'UNSA", "d'une autre organisation syndicale", "d'aucune organisation syndicale"))



