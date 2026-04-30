# Chargement des packages----

library(tidyverse) # Chargement tidyverse
library(readxl)
library(questionr)
library(readxl) #pour pouvoir par exemple importer les tables excel
library(foreign) #pour pouvoir par exemple importer les tables dbf
library(haven) #pour pouvoir par exemple importer les tables SAS, SPSS, STATA
library(RColorBrewer) 
library(ggrepel) 
library(explor)
library(ggplot2)
library(scales)  # pour percent_format()
library(factoextra)
library(dplyr)
library(gtsummary)
library(survey)
library(gt)
library(tibble)
library(stringr)
library(janitor) # pour le nettoyage

# Importation de la base de données----

## Identification de l'encodage adéquat----

library(readr)

guess_encoding("baro2025.csv")

#baro2025 <- read.csv(paste0("annual/", 2025, ".csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = c("N/A", "Vide", "<NA>"))

## Importation proprement dite de toutes les bases annuelles  + rename ----

for (i in setdiff(2013:2025, 2020)) {  # setdiff : pour exclure 2020 de la plage 2013:2025
  
  #Importation
  data <- read.csv(paste0("annual/", i, ".csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = c("N/A","", "Vide")
  )
  
  # Rename des colonnes
  data <- data %>% 
    mutate(ANNEE=i) %>% 
    rename(
      SEXE = any_of("Sexe"),
      AGE = any_of("Votre.âge"),
      CATEGORIE = any_of("Votre.catégorie"),
      METIER = any_of("Votre.métier"),
      MINISTERE = any_of("Votre.champ.ministériel.ou.votre.secteur.d.activité.dans.le.privé"),
      CODE_DEP = any_of(c("Votre.département..code.","code_dep")),
      EXERCE_EDUCATION_PRIORITAIRE = any_of("J.exerce.en.éducation.prioritaire..REP..REP......"),
      ANCIENNETE = any_of("Votre.ancienneté.dans.le.métier"),
      ADHESION_SYNDICALE = any_of("Vous.êtes.adhérent.e.ou.sympathisant.e"),
      LIEU_EXERCICE = any_of("Votre.lieu.d.exercice"),
      QT1_TRAVAIL_SALAIRE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Salaire."),
      QT1_TRAVAIL_EPANOUI = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Épanouissement."),
      QT1_TRAVAIL_NECESSITE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Nécessité."),
      QT1_TRAVAIL_STABLILITE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Stabilité."),
      QT1_TRAVAIL_SOCIAL = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Lien.social."),
      QT1_TRAVAIL_EQUIPE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Équipe."),
      QT1_TRAVAIL_EQUI_VIE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Équilibre.vie.pro...vie.perso."),
      QT1_TRAVAIL_UTILITE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Utilité."),
      QT1_TRAVAIL_PARTAGE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Partage."),
      QT1_TRAVAIL_CARRIERE = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Carrière."),
      QT1_TRAVAIL_SENS = any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Sens."),
      QT1_TRAVAIL_NSPP=any_of("Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Ne.se.prononce.pas."),
      QT2_Ressentir_Reconnaissance_et_respect = any_of(c("Q..tradi....2..Globalement..ressentez.vous.de.la.reconnaissance.et.du.respect.dans.votre.pratique.professionnelle..","X1..Globalement..ressentez.vous.de.la.reconnaissance.et.du.respect.dans.votre.pratique.professionnelle..")),
      QT3_Aimer_profession = any_of(c("Q..tradi....3..Aimez.vous.la.profession.que.vous.exercez..","X2..Aimez.vous.la.profession.que.vous.exercez..")),
      QT4_Heureux_dans_profession = any_of(c("Q..tradi....4..Êtes.vous.heureux.se.d.exercer.votre.profession..","X3..Êtes.vous.heureux.se.d.exercer.votre.profession..")),
      QT5_Les_missions_confiées_ont_du_sens = any_of(c("Q..tradi....5..Les.missions.qui.vous.sont.confiées.ont.elles.du.sens.pour.vous..","X4..Les.missions.qui.vous.sont.confiées.ont.elles.du.sens.pour.vous..")),
      QT6_Degré_satisfaction_du_choix_métier = any_of("Q..tradi....6..Parmi.les.affirmations.suivantes..laquelle.vous.correspond.le.mieux.."),
      QT7_Cond_travail_satisfaisantes = any_of(c("Q..tradi....7..Diriez.vous.que.vos.conditions.de.travail.sont.satisfaisantes..","X6..Diriez.vous.que.vos.conditions.de.travail.sont.satisfaisantes..")),
      QT8_Exposition_Charge_travail_excessive_et_délais_courts = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Une.charge.de.travail.excessive..des.délais.trop.courts."),
      QT8_Exposition_stress = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Des.situations.de.stress.ou.émotionnellement.difficiles.à.gérer."),
      QT8_Exposition_manque_autonomie = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.manque.d.autonomie.ou.l.impossibilité.de.participer.aux.décisions."),
      QT8_Exposition_manque_de_reconnaissance = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.manque.de.reconnaissance.de.votre.engagement."),
      QT8_Exposition_consignes_inadaptées_ou_inefficaces = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Des.consignes.inadaptées.ou.inefficaces.qu.il.faut.pourtant.appliquer."),
      QT8_Exposition_sentiment_de_craintes_sur_avenir_de_la_profession = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.sentiment.de.crainte.concernant.l.avenir.de.votre.profession."),
      QT8_Exposition_outils_inadaptés_ou_inefficaces = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Faire.face.à.des.outils.de.travail.inadaptés.ou.inefficaces."),
      QT8_Exposition_cadre_travail_non_sûr = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.cadre.de.travail.non.sûr."),
      QT8_NSPP = any_of("Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Ne.se.prononce.pas."),
      QT9_Domaine_prioritaire_dAmélioration_Relations_hérarchiques = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Relations.hiérarchiques.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Relations.hiérarchiques.")),
      QT9_Domaine_prioritaire_dAmélioration_Amenagement_lieu_de_travail = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Aménagement.du.lieu.de.travail.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Aménagement.du.lieu.de.travail.")),
      QT9_Domaine_prioritaire_dAmélioration_Appui_exercice_professionnel = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Appui.à.l.exercice.professionnel.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Appui.à.l.exercice.professionnel.")),
      QT9_Domaine_prioritaire_dAmélioration_Charge_de_travail = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Charge.de.travail.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Charge.de.travail.")),
      QT9_Domaine_prioritaire_dAmélioration_Formation_pro_continue = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Formation.professionnelle.continue.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Formation.professionnelle.continue.")),
      QT9_Domaine_prioritaire_dAmélioration_Perspectives_de_carrière = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Perspectives.de.carrière.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Perspectives.de.carrière.")),
      QT9_Domaine_prioritaire_dAmélioration_Pouvoir_dAchat = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Pouvoir.d.achat.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Pouvoir.d.achat.")),
      QT9_Domaine_prioritaire_dAmélioration_Possibilité_parcours_pro_varié = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Possibilité.d.un.parcours.professionnel.varié.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Possibilité.d.un.parcours.professionnel.varié.")),
      QT9_Domaine_prioritaire_dAmélioration_SAanté_au_travail = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Santé.au.travail.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Santé.au.travail.")),
      QT9_Domaine_prioritaire_dAmélioration_Temps_de_travail = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.travail.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.travail.")),
      QT9_Domaine_prioritaire_dAmélioration_Temps_de_concertation_équipe = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.concertation..équipe..","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.concertation..équipe..")),
      QT9_NSPP = any_of(c("Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Ne.se.prononce.pas.","X9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Ne.se.prononce.pas.")),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Sécurité_et_climat = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....La.sécurité..le.climat.sur.mon.lieu.d.exercice."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Equilibre_vie_pro_et_perso = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.équilibre.entre.vie.professionnelle.et.vie.personnelle."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Aménagement_lieu_de_travail = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.aménagement.de.mon.lieu.de.travail."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Aménagement_poste_de_travail = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.aménagement.de.mon.poste.de.travail."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Outils_de_travail = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Les.outils.avec.lesquels.je.travaille."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Renforcement_compétences = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Être.formé.e..renforcer.ou.développer.mes.compétences."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Management = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Le.pilotage..le.management..les.relations.hiérarchiques."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Moyens_supplémentaires = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Avoir.des.moyens.supplémentaires."),
      QT10_Domaine_prioritaire_dAmélioration_du_quotidien_Sentiment_dIsolement = any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Le.sentiment.d.isolement."),
      QT10_NSPP=any_of("Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Ne.se.prononce.pas."),
      QT11_Changement_métier_dans_le_public = any_of(c("Q..tradi....11..Dans.le.secteur.public","Souhaiteriez.vous.changer.de.métier.dans.les.prochaines.années....10..Dans.le.secteur.public.")),
      QT12_Changement_métier_dans_le_privé = any_of(c("Q..tradi....12..Dans.le.secteur.privé","Souhaiteriez.vous.changer.de.métier.dans.les.prochaines.années....11..Dans.le.secteur.privé.")),
      QT13_BENEFICIER_FORMATION=any_of("Q..tradi....13..Durant.ces.3.dernières.années..avez.vous.pu.bénéficier.d.une.formation.initiale.ou.continue.qui.réponde.à.vos.attentes.."),
      QT14_Accord_avec_les_choix_politiques = any_of(c("Q..tradi....14..Diriez.vous.que.vous.êtes.en.accord.avec.les.choix.politiques.faits.dans.votre.secteur.d.activité..","X14..Diriez.vous.que.vous.êtes.en.accord.avec.les.choix.politiques.faits.dans.votre.secteur.d.activité..")),
      QT15_Conseiller_métier_aux_jeunes = any_of(c("Q..tradi....15..Conseilleriez.vous.votre.métier.à.un.e.jeune.de.votre.entourage..","X15..Conseilleriez.vous.votre.métier.à.un.e.jeune.de.votre.entourage..")),
      QA1_MOTIV_SENS_UTILITE=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.sens.et.l.utilité.de.ce.que.je.fais."),
      QA1_MOTIV_LIEN_SOCIAL=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.lien.social.avec.les.jeunes.ou.les.publics.accompagnés."),
      QA1_MOTIV_INTELLECT=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....L.intérêt.intellectuel.ou.la.diversité.des.missions."),
      QA1_MOTIV_LIBERTE_AUTONOMIE=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....La.liberté.ou.l.autonomie.que.permet.ma.pratique."),
      QA1_MOTIV_EQUIPE_ECHANGES=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.travail.en.équipe.et.les.échanges.professionnels."),
      QA1_MOTIV_STABILITE_EMPLOI=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....La.stabilité.de.l.emploi."),
      QA1_MOTIV_EQUILIBRE_VIE=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....L.équilibre.vie.personnelle...vie.professionnelle."),
      QA1_MOTIV_NSPP=any_of("Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Ne.se.prononce.pas."),
      QA2_ATTRACTIVITE_SALAIRES_BAS=any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Des.salaires.trop.bas.par.rapport.aux.responsabilités."),
      QA2_ATTRACTIVITE_IMAGE_DEGRADEE=any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Une.image.dégradée.dans.l.opinion."),
      QA2_ATTRACTIVITE_IMPACT_REFORMES=any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Trop.de.réformes.qui.impactent.notre.secteur."),
      QA2_ATTRACTIVITE_CT =any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Le.manque.de.moyens.et.les.conditions.de.travail."),
      QA2_ATTRACTIVITE_PAS_DAVIS=any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Je.ne.partage.pas.ce.constat."),
      QA2_ATTRACTIVITE_NSPP=any_of("Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Ne.se.prononce.pas."),
      QA3_AGMENTATION_SALAIRE=any_of("Q..actu....3..Selon.vous..quel.niveau.d.augmentation.de.salaire.serait.juste.aujourd.hui.."),
      QA4_MESURES_FRAIS_PRO=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Mieux.prendre.en.charge.les.frais.professionnels..déplacements..repas.."),
      QA4_MESURES_AIDE_LOGEMENT_INSTALL=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Bénéficier.d.aides.au.logement.et.à.l.installation."),
      QA4_MESURES_PRIMES=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Ajouter.ou.revaloriser.les.primes.qui.reconnaissent.l.investissement.de.chacun."),
      QA4_MESURES_PROTECTION_SOCIALES=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Améliorer.la.protection.sociale..mutuelle..prévoyance.."),
      QA4_MESURES_AVANTAGES_COLLECTIFS=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Offrir.plus.d.avantages.collectifs.type.comité.d.entreprise."),
      QA4_MESURES_NSPP=any_of("Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Ne.se.prononce.pas."),
      QA5_CHARGE_LOURDE=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Trop.lourde..pas.assez.reconnue."),
      QA5_CHARGE_MAL_REPARTIE=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Mal.répartie..on.manque.de.personnels."),
      QA5_CHARGE_MAL_ORGANISEE=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Mal.organisée..je.perds.du.temps."),
      QA5_CHARGE_ALLEGEABLE=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Allégeable.avec.de.meilleurs.outils..numérique..IA....."),
      QA5_CHARGE_EQUILIBREE=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Je.la.trouve.plutôt.équilibrée..elle.me.convient."),
      QA5_CHARGE_NSPP=any_of("Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Ne.se.prononce.pas."),
      QA6_CARRIERE_POINT_PARCOURS=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Pouvoir.faire.le.point.sur.mon.parcours..savoir.où.j.en.suis..et.ce.que.je.pourrais.faire.ensuite."),
      QA6_CARRIERE_PARENTHESE=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Avoir.droit.à.une.parenthèse.pour.vivre.d.autres.expériences."),
      QA6_CARRIERE_FORMATION=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Me.former.pour.acquérir.de.nouvelles.compétences.ou.certifications."),
      QA6_CARRIERE_EVOLUTION_POSTES_RESP=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Évoluer.vers.des.postes.à.responsabilités.accrues.au.sein.de.mon.domaine.actuel."),
      QA6_CARRIERE_CHANGER_METIER=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Pouvoir.changer.de.métier.ou.évoluer.vers.un.autre.domaine."),
      QA6_CARRIERE_INNOVER=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Avoir.les.moyens.d.innover.ou.porter.des.projets.qui.soient.reconnus."),
      QA6_CARRIERE_NSPP=any_of("Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Ne.se.prononce.pas."),
      QA7_ACTION_SYND_EFFICACE=any_of("Q..actu....7..Pensez.vous.que.l.action.syndicale.est.efficace.sur.ces.enjeux.d.attractivité.de.nos.métiers.."),
      
      QA8_ATTENTES_SYND_NEGOCIER_ACCORDS=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Négocier.des.accords.concrets."),
      QA8_ATTENTES_SYND_MOB_MASSIVES=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Organiser.des.mobilisations.massives."),
      QA8_ATTENTES_SYND_CAMPAGNES_LIGNE_MEDIAS=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Mener.des.campagnes.en.ligne.ou.dans.les.médias."),
      QA8_ATTENTES_SYND_UNION=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....S.unir.pour.peser.plus.fort."),
      QA8_ATTENTES_SYND_FAIRE_PRESSION=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Faire.pression.sur.les.décideurs.politiques."),
      QA8_ATTENTES_SYND_NSPP=any_of("Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Ne.se.prononce.pas."),
      QA9_FinAssu_GRATUIT_LAIQUE=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Offrir.une.École.gratuite..laïque.et.ouverte.à.tous."),
      QA9_FinAssu_APPRENTI_FOND=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Assurer.les.apprentissages.fondamentaux.pour.toutes.et.tous."),
      QA9_FinAssu_RED_INEGALITES=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Réduire.les.inégalités.entre.les.élèves.et.les.territoires."),
      QA9_FinAssu_AIDE_JEUNES=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Aider.les.jeunes.à.comprendre.le.monde.et.à.y.prendre.part."),
      QA9_FinAssu_COMPETENCES_UTILES=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Préparer.aux.compétences.utiles.pour.l.insertion.professionnelle."),
      QA9_FinAssu_EPANOUI_PERSO_CULTURE=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Encourager.l.épanouissement.personnel.et.culturel."),
      QA9_FinAssu_FORMATION_ELITE=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Former.une.élite.nationale."),
      QA9_FinAssu_VIVRE_ENS=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Garantir.les.bases.du.vivre.ensemble."),
      QA9_FinAssu_NSPP=any_of("Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Ne.se.prononce.pas."),
      QA10_AVENIR_EDUC_VIVRE_ENS=any_of("Q..actu....Si.l.École.doit.transmettre.les.savoirs.fondamentaux..elle.doit.tout.autant.garantir.l.apprentissage.du.vivre.ensemble."),
      QA10_AVENIR_EDUC_LAICITE_PRINCIPE=any_of("Q..actu....La.laïcité.est.un.principe.de.liberté.qui.protège.dans.le.respect.de.la.loi."),
      QA10_AVENIR_EDUC_MISSION_PUB=any_of("Q..actu....L.éducation.doit.rester.une.mission.publique.préservée.de.toute.logique.de.marchandisation."),
      QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE=any_of("Q..actu....Le.numérique.et.l.intelligence.artificielle.doivent.être.utilisés.avec.discernement..sans.remplacer.la.relation.humaine."),
      QA10_AVENIR_EDUC_ARGENT_PUB=any_of("Q..actu....L.argent.public.doit.être.exclusivement.consacré.aux.écoles..établissements.et.formations.publics."),
      QA10_AVENIR_EDUC_REUSSI_TOUS=any_of("Q..actu....Il.faut.viser.la.réussite.de.toutes.et.tous..sans.sélection.précoce.ni.mise.à.l.écart."),
      QA10_AVENIR_EDUC_REALITE_TERRITOIRE=any_of("Q..actu....L.éducation.doit.prendre.en.compte.les.réalités.territoriales.sans.renoncer.à.un.cadre.national.commun.qui.garantit.l.égalité."),
      QA10_AVENIR_EDUC_INCLUSION=any_of("Q..actu....L.inclusion.et.l.hétérogénéité.de.tous.les.élèves.sont.des.richesses..même.si.cela.demande.plus.de.moyens."),
      QA10_AVENIR_EDUC_CITOYENS_LIBRES=any_of("Q..actu....Le.système.éducatif.prépare.les.jeunes.à.devenir.des.citoyens.libres.et.pas.uniquement.à.ce.qu.ils.s.insèrent.professionnellement."),
      QA11_INFO_MEDIAS=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.médias..presse..radio..TV.."),
      QA11_INFO_RESEAUX_SOCIAUX=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.réseaux.sociaux."),
      QA11_INFO_WEBINAIRES=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.formations..webinaires.ou.podcasts."),
      QA11_INFO_ECHANGES_COLLEGUES=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.échanges.avec.mes.collègues."),
      QA11_INFO_PUBLICATIONS=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.publications.institutionnelles."),
      QA11_INFO_COM_SYND=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.communications.des.syndicats."),
      QA11_INFO_NSPP=any_of("Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Ne.se.prononce.pas.")
    )
  
  # assign : pour affecter à paste0("baro", i) la data obtenue
  assign(                       
    paste0("baro", i),
    data
  )
}

#Caractéristique de la table 

dim(baro2025) # 53209 obs et 147 variables y compris l'année
names(baro2025)


# Création/Recodage des variables----

## Département : 4 zones----

dep_4 <- data.frame(
  code_dep = c(
    "075",  # Paris
    #Banlieue parisienne
    "077", "078", "091", "092", "093", "094", "095",
    #Départements très ruraux
    "003", "009", "015", "019", "023", "043", "046", "048", "058", "070",
    # Outre_mer
    "971", "972", "973", "974", "975", "976", "987","999"
  ),
  ZONE4 = c(
    "Paris",
    rep("Banlieue-Paris", 7),
    rep("Départements très ruraux", 10),
    rep("Outre-mer", 8)
  )
)

# Extraire les départements uniques du baromètre
autres_deps <- setdiff(unique(baro2025$CODE_DEP), dep_4$CODE_DEP)

# Créer les autres lignes
autres_zones <- data.frame(
  code_dep = autres_deps,
  ZONE4 = "Reste des départements"
)

## Fusion finale

dep_4 <- bind_rows(dep_4, autres_zones)

## Jointure

baro2025 <- baro2025 %>%
  left_join(dep_4, by = c("CODE_DEP" = "code_dep"))


## Création de la var représentant les différents DROM et COM et les autres départements ----

baro2025$OM<-case_when(
  baro2025$Votre.département == "971 GUADELOUPE" ~ "GUADELOUPE",
  baro2025$Votre.département == "972 MARTINIQUE" ~ "MARTINIQUE",
  baro2025$Votre.département == "973 GUYANE" ~ "GUYANE",
  baro2025$Votre.département == "974 RÉUNION" ~ "LA RÉUNION",
  baro2025$Votre.département == "975 ST PIERRE ET MIQUELON" ~ "ST PIERRE ET MIQUELON",
  baro2025$Votre.département == "976 MAYOTTE" ~ "MAYOTTE",
  baro2025$Votre.département == "987 POLYNÉSIE FRANCAISE" ~ "POLYNÉSIE FRANCAISE",
  baro2025$Votre.département == "999 AUTRES COM ET NOUVELLE CALÉDONIE" ~ "AUTRES COM ET NOUVELLE CALÉDONIE",
  T ~ "Autres départements")


