# Chargement des packages----

library(tidyverse) # Chargement tidyverse
library(readxl)
library(questionr)
library(readxl) #pour pouvoir par exemple importer les tables excel
library(foreign) #pour pouvoir par exemple importer les tables dbf
library(haven) #pour pouvoir par exemple importer les tables SAS, SPSS, STATA
library(FactoMineR)
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

# Importation de la base de données----

## Identification de l'encodage adéquat----

library(readr)

guess_encoding("baro2025.csv")

## Importation proprement dite----

###Adapté pour les questions tradi (Pour le traitement des questions actu, on fera le filtrage de ceux qui ont effectivement répondu aux questions-

baro<-read.csv2("baro2025.csv", fileEncoding = "ISO-8859-1",stringsAsFactors = T, na.strings = c("N/A","Ne se prononce pas")) # na.strings = "N/A" pour instruire que les "N/A" soient traités comme des NA 


#Caractéristique de la table 

dim(baro) # 53209 obs et 146 variables
names(baro)


# Création/Recodage des variables----

## Questions----

baro <- baro %>%
  rename(
    EDUC_PRIO="J.exerce.en.éducation.prioritaire..REP..REP......",
    ANCIENNETE="Votre.ancienneté.dans.le.métier",
    LIEU_EXERCICE="Votre.lieu.d.exercice",
    CATEGORIE="Votre.catégorie",
    AGE="Votre.âge",
    METIER="Votre.métier",
    ADHESION_SYNDICALE="Vous.êtes.adhérent.e.ou.sympathisant.e",
    SECTEUR = "Votre.champ.ministériel.ou.votre.secteur.d.activité.dans.le.privé",
    QT1_TRAVAIL_SALAIRE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Salaire.",
    QT1_TRAVAIL_EPANOUI = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Épanouissement.",
    QT1_TRAVAIL_NECESSITE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Nécessité.",
    QT1_TRAVAIL_STABLILITE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Stabilité.",
    QT1_TRAVAIL_SOCIAL = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Lien.social.",
    QT1_TRAVAIL_EQUIPE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Équipe.",
    QT1_TRAVAIL_EQUI_VIE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Équilibre.vie.pro...vie.perso.",
    QT1_TRAVAIL_UTILITE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Utilité.",
    QT1_TRAVAIL_PARTAGE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Partage.",
    QT1_TRAVAIL_CARRIERE = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Carrière.",
    QT1_TRAVAIL_SENS = "Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Sens.",
    QT1_TRAVAIL_NSPP="Q..tradi....1..En.pensant.à.ce.que.devrait.être.le.travail..quel.est.le.mot.qui.vous.vient.en.premier....Ne.se.prononce.pas.",
    QT2_RECONNAISSANCE = "Q..tradi....2..Globalement..ressentez.vous.de.la.reconnaissance.et.du.respect.dans.votre.pratique.professionnelle..",
    QT3_AMOUR_PROFESSION="Q..tradi....3..Aimez.vous.la.profession.que.vous.exercez..",
    QT4_HEUREUX="Q..tradi....4..Êtes.vous.heureux.se.d.exercer.votre.profession..",
    QT5_SENS="Q..tradi....5..Les.missions.qui.vous.sont.confiées.ont.elles.du.sens.pour.vous..",
    QT6_CHOIX_METIER="Q..tradi....6..Parmi.les.affirmations.suivantes..laquelle.vous.correspond.le.mieux..",
    QT7_CT_SATISTFAISANTES="Q..tradi....7..Diriez.vous.que.vos.conditions.de.travail.sont.satisfaisantes..",
    QT8_CHARGE_DELAI = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Une.charge.de.travail.excessive..des.délais.trop.courts.",
    QT8_STRESS_EMOTION = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Des.situations.de.stress.ou.émotionnellement.difficiles.à.gérer.",
    QT8_AUTONOMIE = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.manque.d.autonomie.ou.l.impossibilité.de.participer.aux.décisions.",
    QT8_RECONNAISSANCE = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.manque.de.reconnaissance.de.votre.engagement.",
    QT8_CONSIGNES_INADAPTEES = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Des.consignes.inadaptées.ou.inefficaces.qu.il.faut.pourtant.appliquer.",
    QT8_CRAINTES_AVENIR = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.sentiment.de.crainte.concernant.l.avenir.de.votre.profession.",
    QT8_OUTILS_INADEQ = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Faire.face.à.des.outils.de.travail.inadaptés.ou.inefficaces.",
    QT8_CADRE_NONSUR = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Un.cadre.de.travail.non.sûr.",
    QT8_NSPP = "Q..tradi....8..Parmi.les.situations.suivantes..à.laquelle.vous.sentez.vous.particulièrement.exposé.e....Ne.se.prononce.pas.",
    QT9_RELATIONS_HIERARCHIE = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Relations.hiérarchiques.",
    QT9_AMENAGE_LT = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Aménagement.du.lieu.de.travail.",
    QT9_APPUI_EXERCICE_PRO = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Appui.à.l.exercice.professionnel.",
    QT9_CHARGE_T = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Charge.de.travail.",
    QT9_FORMATION = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Formation.professionnelle.continue.",
    QT9_CARRIERE = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Perspectives.de.carrière.",
    QT9_POUVOIR_ACHAT = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Pouvoir.d.achat.",
    QT9_VARIETE_PARCOURS = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Possibilité.d.un.parcours.professionnel.varié.",
    QT9_SANTE = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Santé.au.travail.",
    QT9_TEMPS_TRAVAIL = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.travail.",
    QT9_TEMPS_CONCERTATION_EQUIPE = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Temps.de.concertation..équipe..",
    QT9_NSPP = "Q..tradi....9..Parmi.les.domaines.suivants..quels.seraient.les.trois.que.vous.voudriez.voir.améliorés.prioritairement....Ne.se.prononce.pas.",
    QT10_SECURITE_CLIMAT="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....La.sécurité..le.climat.sur.mon.lieu.d.exercice.",
    QT10_EQUILIBRE_VIE="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.équilibre.entre.vie.professionnelle.et.vie.personnelle.",
    QT10_AMENAGE_LT="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.aménagement.de.mon.lieu.de.travail.",
    QT10_AMENAGE_PT="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....L.aménagement.de.mon.poste.de.travail.",
    QT10_OUTILS="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Les.outils.avec.lesquels.je.travaille.",
    QT10_RENFORCEMENT_COMPETENCES="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Être.formé.e..renforcer.ou.développer.mes.compétences.",
    QT10_RELATIONS_HIERARCHIE="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Le.pilotage..le.management..les.relations.hiérarchiques.",
    QT10_MOYENS="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Avoir.des.moyens.supplémentaires.",
    QT10_ISOLEMENT="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Le.sentiment.d.isolement.",
    QT10_NSPP="Q..tradi....10..Pour.améliorer.votre.quotidien..quels.sont.les.domaines..jusqu.à.3..sur.lesquels.agir.en.priorité....Ne.se.prononce.pas.",
    QT11_CHANGE_METIER_PUB="Q..tradi....11..Dans.le.secteur.public",
    QT12_CHANGE_METIER_PRIV="Q..tradi....12..Dans.le.secteur.privé",
    QT13_BENEFICIER_FORMATION="Q..tradi....13..Durant.ces.3.dernières.années..avez.vous.pu.bénéficier.d.une.formation.initiale.ou.continue.qui.réponde.à.vos.attentes..",
    QT14_ACCORD_CHOIX_POLITIQUES="Q..tradi....14..Diriez.vous.que.vous.êtes.en.accord.avec.les.choix.politiques.faits.dans.votre.secteur.d.activité..",
    QT15_CONSEIL_METIER="Q..tradi....15..Conseilleriez.vous.votre.métier.à.un.e.jeune.de.votre.entourage..",
    QA1_MOTIV_SENS_UTILITE="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.sens.et.l.utilité.de.ce.que.je.fais.",
    QA1_MOTIV_LIEN_SOCIAL="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.lien.social.avec.les.jeunes.ou.les.publics.accompagnés.",
    QA1_MOTIV_INTELLECT="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....L.intérêt.intellectuel.ou.la.diversité.des.missions.",
    QA1_MOTIV_LIBERTE_AUTONOMIE="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....La.liberté.ou.l.autonomie.que.permet.ma.pratique.",
    QA1_MOTIV_EQUIPE_ECHANGES="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Le.travail.en.équipe.et.les.échanges.professionnels.",
    QA1_MOTIV_STABILITE_EMPLOI="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....La.stabilité.de.l.emploi.",
    QA1_MOTIV_EQUILIBRE_VIE="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....L.équilibre.vie.personnelle...vie.professionnelle.",
    QA1_MOTIV_NSPP="Q..actu....1..Qu.est.ce.qui.vous.motive.dans.votre.métier.aujourd.hui....Ne.se.prononce.pas.",
    QA2_ATTRACTIVITE_SALAIRES_BAS="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Des.salaires.trop.bas.par.rapport.aux.responsabilités.",
    QA2_ATTRACTIVITE_IMAGE_DEGRADEE="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Une.image.dégradée.dans.l.opinion.",
    QA2_ATTRACTIVITE_IMPACT_REFORMES="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Trop.de.réformes.qui.impactent.notre.secteur.",
    QA2_ATTRACTIVITE_CT ="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Le.manque.de.moyens.et.les.conditions.de.travail.",
    QA2_ATTRACTIVITE_PAS_DAVIS="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Je.ne.partage.pas.ce.constat.",
    QA2_ATTRACTIVITE_NSPP="Q..actu....2..Pourtant..à.votre.avis.pourquoi.nos.métiers.n.attirent.plus.autant....Ne.se.prononce.pas.",
    QA3_AGMENTATION_SALAIRE="Q..actu....3..Selon.vous..quel.niveau.d.augmentation.de.salaire.serait.juste.aujourd.hui..",
    QA4_MESURES_FRAIS_PRO="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Mieux.prendre.en.charge.les.frais.professionnels..déplacements..repas..",
    QA4_MESURES_AIDE_LOGEMENT_INSTALL="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Bénéficier.d.aides.au.logement.et.à.l.installation.",
    QA4_MESURES_PRIMES="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Ajouter.ou.revaloriser.les.primes.qui.reconnaissent.l.investissement.de.chacun.",
    QA4_MESURES_PROTECTION_SOCIALES="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Améliorer.la.protection.sociale..mutuelle..prévoyance..",
    QA4_MESURES_AVANTAGES_COLLECTIFS="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Offrir.plus.d.avantages.collectifs.type.comité.d.entreprise.",
    QA4_MESURES_NSPP="Q..actu....4..Hors.rémunération..quelles.mesures.amélioreraient.le.plus.votre.pouvoir.d.achat....Ne.se.prononce.pas.",
    QA5_CHARGE_LOURDE="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Trop.lourde..pas.assez.reconnue.",
    QA5_CHARGE_MAL_REPARTIE="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Mal.répartie..on.manque.de.personnels.",
    QA5_CHARGE_MAL_ORGANISEE="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Mal.organisée..je.perds.du.temps.",
    QA5_CHARGE_ALLEGEABLE="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Allégeable.avec.de.meilleurs.outils..numérique..IA.....",
    QA5_CHARGE_EQUILIBREE="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Je.la.trouve.plutôt.équilibrée..elle.me.convient.",
    QA5_CHARGE_NSPP="Q..actu....5..Comment.vivez.vous.votre.charge.de.travail.aujourd.hui....Ne.se.prononce.pas.",
    QA6_CARRIERE_POINT_PARCOURS="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Pouvoir.faire.le.point.sur.mon.parcours..savoir.où.j.en.suis..et.ce.que.je.pourrais.faire.ensuite.",
    QA6_CARRIERE_PARENTHESE="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Avoir.droit.à.une.parenthèse.pour.vivre.d.autres.expériences.",
    QA6_CARRIERE_FORMATION="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Me.former.pour.acquérir.de.nouvelles.compétences.ou.certifications.",
    QA6_CARRIERE_EVOLUTION_POSTES_RESP="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Évoluer.vers.des.postes.à.responsabilités.accrues.au.sein.de.mon.domaine.actuel.",
    QA6_CARRIERE_CHANGER_METIER="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Pouvoir.changer.de.métier.ou.évoluer.vers.un.autre.domaine.",
    QA6_CARRIERE_INNOVER="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Avoir.les.moyens.d.innover.ou.porter.des.projets.qui.soient.reconnus.",
    QA6_CARRIERE_NSPP="Q..actu....6..Qu.est.ce.qui.permettrait..selon.vous..d.améliorer.les.perspectives.de.carrière....Ne.se.prononce.pas.",
    QA7_ACTION_SYND_EFFICACE="Q..actu....7..Pensez.vous.que.l.action.syndicale.est.efficace.sur.ces.enjeux.d.attractivité.de.nos.métiers..",
    
    QA8_ATTENTES_SYND_NEGOCIER_ACCORDS="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Négocier.des.accords.concrets.",
    QA8_ATTENTES_SYND_MOB_MASSIVES="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Organiser.des.mobilisations.massives.",
    QA8_ATTENTES_SYND_CAMPAGNES_LIGNE_MEDIAS="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Mener.des.campagnes.en.ligne.ou.dans.les.médias.",
    QA8_ATTENTES_SYND_UNION="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....S.unir.pour.peser.plus.fort.",
    QA8_ATTENTES_SYND_FAIRE_PRESSION="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Faire.pression.sur.les.décideurs.politiques.",
    QA8_ATTENTES_SYND_NSPP="Q..actu....8..Pour.agir.concrètement..que.devraient.faire.les.syndicats....Ne.se.prononce.pas.",
    QA9_FinAssu_GRATUIT_LAIQUE="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Offrir.une.École.gratuite..laïque.et.ouverte.à.tous.",
    QA9_FinAssu_APPRENTI_FOND="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Assurer.les.apprentissages.fondamentaux.pour.toutes.et.tous.",
    QA9_FinAssu_RED_INEGALITES="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Réduire.les.inégalités.entre.les.élèves.et.les.territoires.",
    QA9_FinAssu_AIDE_JEUNES="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Aider.les.jeunes.à.comprendre.le.monde.et.à.y.prendre.part.",
    QA9_FinAssu_COMPETENCES_UTILES="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Préparer.aux.compétences.utiles.pour.l.insertion.professionnelle.",
    QA9_FinAssu_EPANOUI_PERSO_CULTURE="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Encourager.l.épanouissement.personnel.et.culturel.",
    QA9_FinAssu_FORMATION_ELITE="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Former.une.élite.nationale.",
    QA9_FinAssu_VIVRE_ENS="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Garantir.les.bases.du.vivre.ensemble.",
    QA9_FinAssu_NSPP="Q..actu....9..L.attractivité.de.nos.métiers.passe.aussi.par.la.perception.globale.du.système.éducatif..Selon.vous..aujourd.hui..quelles.sont.les.finalités.les.mieux.assurées....Ne.se.prononce.pas.",
    QA10_AVENIR_EDUC_VIVRE_ENS="Q..actu....Si.l.École.doit.transmettre.les.savoirs.fondamentaux..elle.doit.tout.autant.garantir.l.apprentissage.du.vivre.ensemble.",
    QA10_AVENIR_EDUC_LAICITE_PRINCIPE="Q..actu....La.laïcité.est.un.principe.de.liberté.qui.protège.dans.le.respect.de.la.loi.",
    QA10_AVENIR_EDUC_MISSION_PUB="Q..actu....L.éducation.doit.rester.une.mission.publique.préservée.de.toute.logique.de.marchandisation.",
    QA10_AVENIR_EDUC_VIVRE_ENSIA_MODEREE="Q..actu....Le.numérique.et.l.intelligence.artificielle.doivent.être.utilisés.avec.discernement..sans.remplacer.la.relation.humaine.",
    QA10_AVENIR_EDUC_ARGENT_PUB="Q..actu....L.argent.public.doit.être.exclusivement.consacré.aux.écoles..établissements.et.formations.publics.",
    QA10_AVENIR_EDUC_REUSSI_TOUS="Q..actu....Il.faut.viser.la.réussite.de.toutes.et.tous..sans.sélection.précoce.ni.mise.à.l.écart.",
    QA10_AVENIR_EDUC_REALITE_TERRITOIRE="Q..actu....L.éducation.doit.prendre.en.compte.les.réalités.territoriales.sans.renoncer.à.un.cadre.national.commun.qui.garantit.l.égalité.",
    QA10_AVENIR_EDUC_INCLUSION="Q..actu....L.inclusion.et.l.hétérogénéité.de.tous.les.élèves.sont.des.richesses..même.si.cela.demande.plus.de.moyens.",
    QA10_AVENIR_EDUC_CITOYENS_LIBRES="Q..actu....Le.système.éducatif.prépare.les.jeunes.à.devenir.des.citoyens.libres.et.pas.uniquement.à.ce.qu.ils.s.insèrent.professionnellement.",
    QA11_INFO_MEDIAS="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.médias..presse..radio..TV..",
    QA11_INFO_RESEAUX_SOCIAUX="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.réseaux.sociaux.",
    QA11_INFO_WEBINAIRES="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.formations..webinaires.ou.podcasts.",
    QA11_INFO_ECHANGES_COLLEGUES="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.échanges.avec.mes.collègues.",
    QA11_INFO_PUBLICATIONS="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.publications.institutionnelles.",
    QA11_INFO_COM_SYND="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Les.communications.des.syndicats.",
    QA11_INFO_NSPP="Q..actu....11..Pour.suivre.ces.débats..et.rester.informé.e.dans.le.cadre.de.votre.pratique.professionnelle..à.quoi.avez.vous.recours....Ne.se.prononce.pas."
  )

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
autres_deps <- setdiff(unique(baro$code_dep), dep_4$code_dep)

# Créer les autres lignes
autres_zones <- data.frame(
  code_dep = autres_deps,
  ZONE4 = "Reste des départements"
)

## Fusion finale

dep_4 <- bind_rows(dep_4, autres_zones)

## Jointure

baro <- baro %>%
  left_join(dep_4, by = "code_dep")



##METIER en 10 classes -----

baro$METIER10<-fct_collapse(factor(baro$METIER),
                            "Enseignants et autres éduc."=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport", "Autre formateur·trice","Conseiller·ère pédagogique","Conseiller·ère en formation continue (CFC)","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Animateur·trice"),
                            "AESH, AED, EVS"="AESH, assistant·e d'éducation, EVS",
                            "Pers. sociaux et de sante"=c("Assistant·e de service social et Conseiller·ère Technique","Psychologue","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"),
                            "Dir. d'école"="Directeur·trice d'école",
                            "Pers. de direction d'etbt"=c("Directeur·trice de SEGPA", "Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole"),
                            "Inspecteurs"=c("Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports"),
                            "Pers. administratif"="Administratif·tive, gestionnaire",
                            "Pers. technique"=c("Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Ouvrier·ère (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste"),
                            "Chercheurs"="Chercheur·e et Enseignant·e-chercheur·e",
                            "Professions non catégorisées"="Autre")


##METIER en 15 classes -----


baro$METIER15 <-case_when(
  baro$METIER == "Administratif·tive, gestionnaire" ~ "AESH, AED, EVS",
  baro$METIER == "AESH, assistant·e d'éducation, EVS" ~ "Admin, gestionnaire",
  baro$METIER == "Assistant·e de service social et Conseiller·ère Technique" ~ "Assist. de service social et CT",
  baro$METIER == "Chercheur·e et Enseignant·e-chercheur·e" ~ "Chercheurs et ens. chercheurs",
  baro$METIER == "Conseiller·ère pédagogique" ~ "Conseillers pédagogiques",
  baro$METIER == "Directeur·trice d'école" ~ "Dir. d'école",
  baro$METIER == "Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)" ~ "DDFPT",
  baro$METIER == "Enseignant·e, CPE, ..." ~ "Enseignant·e, CPE",
  baro$METIER == "Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)" ~ "Infirmiers",
  baro$METIER == "Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" ~ "Ingénieurs et tech.",
  baro$METIER %in% c("Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports") ~ "Inspecteurs",
  baro$METIER == "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)" ~ "Médecins",
  baro$METIER == "Personnel de direction" ~ "Pers. de direction",
  baro$METIER == "Psychologue" ~ "Psychologue",
  T ~ "Autres")

##METIER en 9 classes -----

baro$METIER9<-fct_collapse(factor(baro$METIER),
                           "Enseignants"=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport"),
                           "Conseillers pedagogiques" = c("Autre formateur·trice","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)"),
                           "AESH..."="AESH, assistant·e d'éducation, EVS",
                           "Autres Pers sociaux et de sante"=c("Assistant·e de service social et Conseiller·ère Technique","Psychologue","Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"),
                           "Pers de direction d'etbt"=c("Directeur·trice de SEGPA","Directeur·trice d'école","Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole"),
                           "Inspecteurs"=c("Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports"),
                           "Pers admn tech et de support"=c("Administratif·tive, gestionnaire","Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste","Ouvrier·ère (en service scolaire, agricole ou ESR)"),
                           "Chercheurs"="Chercheur·e et Enseignant·e-chercheur·e",
                           "Autres métiers"="Autre")


##METIER en 7 classes + Autre-----

baro$METIER7<-fct_collapse(factor(baro$METIER),
                           "Enseignants"=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport"),
                           "Conseillers pedagogiques" = c("Autre formateur·trice","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)"),
                           "Pers sociaux et de sante"=c("AESH, assistant·e d'éducation, EVS","Assistant·e de service social et Conseiller·ère Technique","Psychologue","Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"),
                           "Pers de direction d'etbt"=c("Directeur·trice de SEGPA","Directeur·trice d'école","Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole"),
                           "Inspecteurs"=c("Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports"),
                           "Pers admn tech et de support"=c("Administratif·tive, gestionnaire","Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste","Ouvrier·ère (en service scolaire, agricole ou ESR)"),
                           "Chercheurs"="Chercheur·e et Enseignant·e-chercheur·e",
                           "Autres métiers"="Autre")


##METIER en 6 classes avec Autre-----

baro$METIER6<-fct_collapse(factor(baro$METIER),
                           "Enseignants"=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport"),
                           "Pers sociaux et de sante"=c("AESH, assistant·e d'éducation, EVS","Assistant·e de service social et Conseiller·ère Technique","Psychologue","Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"),
                           "Personnel d'encadrement"=c("Directeur·trice de SEGPA","Directeur·trice d'école","Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole","Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports","Autre formateur·trice","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)"),
                           "Pers admn tech et de support"=c("Administratif·tive, gestionnaire","Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste","Ouvrier·ère (en service scolaire, agricole ou ESR)"),
                           "Chercheurs"="Chercheur·e et Enseignant·e-chercheur·e",
                           "Autres métiers"="Autre")


##METIER en 2 classes : AESH et les autres-----

baro$METIER2_AESH<-fct_collapse(factor(baro$METIER),
                                "AESH, assistant·e d'éducation, EVS"="AESH, assistant·e d'éducation, EVS",
                                "Les autres métiers"=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport","Assistant·e de service social et Conseiller·ère Technique","Psychologue","Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)","Directeur·trice de SEGPA","Directeur·trice d'école","Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole","Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports","Autre formateur·trice","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)","Administratif·tive, gestionnaire","Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste","Ouvrier·ère (en service scolaire, agricole ou ESR)","Chercheur·e et Enseignant·e-chercheur·e","Autre"))

## Création de la var représentant les différents DROM et COM et les autres départements ----

baro$OM<-case_when(
  baro$Votre.département == "971 GUADELOUPE" ~ "GUADELOUPE",
  baro$Votre.département == "972 MARTINIQUE" ~ "MARTINIQUE",
  baro$Votre.département == "973 GUYANE" ~ "GUYANE",
  baro$Votre.département == "974 RÉUNION" ~ "LA RÉUNION",
  baro$Votre.département == "975 ST PIERRE ET MIQUELON" ~ "ST PIERRE ET MIQUELON",
  baro$Votre.département == "976 MAYOTTE" ~ "MAYOTTE",
  baro$Votre.département == "987 POLYNÉSIE FRANCAISE" ~ "POLYNÉSIE FRANCAISE",
  baro$Votre.département == "999 AUTRES COM ET NOUVELLE CALÉDONIE" ~ "AUTRES COM ET NOUVELLE CALÉDONIE",
  T ~ "Autres départements")

##SYNDICAT en 2 classes :-----

### SNIES et les autres----

baro$SYNDICAT_SNIES <-case_when(
  baro$METIER == "Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)" ~ "Infirmier·ères (INFENES, agri., CT)",
  T ~ "Autres métiers")

### A&I et les autres----

baro$SYNDICAT_AI <-case_when(
  baro$METIER == "Administratif·tive, gestionnaire" ~ "Administratif.tives, gest.",
  T ~ "Autres métiers")

### SEA et les autres----

baro$SYNDICAT_SEA<-case_when(
  ((baro$METIER == "Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole") | ((baro$METIER == "Administratif·tive, gestionnaire") & (baro$SECTEUR =="Agriculture")) | (baro$Métiers_tout == "Enseignant·e, CPE, ... Enseignant·e - enseignement agricole")) ~ "Pers. ens. agricole",
  T ~ "Autres métiers")

### SEJS et les autres----

baro$SYNDICAT_SEJS <-case_when(
  baro$METIER == "Inspecteur·trice de la Jeunesse et des sports" ~ "Insp. de la jeunesse et des sports",
  T ~ "Autres métiers")


### SEP et les autres----

baro$SYNDICAT_SEP <-case_when(
  baro$METIER %in% c("Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)", "Conseiller·ère Technique et Pédagogique Supérieur (CTPS)") ~ "Animateur.trices, CEPJ, CTPS",
  T ~ "Autres métiers")

### SE et les autres----


baro$SYNDICAT_SE<-case_when(
  (baro$METIER %in% c("Autre formateur·trice","Directeur·trice d'école","Directeur·trice de SEGPA","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","AESH, assistant·e d'éducation, EVS", "Psychologue")) | (baro$Métiers_tout %in% c("Enseignant·e, CPE, ... Certifié·e","Enseignant·e, CPE, ... Enseignant·e en primaire","Enseignant·e, CPE, ... Agrégé·e", "Enseignant·e, CPE, ... Enseignant·e en élémentaire", "Enseignant·e, CPE, ... CPE", "Enseignant·e, CPE, ... Enseignant·e spécialisé·e","Enseignant·e, CPE, ... Enseignant·e en maternelle", "Enseignant·e, CPE, ... Professeur·e de lycée professionnel","Enseignant·e, CPE, ... Autre","Enseignant·e, CPE, ... Professeur·e d'EPS", "Chercheur·e et Enseignant·e-chercheur·e PRCE")) ~ "Pers. ens. scolaire (Ens., CPE, PsyEN, AED, AESH, ...)",
  T ~ "Autres métiers")


#### Base SE (juste pour faire des vérifications au niveau de la console !)

baro_SE <- baro %>% 
  filter((METIER %in% c("Autre formateur·trice","Directeur·trice d'école","Directeur·trice de SEGPA","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","AESH, assistant·e d'éducation, EVS", "Psychologue")) | (Métiers_tout %in% c("Enseignant·e, CPE, ... Certifié·e","Enseignant·e, CPE, ... Enseignant·e en primaire","Enseignant·e, CPE, ... Agrégé·e", "Enseignant·e, CPE, ... Enseignant·e en élémentaire", "Enseignant·e, CPE, ... CPE", "Enseignant·e, CPE, ... Enseignant·e spécialisé·e","Enseignant·e, CPE, ... Enseignant·e en maternelle", "Enseignant·e, CPE, ... Professeur·e de lycée professionnel","Enseignant·e, CPE, ... Autre","Enseignant·e, CPE, ... Professeur·e d'EPS", "Chercheur·e et Enseignant·e-chercheur·e PRCE")))

TAP_METIER_SE<- table(baro_SE$Métiers_tout)
write.csv2(TAP_METIER_SE,"TAP_METIER_SE.csv", fileEncoding = "WINDOWS-1252")

#### AESH et les autres----

baro$METIER2_AESH_SE<-fct_collapse(factor(baro$METIER),
                                   "AESH, AED et EVS"="AESH, assistant·e d'éducation, EVS",
                                   "Autres métiers"=c("Enseignant·e, CPE, ...","Professeur·e de la Ville de Paris","Professeur·e de sport","Assistant·e de service social et Conseiller·ère Technique","Psychologue","Animateur·trice","Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)","Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice","Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)",  "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)","Directeur·trice de SEGPA","Directeur·trice d'école","Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)","Personnel de direction","Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole","Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)","Inspecteur·trice de l'Éducation nationale (IEN)","Inspecteur·trice de l'enseignement agricole","Inspecteur·trice de la Jeunesse et des sports","Autre formateur·trice","Conseiller·ère en formation continue (CFC)","Conseiller·ère pédagogique","Conseiller·ère Technique et Pédagogique Supérieur (CTPS)","Administratif·tive, gestionnaire","Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)","Bibliothécaire","Architecte et urbaniste","Ouvrier·ère (en service scolaire, agricole ou ESR)","Chercheur·e et Enseignant·e-chercheur·e","Autre"))


#### SE Outre mer et les autres----

##### Condition globale sur les métiers du SE 

condition_metier <- (baro$METIER %in% c(
  "Autre formateur·trice", "Directeur·trice d'école", "Directeur·trice de SEGPA",
  "Conseiller·ère en formation continue (CFC)", "Conseiller·ère pédagogique",
  "AESH, assistant·e d'éducation, EVS", "Psychologue")) |
  (baro$Métiers_tout %in% c(
    "Enseignant·e, CPE, ... Certifié·e",
    "Enseignant·e, CPE, ... Enseignant·e en primaire",
    "Enseignant·e, CPE, ... Agrégé·e",
    "Enseignant·e, CPE, ... Enseignant·e en élémentaire",
    "Enseignant·e, CPE, ... CPE",
    "Enseignant·e, CPE, ... Enseignant·e spécialisé·e",
    "Enseignant·e, CPE, ... Enseignant·e en maternelle",
    "Enseignant·e, CPE, ... Professeur·e de lycée professionnel",
    "Enseignant·e, CPE, ... Autre",
    "Enseignant·e, CPE, ... Professeur·e d'EPS",
    "Chercheur·e et Enseignant·e-chercheur·e PRCE")
  )

##### Création de la var des DROM et COM du SE

baro$DEP_OM<-case_when(
  baro$Votre.département == "971 GUADELOUPE" & condition_metier ~ "GUADELOUPE",
  baro$Votre.département == "972 MARTINIQUE" & condition_metier ~ "MARTINIQUE",
  baro$Votre.département == "973 GUYANE" & condition_metier ~ "GUYANE",
  baro$Votre.département == "974 RÉUNION" & condition_metier ~ "LA RÉUNION",
  baro$Votre.département == "975 ST PIERRE ET MIQUELON" & condition_metier ~ "ST PIERRE ET MIQUELON",
  baro$Votre.département == "976 MAYOTTE" & condition_metier ~ "MAYOTTE",
  baro$Votre.département == "987 POLYNÉSIE FRANCAISE" & condition_metier ~ "POLYNÉSIE FRANCAISE",
  baro$Votre.département == "999 AUTRES COM ET NOUVELLE CALÉDONIE" & condition_metier ~ "AUTRES COM ET NOUVELLE CALÉDONIE",
  T ~ "Autres départements")


#### SE par tranche d'âge et les autres----

baro$SYNDICAT_SE_AGE <-case_when(
  baro$AGE == "- de 35 ans" & condition_metier ~ "Moins de 35 ans",
  baro$AGE == "de 35 à 45 ans" & condition_metier ~ "35 - 45 ans",
  baro$AGE == "de 46 à 55 ans" & condition_metier ~ "46 - 55 ans",
  baro$AGE == "+ de 55 ans" & condition_metier ~ "Plus de 55 ans",
  T ~ "Autre syndicat")

#### SE Directeur d'école et les autres----

baro$SYNDICAT_SE_DIR <-case_when(
  baro$METIER == "Directeur·trice d'école" & condition_metier ~ "Directeur·trices d'école",
  T ~ "Autre métier")

#### SE EDUCATION PRIORITAIRE et les autres----

baro$SYNDICAT_SE_EDuC_PRIO <-case_when(
  baro$EDUC_PRIO == "Oui" & condition_metier ~ "Pers. ens. scolaire en éduc. prioritaire",
  T ~ "Pas en éducation prioritaire")

#### SE Collèges et les autres----

baro$SYNDICAT_SE_Col <-case_when(
  baro$LIEU_EXERCICE == "Collèges" & condition_metier ~ "Pers. ens. scolaire des collèges",
  T ~ "Autre pers.")


#### SE Lycées pro et les autres----

baro$SYNDICAT_SE_LP <-case_when(
  baro$LIEU_EXERCICE == "Lycées Professionnels (LP)" & condition_metier ~ "Pers. ens. scolaire des Lycées pro.",
  T ~ "Autre pers.")


### SIEN et les autres----

baro$SYNDICAT_SIEN <-case_when(
  baro$METIER == "Inspecteur·trice de l'Éducation nationale (IEN)" ~ "IEN",
  T ~ "Autre métier")

### SNAPS et les autres----

baro$SYNDICAT_SNAPS <-case_when(
  baro$METIER %in% c("Professeur·e de sport", "Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)", "Conseiller·ère Technique et Pédagogique Supérieur (CTPS)") ~ "Prof. sport, CEPJ, CTPS",
  T ~ "Autre métier")

### SNASEN et les autres----

baro$SYNDICAT_SNASEN <-case_when(
  baro$METIER == "Assistant·e de service social et Conseiller·ère Technique" ~ "Assist. Service social et CT",
  T ~ "Autre métier")

### SNIA-IPR et les autres----

baro$SYNDICAT_SNIA_IPR <-case_when(
  baro$METIER == "Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)" ~ "IA-IPR",
  T ~ "Autre métier")

### SNMSU et les autres----

baro$SYNDICAT_SNMSU <-case_when(
  baro$METIER == "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)" ~ "Médecins (scolaires, agri., univ. et CT)",
  T ~ "Autre métier")


### SN2D et les autres----

baro$SYNDICAT_SN2D <-case_when(
  baro$METIER == "Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)" ~ "DDFPT",
  T ~ "Autre métier")

### SNPDEN et les autres----

baro$SYNDICAT_SNPDEN<-case_when(
  (baro$Métiers_tout %in% c("Personnel de direction Principal·e adjoint·e", "Personnel de direction Principal·e", "Personnel de direction Proviseur·e", "Personnel de direction Proviseur·e adjoint·e", "Personnel de direction Autre")) ~ "Pers. de direction",
  T ~ "Autres métiers")

### SNPTES et les autres----

baro$SYNDICAT_SNPTES<-case_when(
  (baro$METIER %in% c("Bibliothécaire", "Ouvrier·ère (en service scolaire, agricole ou ESR)", "Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)")) ~ "Pers. ITRF, Biblio. et ouvriers EN et ESR",
  T ~ "Autres métiers")

### STIP-AEP et les autres----

baro$SYNDICAT_STIP_AEP<-case_when(
  (baro$Votre.métier.détaillé %in% c("Enseignant·e en élémentaire", "Enseignant·e en maternelle", "Enseignant·e en primaire")) & (baro$région == "Polynésie française") ~ "Pers. Ens. 1er degré Polynésie française",
  T ~ "Autres métiers")

### SUP’RECHERCHE et les autres----

baro$SYNDICAT_SUP_RECHERCHE<-case_when(
  baro$METIER == "Chercheur·e et Enseignant·e-chercheur·e" ~ "Chercheur.es et ens. chercheur.es",
  T ~ "Autres métiers")

### ITRF.BI.O et les autres----

baro$SYNDICAT_ITRF_BI_O<-case_when(
  (baro$METIER %in% c("Bibliothécaire", "Ouvrier·ère (en service scolaire, agricole ou ESR)", "Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)")) ~ "Pers. ITRF, Biblio. et ouvriers EN et ESR",
  T ~ "Autres métiers")