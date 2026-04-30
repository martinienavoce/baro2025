# Métier ------

baro$METIER2026 <- NA

baro$METIER2026[baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale","Agriculture") & baro$Votre.métier.détaillé=="CPE"] <- "Conseiller·ère principal·e d'éducation (CPE)"

baro$METIER2026[baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Directeur·trice d'école"] <- "Directeur·trice d'école"

baro$METIER2026[baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale","Agriculture") & baro$METIER=="Enseignant·e, CPE, ..." & baro$Votre.métier.détaillé != "CPE"] <- "Enseignant·e"

baro$METIER2026[baro$METIER=="Administratif·tive, gestionnaire" & baro$MINISTERE != "Secteur privé"] <- "Personnel administratif"

baro$METIER2026[baro$METIER=="Personnel de direction" & baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale")] <- "Personnel de direction"

baro$METIER2026[baro$MINISTERE %in% c("Affaires sociales","Éducation nationale","Secteur privé") & baro$METIER=="Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice"] <- "Éducateur·trice spécialisé·e, Moniteur·trice éducateur·trice"

baro$METIER2026[baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)"] <- "Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Inspecteur·trice de l'enseignement agricole"] <- "Inspecteur·trice de l'enseignement agricole"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Directeur·trice de SEGPA"] <- "Directeur·trice de SEGPA"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER %in% c("Conseiller·ère pédagogique","Autre formateur·trice")] <- "Formateur·trice"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale", "Enseignement supérieur, recherche") & baro$METIER == "Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)"] <- "Infirmier·ère et Infirmier·ère conseiller·ère technique"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER == "Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)"] <- "Inspecteur·trice d'académie - Inspecteur·trice pédagogique régional·e (IA-IPR)"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER == "Inspecteur·trice de l'Éducation nationale (IEN)"] <- "Inspecteur·trice de l'Éducation nationale (IEN)"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale","Enseignement supérieur, recherche") & baro$METIER == "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"] <- "Médecin et Médecin conseiller·ère technique"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER == "Médecin (en service scolaire, agricole ou universitaire et conseiller·ère technique)"] <- "Médecin"

baro$METIER2026[baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire"] <- "Personnel des bibliothèques"

baro$METIER2026[baro$MINISTERE %in% c("Culture") & baro$METIER=="Architecte et urbaniste"] <- "Personnel scientifique"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale","Agriculture") & baro$METIER=="AESH, assistant·e d'éducation, EVS"] <- "AESH, AED"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale","Enseignement supérieur, recherche") & baro$METIER=="Assistant·e de service social et Conseiller·ère Technique"] <- "Assistant·e de service social et Conseiller·ère technique de service social"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Conseiller·ère en formation continue (CFC)"] <- "Conseiller·ère en formation continue (CFC) - Conseiller·ère en formation professionnelle (CFP)"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)"] <- "Directeur·trice délégué·e aux formations professionnelles et technologiques (DDFPT)"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Directeur·trice de SEGPA"] <- "Directeur·trice de SEGPA"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Professeur·e de la Ville de Paris"] <- "Professeur·e de la Ville de Paris"

baro$METIER2026[baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Psychologue"] <- "Psychologue EN"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole"] <- "Directeur·trice et Directeur·trice adjoint·e"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER %in% c("Autre formateur·trice")] <- "Formateur·trice"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER == "Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)"] <- "Infirmier·ère"

baro$METIER2026[baro$MINISTERE %in% c("Agriculture") & baro$METIER == "Ouvrier·ère (en service scolaire, agricole ou ESR)"] <- "Personnel ouvrier·ère"

baro$METIER2026[baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER == "Chercheur·e et Enseignant·e-chercheur·e"] <- "Enseignant·e-chercheur·e et Chercheur·e"

baro$METIER2026[baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER == "Animateur·trice"] <- "Animateur·trice"

baro$METIER2026[baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER == "Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)"] <- "Conseiller·ère d'Éducation Populaire et de Jeunesse (CEPJ)"

baro$METIER2026[baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER == "Conseiller·ère Technique et Pédagogique Supérieur (CTPS)"] <- "Conseiller·ère Technique et Pédagogique Supérieur (CTPS)"

baro$METIER2026[baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER == "Inspecteur·trice de la Jeunesse et des sports"] <- "Inspecteur·trice de la Jeunesse et des sports (IJS)"

baro$METIER2026[baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER == "Professeur·e de sport"] <- "Professeur·e de sport"

baro$METIER2026[baro$METIER == "Autre"] <- "Autre"

#Tests

test_METIER <- addmargins(table(baro$MINISTERE2026,baro$METIER, useNA = "ifany")) %>% 
  as.data.frame.matrix() %>% 
  tibble::rownames_to_column(var = "MINISTERE2026")

test_METIER2026 <- addmargins(table(baro$MINISTERE2026,baro$METIER2026, useNA = "ifany")) %>% 
  as.data.frame.matrix() 

colnames(test_METIER2026)[is.na(colnames(test_METIER2026))] <- "Vide_créé"

test_METIER2026 <- test_METIER2026 %>% 
  tibble::rownames_to_column(var = "MINISTERE2026")

test_METIER_final <- left_join(test_METIER,test_METIER2026,by = "MINISTERE2026") # Jointure des deux tables

test_METIER_final <- t(test_METIER_final) # Transposition

colnames(test_METIER_final) <- test_METIER_final[1, ] # Définir la 1ère ligne de la matrice comme entête

test_METIER_final <- test_METIER_final[-1, ] # Supression de la ligne de trop

test_METIER_final <- test_METIER_final %>% 
  as.data.frame.matrix() %>% 
  tibble::rownames_to_column(var = "MINISTERE2026")%>%
  mutate(across(-1, as.numeric))

write.csv2(test_METIER_final,"Table_METIER.csv", fileEncoding = "UTF-8", na="", row.names = F)

# Métier détaillé----

baro$METIER_DETAIL2026 <- case_when(
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Enseignant·e, CPE, ..." &  baro$Votre.métier.détaillé %in% c("Enseignant·e en maternelle","Enseignant·e en élémentaire","Enseignant·e en primaire") ~ "Professeur·e des écoles (PE)",
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Enseignant·e, CPE, ..." &  baro$Votre.métier.détaillé %in% c("Agrégé·e","Certifié·e","Professeur·e d'EPS","Professeur·e de lycée professionnel") ~ "Professeur·e du 2nd degré",
  
  baro$METIER=="Administratif·tive, gestionnaire" &  baro$Votre.métier.détaillé == "ADJAENES" ~ "Adjoint·e administratif·ve (ADJAENES, ADJA, etc.)",
  
  baro$METIER=="Administratif·tive, gestionnaire" &  baro$Votre.métier.détaillé == "SAENES" ~ "Secrétaire administratif·ve (SAENES, SA, etc.)",
  
  baro$METIER=="Administratif·tive, gestionnaire" &  baro$Votre.métier.détaillé == "AAE" ~ "Attaché·e d'administration (AAE, ...)",
  
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Personnel de direction" &  baro$Votre.métier.détaillé == "Principal·e" ~ "Principal·e",
  
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Personnel de direction" &  baro$Votre.métier.détaillé == "Principal·e adjoint·e" ~ "Principal·e adjoint·e",
  
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Personnel de direction" &  baro$Votre.métier.détaillé == "Proviseur·e" ~ "Proviseur·e",
  
  baro$MINISTERE %in% c("Europe et Affaires étrangères","Éducation nationale") & baro$METIER=="Personnel de direction" &  baro$Votre.métier.détaillé == "Proviseur·e adjoint·e" ~ "Proviseur·e adjoint·e",
  
  baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" &  baro$Votre.métier.détaillé %in% c("ITRF - Ingénieur·e de Recherche (IGR)","ITA - Ingénieur·e de Recherche (IR)") ~ "Ingénieur·e de Recherche",
  
  baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" &  baro$Votre.métier.détaillé %in% c("ITRF - Ingénieur·e d'Etudes (IGE)","ITA - Ingénieur·e d'Etudes (IE)") ~ "Ingénieur·e d'Etudes",
  
  baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" &  baro$Votre.métier.détaillé %in% c("ITRF - Assistant·e Ingénieur (ASI)","ITA - Assistant·e Ingénieur (AI)") ~ "Assistant·e Ingénieur",
  
  baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" &  baro$Votre.métier.détaillé %in% c("ITRF - Technicien·ne (TECH)","ITA - Technicien·ne (TR)") ~ "Technicien·ne",
  
  baro$MINISTERE %in% c("Culture","Éducation nationale","Agriculture","Enseignement supérieur, recherche") & baro$METIER=="Ingénieur·e et technicien·ne (en service scolaire, agricole ou ESR)" &  baro$Votre.métier.détaillé %in% c("ITRF - Adjoint·e Technique (ATRF)","ITA - Adjoint·e Technique (ATR)") ~ "Adjoint·e Technique",
  
  baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire" &  baro$Votre.métier.détaillé =="Conservateur·trice général·e" ~ "Conservateur·trice général·e",
  
  baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire" &  baro$Votre.métier.détaillé =="Conservateur·trice" ~ "Conservateur·trice",
  
  baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire" &  baro$Votre.métier.détaillé =="Bibliothécaire" ~ "Bibliothécaire",
  
  baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire" &  baro$Votre.métier.détaillé =="Bibliothécaire assistant·e spécialisé·e" ~ "Bibliothécaire assistant·e spécialisé·e",
  
  baro$MINISTERE %in% c("Culture","Enseignement supérieur, recherche") & baro$METIER=="Bibliothécaire" &  baro$Votre.métier.détaillé =="Magasinier·ère" ~ "Magasinier·ère",
  
  baro$MINISTERE %in% c("Éducation nationale","Agriculture") & baro$METIER=="AESH, assistant·e d'éducation, EVS" &  baro$Votre.métier.détaillé %in% c("AED","AED pré-pro") ~ "Assistant·e d'Éducation (AED)",
  
  baro$MINISTERE %in% c("Éducation nationale","Agriculture") & baro$METIER=="AESH, assistant·e d'éducation, EVS" &  baro$Votre.métier.détaillé == "AESH" ~ "Accompagnant·e d'élèves en situation de handicap (AESH)",
  
  baro$MINISTERE %in% c("Éducation nationale","Enseignement supérieur, recherche") & baro$METIER=="Assistant·e de service social et Conseiller·ère Technique" &  baro$Votre.métier.détaillé == "Assistant·e de service social" ~ "Assistant·e de service social",
  
  baro$MINISTERE %in% c("Éducation nationale","Enseignement supérieur, recherche") & baro$METIER=="Assistant·e de service social et Conseiller·ère Technique" &  baro$Votre.métier.détaillé == "Conseiller·ère technique de service social" ~ "Conseiller·ère technique de service social",
  
  baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Enseignant·e, CPE, ..." &  baro$Votre.métier.détaillé == "Enseignant·e spécialisé·e" ~ "Enseignant·e spécialisé·e",
  
  baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Autre formateur·trice" &  baro$Votre.métier.détaillé == "ERUN" ~ "Enseignant·e référent·e aux usages du numérique (ERUN)",
  
  baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Autre formateur·trice" &  baro$Votre.métier.détaillé == "Formateur·trice académique" ~ "Formateur·trice académique",
  
  baro$MINISTERE %in% c("Éducation nationale") & baro$METIER=="Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)" &  baro$Votre.métier.détaillé == "Infirmier·ère en EPLE" ~ "Infirmier·ère",
  
  baro$MINISTERE %in% c("Éducation nationale","Enseignement supérieur, recherche") & baro$METIER=="Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)" &  baro$Votre.métier.détaillé == "Infirmier·ère Conseiller·ère Technique" ~ "Infirmier·ère Conseiller·ère Technique",
  
  baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole" &  baro$Votre.métier.détaillé == "Directeur·trice d'EPLEFPA" ~ "Directeur·trice d'EPLEFPA",
  
  baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole" &  baro$Votre.métier.détaillé %in% c("Directeur·trice adjoint·e d'EPLEFPA chargé·e de la formation scolaire","Directeur·trice adjoint·e d'EPLEFPA chargé·e de l'apprentissage et/ou de la formation continue","Directeur·trice adjoint·e d'EPLEFPA chargé·e de l'exploitation et du développement agricole") ~ "Directeur·trice adjoint·e",
  
  baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole" &  baro$Votre.métier.détaillé == "Directeur·trice d'exploitation agricole ou d'atelier technologique" ~ "Directeur·trice d'exploitation agricole ou d'atelier technologique",
  
  baro$MINISTERE %in% c("Agriculture") & baro$METIER=="Directeur·trice (et directeur·trice adjoint·e) dans l'enseignement agricole" &  baro$Votre.métier.détaillé == "Directeur·trice de centre de formation des apprentis et/ou de centre de formation professionnelle et de la promotion agricoles (CFA/CFPPA)" ~ "Directeur·trice de centre de formation des apprentis et/ou de centre de formation professionnelle et de la promotion agricoles (CFA, CFPPA)",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Chercheur·e et Enseignant·e-chercheur·e" &  baro$Votre.métier.détaillé == "Doctorant" ~ "ATER Doctorant·e",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Chercheur·e et Enseignant·e-chercheur·e" &  baro$Votre.métier.détaillé == "Maitre de conférences" ~ "Maitre de conférences",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Chercheur·e et Enseignant·e-chercheur·e" &  baro$Votre.métier.détaillé == "Professeur·e des Universités" ~ "Professeur·e des Universités",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Chercheur·e et Enseignant·e-chercheur·e" &  baro$Votre.métier.détaillé == "Chargé·e de recherches" ~ "Chargé·e de recherches",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Chercheur·e et Enseignant·e-chercheur·e" &  baro$Votre.métier.détaillé == "Directeur·trice de recherches" ~ "Directeur·trice de recherches",
  
  baro$MINISTERE %in% c("Enseignement supérieur, recherche") & baro$METIER=="Infirmier·ère (en service scolaire, agricole ou universitaire et conseiller·ère technique)" &  baro$Votre.métier.détaillé == "Infirmier·ère dans l'enseignement supérieur" ~ "Infirmier·ère",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Animateur·trice" &  baro$Votre.métier.détaillé == "Coordinateur·trice" ~ "Coordinateur·trice",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Animateur·trice" &  baro$Votre.métier.détaillé == "Directeur·trice" ~ "Directeur·trice de structure",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER %in% c("Animateur·trice","Professeur·e de sport") &  baro$Votre.métier.détaillé == "Formateur·trice" ~ "Formateur·trice",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Chef·fe de pôle" ~ "Chef·fe de pôle",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Chef·fe de pôle adjoint·e" ~ "Chef·fe de pôle adjoint·e",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Chef·fe de bureau" ~ "Chef·fe de bureau",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Chargé·e de mission" ~ "Chargé·e de mission",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Directeur·trice" ~ "Directeur·trice",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "Directeur·trice adjoint·e" ~ "Directeur·trice adjoint·e",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Inspecteur·trice de la Jeunesse et des sports" &  baro$Votre.métier.détaillé == "CTS" ~ "Conseiller·ère technique sportif·ive (CTS)",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Professeur·e de sport" &  baro$Votre.métier.détaillé == "Conseiller·ère d'animation sportive" ~ "Conseiller·ère d'animation sportive",
  
  baro$MINISTERE %in% c("Jeunesse et Sports") & baro$METIER=="Professeur·e de sport" &  baro$Votre.métier.détaillé == "Conseiller·ère technique sportif·ve" ~ "Conseiller·ère technique sportif·ve",
  
  baro$Votre.métier.détaillé=="Autre" ~ "Autre",
  baro$Votre.métier.détaillé=="Vide" ~ "Vide",
  T ~ "Vide_créé"
)

#Test

tab_metier_detail <- addmargins(
  table(baro$METIER_DETAIL2026,useNA = "ifany")
)

# Export

write.csv2(tab_metier_detail,"Table_METIER_DETAIL.csv", fileEncoding = "UTF-8", na="", row.names = F)

# Lieu d'exercice-----

# Métier ------

baro$LIEU_EXERCICE2026 <- baro$LIEU_EXERCICE

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Administration centrale, services académiques"] <- "Services académiques"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "GRETA, GIP FCIP"] <- "CFA publics, GRETA, GIP FCIP"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Collectivité territoriale"] <- "Collectivité territoriale (CCAS, Bibliothèque, MJC, ...)"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "EPLEFPA"] <- "EPLEFPA (LEGTA, LEGTPA, LPA...)"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Établissement sanitaire, social, médico-social"] <- "Établissement sanitaire, social, médico-social (CMP, ASE, SUMPPS, IEM...)"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Autres établissements dépendant du MEN (CIO...)"] <- "Établissements autres dépendant du MEN (CIO, INSPE, EAFC...)"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Lycées d’Enseignement Général et Technologique (LEGT)"] <- "Lycées d'Enseignement Général et Technologique"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Lycées professionnels (LP)"] <- "Lycées Professionnels"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Opérateurs éducatifs (CNED, CANOPÉ, Onisep, Cereq...)"] <- "Opérateurs éducatifs (CNED, CANOPÉ, ONISEP, CEREQ...)"

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Universités, établissements d'enseignement supérieur, PRES"] <- "Universités, établissements d'enseignement supérieur, IUT, ENS, IEP, ECM..."

baro$LIEU_EXERCICE2026[baro$LIEU_EXERCICE == "Universités, établissements d'enseignement supérieur, PRES"] <- "Universités, établissements d'enseignement supérieur, IUT, ENS, IEP, ECM..."


#Test

tab_lieu_exercice2026 <- addmargins(
  table(baro$LIEU_EXERCICE2026,useNA = "ifany")
)

# Export

write.csv2(tab_lieu_exercice2026,"Table_lieu_exercice2026.csv", fileEncoding = "UTF-8", na="", row.names = F)

