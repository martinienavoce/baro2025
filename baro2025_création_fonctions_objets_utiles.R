

# Import des scripts
source("baro2025_import_rename_var.R")
source("baro2025_recodage_modalité.R")


# Fonctions pour :------

## Thème UNSA -----

### Définition des couleurs UNSA

UNSA_BLEU   <- "#003189"
UNSA_ROUGE  <- "#E30613"
UNSA_CLAIR  <- "#e8edf8"

palette_unsa <- c(
  "#003189", "#1D5DB8", "#5B8DD9", "#93B8EE",
  "#E30613", "#F05C65", "#003189", "#7a88aa"
)

### Fonction générant le thème

theme_unsa <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(colour = UNSA_BLEU, face = "bold", size = 14),
      axis.text        = element_text(colour = "#444"),
      panel.grid.major = element_line(colour = "#eef0f6"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      plot.margin      = margin(12, 12, 12, 12)
    )
}



## Graphes des QCU-----

### Tris à plat

plotS_qcu <- function(data, var) {
  tab <- data %>%
    filter(!is.na(.data[[var]])) %>% # Car les variables sont en factor ; donc possibilité d'affichage des NA qui ne sont rien d'autres que les non-concernés par la question
    count(.data[[var]]) %>%     
    mutate(Pourcentage = round(n * 100 / sum(n), 1)) %>%
    rename(Effectif = n)

  # Graphique
  g <- ggplot(tab, aes(
    x    = .data[[var]],
    y    = Effectif,
    fill = .data[[var]],
    text = paste0(
      "<b>", .data[[var]], "</b><br>",
      "Effectif : ", Effectif, "<br>",
      "Part : ", Pourcentage, "%"
    )
  )) +
    geom_bar(stat = "identity", width = 0.65) +
    geom_text(aes(label = paste0(Pourcentage, "%")),
              vjust = 0.5, colour = "white", fontface = "bold", size = 3.8) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, 25)) +   # stringr::str_wrap : pour couper le libellé et renvoyer à la ligne automatique après 25 caractères. Le mettre dans scale_x_discrete permet d'opérer directement sur l'axe et éviter de ce fait de transformer la variable de x en caractère
    labs(title = paste0("Distribution de : ", var),
         x = NULL, y = "Effectif", fill = NULL) +
    theme_bw() +
    theme(legend.position = "none")   # Supprimer la légende car inutile
    
  # longueur max des libellés des modalités
  long_lib <- max(nchar(ordre_moda_baro2025[[var]]), na.rm = TRUE) > 25 # Modalités dont les libellés sont longs (plus de 25 caractères)
    
  # nombre de modalités
  nb_modalites <- nrow(tab)
    
  if (nb_modalites > 5 || long_lib){ 
  
  g <- g + coord_flip() # Si trop de modalités ou trop caractères pour les modalités ---> Barres horizontales

  }
  
  return(g)
  
}

### Croisés

plotC_qcu <- function(data, var_exo, var_endo) {
  
  lev_q1 <- ordre_moda_baro2025[[var_endo]] # Pour le respect de l'ordre des modalités dans les tableaux et graphiques
  lev_q2 <- ordre_moda_baro2025[[var_exo]]
  
  tab_plot <- rprop(
    table(
      q2 = factor(data[[var_exo]], levels = lev_q2),   # en ligne  ----> % par ligne
      q1 = factor(data[[var_endo]], levels = lev_q1),   # en colonne
      useNA = "no"
    ),
    percent = TRUE,   # valeurs en %, pas en proportion
    digits  = 1       # arrondi directement dans rprop
  ) %>%
    as.data.frame() %>%
    filter(q1 != "Total") %>%   # suppression de la colonne "Total" ajoutée par rprop
    mutate(
      q2 = ifelse(as.character(q2) == "Total", "Ensemble", as.character(q2)),
      q1 = factor(q1, levels = lev_q1),
      q2 = factor(q2, levels = c(lev_q2, "Ensemble"))  # Mettre "Ensemble" en dernière position
    )
  
  n_mod    <- n_distinct(data[[var_exo]])
  
  g <- ggplot(tab_plot, aes(
    x    = q2,
    y    = round(Freq,1),
    fill = q1,
    text = paste0(
      "<b>", q2, "</b><br>",
      var_endo, " : ", q1, "<br>",
      "Part : ", round(Freq,1), "%"
    )
  )) +
    geom_bar(stat = "identity", 
             width = 0.65,
             position = position_stack(reverse = TRUE)) + # Pour inverser l'empilement
    geom_text(aes(label = paste0(round(Freq,1), "%")),
              position = position_stack(vjust = 0.5, reverse = TRUE),     # reverse = TRUE : pour inverser l'ordre d'affichage
              colour = "white", fontface = "bold", size = 3.5) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, 25)) +
    labs(title  = paste("Distribution de", var_endo, "selon", var_exo),
         x = NULL, y = "Proportion (%)", fill = NULL) +
    theme_bw() 
  
  long_lib <- max(nchar(ordre_moda_baro2025[[var_exo]]), na.rm = TRUE) > 25 # Nombre maxi de caractères des modalités de la variable supérieur 25
  
  if(n_distinct(data[[var_exo]]) > 5 || long_lib){ # Nombre de modalités supérieur à 5 ou nombre maxi de caractères des modalités supérieur à 25
  
  g <- g + coord_flip()  # Barre horizontale si conditions vérifiées
  
  }
  
  g <- g + theme(axis.text.y = element_text(size = 9))
  
  return(g)
  
}

## Tests (Khi2 et V de Cramér)----- 

test_dependance <- function(data, var_endo, var_exo, seuil = 5, B = 2000) {
  
  tab <- table(data[[var_exo]], data[[var_endo]])
  
  # Vérification des modalités
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(list(
      methode        = "Test impossible",
      p_value        = NA,
      v_cramer       = NA,
      interpretation = paste(
        "Le test d'indépendance ne peut pas être réalisé car",
        "au moins une des variables ne présente pas suffisamment de modalités.",
        "Il est nécessaire d'avoir au minimum deux catégories pour chaque variable."
      )
    ))
  }
  
  # Khi² classique pour récupérer les effectifs théoriques
  
  test_chi <- suppressWarnings(chisq.test(tab, correct = FALSE)) # suppressWarnings : empêche à afficher les warning
  
  expected <- test_chi$expected
  
  condition_ok <- all(expected >= seuil)

  # Khi2 classique
  
  if (condition_ok) {
    
    test <- test_chi
    
    # Calcul V de Cramér
    n <- sum(tab)
    r <- nrow(tab)
    k <- ncol(tab)
    
    v <- sqrt(test$statistic / (n * min(r - 1, k - 1)))
    
    # Interprétation V
    intensite <- case_when(
      v < 0.13 ~ "faible",
      v < 0.20 ~ "modérée",
      TRUE     ~ "forte"
    )
    
    # Interprétation p-value
    signif_txt <- case_when(
      test$p.value < 0.05  ~ "significative",
      TRUE                 ~ "non significative"
    )
    
    interpretation <- paste(
      "Le test du khi² indique que la relation entre les deux variables est",
      signif_txt, "(p-value =", round(test$p.value, 4), ").",
      "Cela signifie que les différences observées entre les groupes ont très peu de chances d'être dues au hasard.",
      "Par ailleurs, l'intensité de cette relation, mesurée par le V de Cramér, est",
      intensite, "(V =", round(v, 3), ")."
    )
    
    return(list(
      methode        = "Khi² classique",
      p_value        = round(test$p.value, 4),
      v_cramer       = round(v, 3),
      interpretation = interpretation
    ))
  }
  
  
  # Khi2 Simulé

  test_sim <- chisq.test(tab, simulate.p.value = TRUE, B = B)
  
  signif_txt <- case_when(
    test_sim$p.value < 0.05  ~ "significative",
    TRUE                     ~ "non significative"
  )
  
  interpretation <- paste(
    "Le test du khi² avec simulation (Monte Carlo) indique que la relation entre les deux variables est",
    signif_txt, "(p-value =", round(test_sim$p.value, 4), ").",
    "La simulation a été utilisée car certaines conditions du test du khi² classique",
    "n'étaient pas respectées (effectifs théoriques trop faibles).",
    "Dans ce contexte, il n'est pas possible d'interpréter la force de la relation",
    "à l'aide du V de Cramér."
  )
  
  return(list(
    methode        = paste0("Khi² simulé (B=", B, ")"),
    p_value        = round(test_sim$p.value, 4),
    v_cramer       = NA,
    interpretation = interpretation
  ))
}

## Graphes des QCM-----


## Colonnes des questions (QT et QA)----

fonction_cols_questions <- function(data){
  names(data)[       # names() : liste des noms des colonnes
    grepl(
      "^(QT|QA)",   # pattern recherché : ici, noms commençant par QT ou QA
      names(data)       # Source de la recherche : liste des colonnes de baro
    )
  ] 
}

## Colonnes des QA----

fonction_cols_qa <- function(data){
  names(data)[       
    grepl(
      "^(QA)",   
      names(data)       
    )
  ] 
}

# Quelques objets utiles----



### Mise au format long des QCM----

#### QT1-----


