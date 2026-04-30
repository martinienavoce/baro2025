library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(plotly)
library(readr)
library(shinyjs)
library(writexl)
#library(rsconnect) dans la console pour connecter l'appli à shinyapps.io
# deployApp() dans la console pour déployer l'appli

############ Import et préparation des données (une seule fois au démarrage)################ 
# Import des scripts
source("baro2025_import_rename_var.R")
source("baro2025_recodage_modalité.R")
source("baro2025_création_fonctions_objets_utiles.R")


# Filtrage des questions (QT + QA)
baro_question <- fonction_cols_questions(baro)[ # fonction créée qui renvoie la liste des noms des colonnes relatives aux questions
  !grepl("NSPP", fonction_cols_questions(baro)) # Exclure ici les questions relatives aux NSPP
] 

# Questions d’actualité
baro_qa <- names(baro)[
  which(names(baro) == "QA1_MOTIV_NSPP") :
    which(names(baro) == "QA11_INFO_COM_SYND")
]

# Questions ayant au plus 50 modalités
vars_moda50 <- names(baro)[sapply(baro, function(x) {
  length(unique(na.omit(x))) <= 50
})]

# variables ayant au plus 150 modalités
vars_moda150 <- names(baro)[sapply(baro, function(x) {
  length(unique(na.omit(x))) <= 150
})]

# Hors questions QT et QA
vars_moda150_hq <- setdiff(names(vars_moda150), names(baro_question))

# variables ayant au plus 150 modalités à l'exception de code_dep
vars_moda150 <- names(baro)[sapply(baro, function(x) {
  length(unique(na.omit(x))) <= 150
})]

# Pré-traitement unique : conversion character ---- factor
baro <- baro %>%
  mutate(across(where(is.character), as.factor))

# Cache des niveaux (évite de recalculer à chaque clic)
levels_cache <- setNames(
  lapply(names(baro), function(v) levels(baro[[v]])),
  names(baro)
) # Pour créer une liste nommée de toutes les modalités des colonnes facteurs du baro


############################## CSS personnalisé UNSA ################################

css_unsa <- "
  /* Barre de navigation */
  .navbar-brand { font-weight: 700 !important; letter-spacing: 0.3px; } 
  
  /* Sidebar */
  .sidebar-container {
    background: white;
    border-right: 1px solid #dde3f0;
    min-height: 100vh;
    padding: 0;
  }
  .sidebar-section {
    padding: 14px 16px;
    border-bottom: 1px solid #eef0f6;
  }
  .sidebar-section-title {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.8px;
    color: #003189;
    text-transform: uppercase;
    margin-bottom: 10px;
  }
  
  /* Bouton principal */
  #bouton_graph {
    background: #E30613 !important;
    border-color: #E30613 !important;
    color: white !important;
    font-weight: 600;
    width: 100%;
    padding: 10px;
    border-radius: 8px;
    font-size: 14px;
    transition: opacity .2s;
  }
  #bouton_graph:hover { opacity: 0.88; }
  
  /* Boutons de téléchargement */
  .dl-btn {
    border: 1px solid #c8d0e8 !important;
    background: white !important;
    color: #003189 !important;
    font-size: 12px !important;
    border-radius: 7px !important;
    padding: 6px 10px !important;
    flex: 1;
  }
  .dl-btn:hover { background: #e8edf8 !important; }
  
  /* Cartes des KPI */
  .kpi-row {
    display: flex;
    gap: 12px;
    padding: 14px 20px;
    background: white;
    border-bottom: 1px solid #dde3f0;
    flex-wrap: wrap;
  }
  .kpi-card {
    background: #f4f6fb;
    border-radius: 8px;
    padding: 10px 16px;
    min-width: 130px;
    flex: 1;
  }
  .kpi-card.danger { background: #fff3f3; border: 0.5px solid #f5bebe; }
  .kpi-label { font-size: 11px; color: #7a88aa; margin-bottom: 2px; }
  .kpi-value { font-size: 20px; font-weight: 700; color: #1a2540; }
  .kpi-bar-mini {
    height: 3px; background: #E30613; width: 60%;
    border-radius: 2px; margin-top: 5px;
  }
  
    /* Panneau principal */
  .main-panel-inner { padding: 20px; }
  
  /* Scrollable chart */
  .chart-scroll { overflow-y: auto; max-height: 680px; }
  
  /* Section titres */
  .section-title {
    font-size: 13px; font-weight: 600; color: #1a2540; margin-bottom: 12px;
    display: flex; align-items: center; gap: 8px;
  }
  .section-badge {
    font-size: 10px; color: #003189; background: #e8edf8;
    padding: 3px 8px; border-radius: 12px;
  }
  
  /* Table DT */
  table.dataTable thead th {
    background: #003189 !important;
    color: white !important;
    font-weight: 500 !important;
    border: none !important;
  }
  table.dataTable tbody tr:last-child td {
    font-weight: 700;
    background: #e8edf8 !important;
    color: #003189 !important;
  }
  table.dataTable { font-size: 13px; }
  
  /* Filtres */
  /* Carte individuelle d'un filtre */
  .filtre-carte {
    background: #f7f9ff;
    border: 1px solid #dde3f0;
    border-radius: 8px;
    padding: 10px 12px 6px 12px;
    margin-bottom: 8px;
    position: relative;
  }
 
  /* En-tête de la carte : numéro + bouton supprimer */
  .filtre-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 6px;
  }
  .filtre-num {
    font-size: 10px;
    font-weight: 700;
    color: #003189;
    text-transform: uppercase;
    letter-spacing: 0.6px;
  }
 
  /* Bouton ✕ supprimer un filtre */
  .btn-suppr-filtre {
    background: none !important;
    border: none !important;
    color: #a32d2d !important;
    font-size: 15px !important;
    line-height: 1 !important;
    padding: 0 2px !important;
    cursor: pointer;
    opacity: 0.7;
    transition: opacity .15s;
  }
  .btn-suppr-filtre:hover { opacity: 1; }
 
  /* Bouton + Ajouter un filtre */
  #ajout_filtre {
    border: 1px dashed #003189 !important;
    color: #003189 !important;
    background: transparent !important;
    font-size: 12px !important;
    width: 100%;
    margin-top: 4px;
    border-radius: 7px !important;
  }
  #ajout_filtre:hover { background: #e8edf8 !important; }
 
  /* Bouton Tout effacer */
  #effacer_filtres {
    border: 1px solid #f5bebe !important;
    color: #a32d2d !important;
    background: #fff3f3 !important;
    font-size: 11px !important;
    width: 100%;
    margin-top: 6px;
    border-radius: 7px !important;
  }
  #effacer_filtres:hover { background: #fde8e8 !important; }
 
  /* Compteur de filtres actifs sous le titre */
  .filtre-compteur {
    font-size: 10px;
    color: #7a88aa;
    margin-bottom: 8px;
  }
  .filtre-compteur span {
    font-weight: 700;
    color: #003189;
  }
"
  

############################## UI #############################
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version  = 5,
    bg       = "#f4f6fb", # Couleur de fond principale de l'appli
    fg       = "#1a2540", # Couleur principale de texte dans l'appli
    primary  = UNSA_BLEU, # Couleur principale de l'appli (boutons principaux, liens, tab,...)
    secondary = "#5B8DD9",
    danger   = UNSA_ROUGE, # Couleur utilisée pour mes éléments critiques
    base_font    = font_google("Inter"), # Police principale du texte (paragraphes, labels, ...)
    heading_font = font_google("Montserrat", wght = 700) # Police des titres
  ),
  
  
  tags$head(tags$style(HTML(css_unsa))), # Pour ajouter le CSS contenu dans css_unsa dans l'en-tête HTML de l'appli
  
  
  # Barre de navigation
  tags$nav(
    class = "navbar",
    style = paste0(
      "background:", UNSA_BLEU, ";",
      "border-bottom: 3px solid ", UNSA_ROUGE, ";",
      "padding: 0 20px; height: 58px;",
      "display: flex; align-items: center; gap: 14px;"
    ),
    tags$img(src = "logo-unsa.png", height = "42px"), # Insertion du logo
    tags$div(
      tags$span("Baromètre des métiers de l'éducation", # Texte à insérer dans la barre de navigation
                style = "color:white; font-size:17px; font-weight:700;"),
      tags$br(),
      tags$span("UNSA-Éducation · Outil d'analyse des données", # Texte à insérer dans la barre de navigation
                style = "color:rgba(255,255,255,0.65); font-size:11px;")
    ),
    tags$div(
      style = "margin-left:auto;",
      tags$span("2025",
                style = paste0(
                  "background:", UNSA_ROUGE, ";",
                  "color:white; font-size:11px; font-weight:700;",
                  "padding:4px 12px; border-radius:20px;"
                ))
    )
  ),
  
  # Barre KPI
  div(
    class = "kpi-row",
    div(class = "kpi-card",
        div(class = "kpi-label", "Répondants analysés"), # Champ relatif au nombre de répondants
        div(class = "kpi-value", textOutput("kpi_n", inline = TRUE)), # valeur de nombre de répondants
        div(class = "kpi-bar-mini")
    ),
    div(class = "kpi-card",
        div(class = "kpi-label", "Question affichée"),
        div(style = "font-size:13px; font-weight:600; color:#1a2540; margin-top:3px;",
            textOutput("kpi_question", inline = TRUE))
    ),
    div(class = "kpi-card",
        div(class = "kpi-label", "Filtres actifs"),
        div(class = "kpi-value", textOutput("kpi_filtres", inline = TRUE))
    ),
    div(class = "kpi-card danger",
        div(class = "kpi-label", style = "color:#a32d2d;", "Non-réponse (NSPP)"),
        div(class = "kpi-value", style = "color:#E30613;",
            textOutput("kpi_nspp", inline = TRUE))
    )
  ),
  
  # Layout principal
  sidebarLayout(
    
  # Sidebar 
    sidebarPanel(
      width = 3,
      style = "padding:0; background:white; border-right:1px solid #dde3f0;",
      
      div(class = "sidebar-section",
          div(class = "sidebar-section-title", "Question principale"),
          selectInput("q1", NULL, choices = vars_moda150, width = "100%")
      ),
      
      div(class = "sidebar-section",
          div(class = "sidebar-section-title", "Croisement"),
          radioButtons("croiser", NULL,
                       choices = c("Sans croisement" = "Non", "Avec croisement" = "Oui"),
                       inline = FALSE), # "Sans croisement"/"Avec croisement" seront les textes affichés et "Non"/"Oui" seront les valeurs correspondantes utilisées dans input ; inline = FALSE : bouton vertical, True pour horizontal
          conditionalPanel(
            condition = "input.croiser == 'Oui'",
            selectInput("q2", "Variable de croisement :", choices = vars_moda50, width = "100%")
          )
      ),
      
      div(class = "sidebar-section",
          div(class = "sidebar-section-title", "Filtres"),
          # Compteur dynamique des filtres actifs
          uiOutput("filtre_compteur_ui"),
          
          # Cartes des filtres (persistantes, modifiables)
          uiOutput("filtres_ui"),
          
          # Bouton ajout
          actionButton("ajout_filtre", "+ Ajouter un filtre", width = "100%"),
          
          # Bouton tout effacer (visible seulement s'il y a au moins 1 filtre)
          conditionalPanel(
            condition = "output.nb_filtres_actifs > 0",
            actionButton("effacer_filtres", "✕  Tout effacer", width = "100%")
          )
      ),
      
      div(style = "padding:14px 16px 6px;",
          actionButton("bouton_graph", "Visualiser", icon = icon("chart-bar"), width = "100%")
      ),
      
      div(style = "display:flex; gap:8px; padding:8px 16px 16px;",
          downloadButton("dl_graph",  "Graphique (.png)", class = "dl-btn"),
          downloadButton("dl_table", "Tableau (.xlsx)",  class = "dl-btn")
      )
    ),
    

# Panneau principal

mainPanel(
  width = 9,
  div(class = "main-panel-inner",
      
      div(class = "section-title",
          "Résultats",
          span(class = "section-badge", textOutput("badge_type", inline = TRUE))
      ),
      
      div(class = "chart-scroll",
          plotlyOutput("plot", height = "auto")
      ),
      
      br(),
      
      DTOutput("table")
  )
)
  )
)

########################### SERVER #################################

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    plot = NULL,
    table = NULL,
    n = 0, 
    nspp = NA
  ) # création d'un objet réactif contenant 3 champs : plot, table, n
  
  # État des filtres
  # On travaille avec une liste d'IDs uniques plutôt qu'un simple compteur.
  # Chaque ID est un entier incrémental ; supprimer un filtre retire son ID
  # de la liste sans décaler les autres → les sélections survivent.
  rv_filters <- reactiveValues(
    ids      = integer(0),  # vecteur des IDs actifs, ex. c(1, 3, 5)
    compteur = 0L           # compteur global, ne redescend jamais
  )
  
  # Exposer le nombre de filtres actifs vers le JS (pour conditionalPanel)
  output$nb_filtres_actifs <- reactive({ length(rv_filters$ids) })
  outputOptions(output, "nb_filtres_actifs", suspendWhenHidden = FALSE)
  
  # Ajouter un filtre 
  observeEvent(input$ajout_filtre, {
    rv_filters$compteur <- rv_filters$compteur + 1L
    rv_filters$ids      <- c(rv_filters$ids, rv_filters$compteur)
  })
  
  # Tout effacer 
  observeEvent(input$effacer_filtres, {
    rv_filters$ids <- integer(0)
  })
  
  # Supprimer un filtre individuel 
  # On observe dynamiquement le bouton ✕ de chaque carte
  observe({
    lapply(rv_filters$ids, function(id) {
      btn_id <- paste0("suppr_", id)
      observeEvent(input[[btn_id]], {
        rv_filters$ids <- setdiff(rv_filters$ids, id)
      }, ignoreInit = TRUE, once = TRUE)
      # once = TRUE : évite les observateurs zombies après suppression
    })
  })
  
  # Rendu des cartes filtres 
  output$filtres_ui <- renderUI({
    ids <- rv_filters$ids
    if (length(ids) == 0) return(NULL)
    
    lapply(seq_along(ids), function(pos) {
      id <- ids[pos]
      
      div(
        class = "filtre-carte",
        
        # En-tête : numéro + bouton supprimer
        div(class = "filtre-header",
            span(class = "filtre-num", paste("Filtre", pos)),
            actionButton(
              inputId = paste0("suppr_", id),
              label   = "✕",
              class   = "btn-suppr-filtre"
            )
        ),
        
        # Sélection de la variable
        selectInput(
          inputId  = paste0("var_", id),
          label    = NULL,
          choices  = setNames(vars_moda150, vars_moda150),
          selected = isolate(input[[paste0("var_", id)]]),  # conserve la sélection
          width    = "100%"
        ),
        
        # Sélection de la modalité (peuplée dynamiquement)
        selectInput(
          inputId  = paste0("val_", id),
          label    = NULL,
          choices  = isolate({                              # conserve les choix/sélection
            var_sel <- input[[paste0("var_", id)]]
            if (!is.null(var_sel) && var_sel %in% names(levels_cache)) {
              c("Tous", levels_cache[[var_sel]])
            } else {
              c("Tous")
            }
          }),
          selected = isolate(input[[paste0("val_", id)]]),
          width    = "100%"
        )
      )
    })
  })
  
  #  Mise à jour des modalités quand la variable change 
  # Observer créé une seule fois par ID ; survit aux re-renders de filtres_ui
  observeEvent(rv_filters$ids, {
    lapply(rv_filters$ids, function(id) {
      var_input_id <- paste0("var_", id)
      val_input_id <- paste0("val_", id)
      
      observeEvent(input[[var_input_id]], {
        var     <- input[[var_input_id]]
        req(var, var %in% names(levels_cache))
        valeurs <- levels_cache[[var]]
        valeurs <- valeurs[!is.na(valeurs)]
        updateSelectInput(session, val_input_id, choices = c("Tous", valeurs),
                          selected = "Tous")
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  
  # Compteur affiché dans la sidebar
  output$filtre_compteur_ui <- renderUI({
    n <- length(rv_filters$ids)
    if (n == 0) return(NULL)
    div(class = "filtre-compteur",
        tags$span(as.character(n)),
        if (n == 1) " filtre actif" else " filtres actifs"
    )
  })
  
  # Données filtrées (reactive partagé)
  df_filtre <- reactive({
    input$bouton_graph  # dépendance explicite
    isolate({      # Pour empêcher la ré-exécution automatique sur tous les inputs, ne s'actualise que sur le bouton
      
      df <- baro   # On commence avec la dataframe complète
      
      # Filtre QA
      if (input$q1 %in% baro_qa || isTRUE(input$q2 %in% baro_qa)) {
        df <- df %>% filter(.data[["Q..tradi....Encore.quelques.questions.pour.mieux.exprimer.vos.attentes..vos.préoccupations.."]] == "Oui")
      }
      
      # Filtres dynamiques : on parcourt uniquement les IDs actifs
      ids <- rv_filters$ids
      if (length(ids) > 0) {
        for (id in ids) {
          var <- input[[paste0("var_", id)]]
          val <- input[[paste0("val_", id)]]
          if (!is.null(var) && !is.null(val) && val != "Tous") {
            df <- df %>% filter(.data[[var]] == val)
          }
        }
      }
      df
    })
  })
  

  # KPI 
  
  output$kpi_n        <- renderText({ format(rv$n, big.mark = " ") })  # Nombre d'enregistrement (répondants) filtrés (voir la rubrique "calcul principal" qui suit)
  output$kpi_question <- renderText({ req(input$q1); input$q1 }) # Afficher la question principale dans la sortie
  output$kpi_filtres  <- renderText({ as.character(length(rv_filters$ids)) })
  output$kpi_nspp     <- renderText({
    req(rv$nspp); paste0(rv$nspp, "%")
  })
  output$badge_type   <- renderText({
    if (input$croiser == "Non") "Tri à plat" else "Tri croisé"
  })
  
  
  # Calcul principal
  observeEvent(input$bouton_graph, {
    req(input$q1)
    
    withProgress(message = "Calcul en cours…", value = 0.3, {   # Création d'une boîte de dialogue de progression pendant que le bloc de code s’exécute ; 30% rempli au départ
      
      df <- df_filtre()
      rv$n <- nrow(df)
      
      # Nb modalités et longueur des labels (réutilisés)
      flip_needed <- function(nom_var) {
        long_lab <- max(nchar(levels_cache[[nom_var]]), na.rm = TRUE) > 25 # Nombre maxi de caractères des modalités de la variable supérieur 25
        n_distinct(df[[nom_var]]) > 5 || long_lab # Nombre de modalités supérieur à 5 ou nombre maxi de caractères des modalités supérieur à 25
      }
      
      
    ################ Tri à plat ##############################
      if (input$croiser == "Non") {
        
        tab1 <- df %>%
          count(.data[[input$q1]]) %>%  
          mutate(Pourcentage = round(n * 100 / sum(n), 1)) %>%
          rename(Effectif = n)
        
        # NSPP
        nspp_row <- tab1 %>% filter(grepl("NSPP", as.character(.data[[input$q1]]), ignore.case = TRUE))
        rv$nspp  <- if (nrow(nspp_row) > 0) nspp_row$Pourcentage[1] else 0
        
        # Ligne Ensemble
        ligne_ens <- tibble(            # tibble pour avoir en dataframe
          !!input$q1 := "Ensemble",       # !! : pour évaluer le q1 entrée
          Effectif   = sum(tab1$Effectif),
          Pourcentage = round(sum(tab1$Pourcentage),0)
        )
        rv$table <- bind_rows(tab1, ligne_ens) # Rendre la table incluant la ligne "Ensemble"
        
        # Hauteur dynamique
        n_mod       <- n_distinct(df[[input$q1]]) # Nb de modalités de q1
        height_plot <- max(400, n_mod * 38) # Hauteur graphique égal au maximum entre 400 et n_mod*38
        
        # Graphique
        g <- ggplot(tab1, aes(
          x    = .data[[input$q1]],
          y    = Effectif,
          fill = .data[[input$q1]],
          text = paste0(
            "<b>", .data[[input$q1]], "</b><br>",
            "Effectif : ", Effectif, "<br>",
            "Part : ", Pourcentage, "%"
          )
        )) +
          geom_bar(stat = "identity", width = 0.65) +
          geom_text(aes(label = paste0(Pourcentage, "%")),
                    vjust = 0.5, colour = "white", fontface = "bold", size = 3.8) +
          scale_fill_manual(values = colorRampPalette(c(UNSA_BLEU, "#93B8EE"))(n_mod)) +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, 25)) +   # stringr::str_wrap : pour couper le libellé et renvoyer à la ligne automatique après 25 caractères. Le mettre dans scale_x_discrete permet d'opérer directement sur l'axe et éviter de ce fait de transformer la variable de x en caractère
          labs(title = paste0("Distribution de : ", input$q1),
               x = NULL, y = "Effectif", fill = NULL) +
          theme_unsa() +
          theme(legend.position = "none")   # Supprimer la légende car inutile
        
        if (flip_needed(input$q1)) g <- g + coord_flip() # Si trop de modalités ou trop caractères pour les modalités ---> Barres horizontales
        
        setProgress(0.85) # Mise à jour de la progression
        rv$plot <- ggplotly(g, tooltip = "text") %>% # tooltip = "text" : Pour contrôler ce qui s'affiche au survol du graphique, ici le contenu de text fixé plus haut
          layout(height = height_plot, legend = list(orientation = "h")) # height = height_plot: Pour attribuer la hauteur calculée au graphique dans plotly
      
        
        
      } else {
        
        
  ##############################Tri croisé ########################################
        req(input$q2)
        
        lev_q1 <- levels_cache[[input$q1]] # Pour le respect de l'ordre des modalités dans les tableaux et graphiques
        lev_q2 <- levels_cache[[input$q2]]
        
        # Tableau effectifs
        tab_raw <- addmargins(
          table(
            factor(df[[input$q2]], levels = lev_q2),
            factor(df[[input$q1]], levels = lev_q1),
            useNA = "no"
          )
        )
        tab_df <- as.data.frame.matrix(tab_raw) %>%
          tibble::rownames_to_column(var = input$q2) # index devient colonne
        colnames(tab_df)[colnames(tab_df) == "Sum"] <- "Total"
        tab_df[[input$q2]][tab_df[[input$q2]] == "Sum"] <- "Ensemble"
        rv$table <- tab_df
        
        # Profils-ligne via rprop (questionr) :
        # On place q2 (variable de croisement) en LIGNE ----> chaque modalité de q2
        # totalise 100 %, ce qui permet de comparer la distribution de q1 selon q2.
        tab_plot <- questionr::rprop(
          table(
            q2 = factor(df[[input$q2]], levels = lev_q2),   # en ligne  ----> % par ligne
            q1 = factor(df[[input$q1]], levels = lev_q1),   # en colonne
            useNA = "no"
          ),
          percent = TRUE,   # valeurs en %, pas en proportion
          digits  = 1       # arrondi directement dans rprop
        ) %>%
          as.data.frame() %>%
          filter(q1 != "Total") %>%   # rprop ajoute "Total" sur la dimension lignes (q2)
          mutate(
            q2 = ifelse(as.character(q2) == "Total", "Ensemble", as.character(q2)),
            q1 = factor(q1, levels = lev_q1),
            q2 = factor(q2, levels = c(lev_q2, "Ensemble"))  # "Ensemble" = colonne de marge
          )
        
        rv$nspp <- 0
        n_mod    <- n_distinct(df[[input$q2]])
        height_plot <- max(500, n_mod * 55)
        
        g <- ggplot(tab_plot, aes(
          x    = q2,
          y    = round(Freq,1),
          fill = q1,
          text = paste0(
            "<b>", q2, "</b><br>",
            input$q1, " : ", q1, "<br>",
            "Part : ", round(Freq,1), "%"
          )
        )) +
          geom_bar(stat = "identity", 
                   width = 0.65,
                   position = position_stack(reverse = TRUE)) + # Pour inverser l'empilement
          geom_text(aes(label = paste0(round(Freq,1), "%")),
                    position = position_stack(vjust = 0.5, reverse = TRUE),     # reverse = TRUE : pour inverser l'ordre d'affichage
                    colour = "white", fontface = "bold", size = 3.5) +
          scale_fill_manual(
            values = colorRampPalette(c(UNSA_BLEU, UNSA_ROUGE))(length(lev_q1))
          ) +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, 25)) +
          labs(title  = paste("Distribution de", input$q1, "selon", input$q2),
               x = NULL, y = "Proportion (%)", fill = NULL) +
          theme_unsa()
        
        if (flip_needed(input$q2)) g <- g + coord_flip()  # Barre horizontale si conditions vérifiées
        
        
        g <- g + theme(axis.text.y = element_text(size = 9))
        
        setProgress(0.85)
        rv$plot <- ggplotly(g, tooltip = "text") %>%
          layout(height = height_plot, 
                 legend = list(orientation = "h"),
        yaxis  = list(automargin = TRUE)) # Pour élargir automatiquement la marge pour que les labels ne soient pas coupés ni superposés
      }
    })  # fin withProgress
    
    # Activer l'overlay blanc si nécessaire
    runjs("document.querySelectorAll('.chart-scroll').forEach(el => el.scrollTop = 0);")
  })
  
  # Sorties
  output$plot <- renderPlotly({
    req(rv$plot)
    rv$plot
  }) # Graphique
  
  output$table <- renderDT({
    req(rv$table)
    datatable(
      rv$table,
      rownames  = FALSE,
      options   = list(
        pageLength = 20,
        dom        = "tp",
        language   = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/French.json")
      ),
      class = "display nowrap"
    )
  }) # Table
  
  # Téléchargements
  output$dl_graph <- downloadHandler(
    filename = function() paste0("baro_", input$q1, "_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$plot)
      # Utilise plotly::save_image (nécessite kaleido ou orca)
      tryCatch(
        plotly::save_image(rv$plot, file = file, format = "png", width = 1200, height = 700),
        error = function(e) {
          showNotification(
            "Installez le package 'kaleido' pour l'export PNG : reticulate::py_install('kaleido')",
            type = "warning", duration = 6
          )
        }
      )
    }
  )
  
  output$dl_table <- downloadHandler(
    filename = function() paste0("baro_", input$q1, "_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(rv$table)
      writexl::write_xlsx(as.data.frame(rv$table), file)
    }
  )
}

############################### Lancement ######################################

shinyApp(ui = ui, server = server)
      
  

