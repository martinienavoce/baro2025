library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(plotly)
library(readr)
library(shinyjs)

# Import des scripts
source("baro_import_rec_var.R")
source("baro25_recodage_modalité.R")

### Filtrage des questions (QT + QA)
baro_question <- baro[, 
                      which(names(baro) == "QT1_TRAVAIL_NSPP") :
                        which(names(baro) == "QA11_INFO_COM_SYND")
]

### Questions d’actualité
baro_qa <- names(baro)[
  which(names(baro) == "QA1_MOTIV_NSPP") :
    which(names(baro) == "QA11_INFO_COM_SYND")
]

### Questions ayant au plus 50 modalités
vars_moda50 <- names(baro)[sapply(baro, function(x) {
  length(unique(na.omit(x))) <= 50
})]

### variables ayant au plus 150 modalités
vars_moda150 <- names(baro)[sapply(baro, function(x) {
  length(unique(na.omit(x))) <= 150
})]

### Hors questions QT et QA
vars_moda150_hq <- setdiff(names(vars_moda150), names(baro_question))

############################### UI #################################
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  titlePanel("Baromètre des métiers 2025"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Question principale"),
      selectInput("q1", "Choisir une question :", 
                  choices = names(baro_question)),
      
      hr(),
      
      radioButtons(
        "croiser",
        "Souhaitez-vous croiser cette question avec une autre variable ?",
        choices = c("Non", "Oui")
      ),
      
      conditionalPanel(
        condition = "input.croiser == 'Oui'",
        selectInput("q2", "Variable de croisement :", 
                    choices = vars_moda50)
      ),
      
      hr(),
      
################################### Filtres #########################
      h4("Filtres"),
      
      radioButtons(
        "filtrer",
        "Souhaitez-vous effectuer un filtre ?",
        choices = c("Non", "Oui")
      ),
      
      conditionalPanel(
        condition = "input.filtrer == 'Oui'",
        
        actionButton("ajout_filtre", "Ajouter un filtre :"),
        br(), br(),
        
        uiOutput("filtres_ui")
      ),
      
      br(),
      
      actionButton("bouton_graph", "Visualiser le graphique")
    ),
    
mainPanel(
  
  tags$head(
    tags$style(HTML("
    
    .main-bg {
      position: relative;
      height: 100%;
      min-height: 600px;
      background-image: url('logo-unsa.png');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
    }
    
    .overlay {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color: rgba(255, 255, 255, 0); /* transparent au départ */
      transition: background-color 0.5s ease;
      z-index: 1;
    }
    
    .overlay.active {
      background-color: rgba(255, 255, 255, 0.85); /* assombrissement */
    }
    
    .content {
      position: relative;
      z-index: 2;
      padding: 20px;
    }
    
    "))
  ),
  
  div(
    class = "main-bg",
    
    div(id = "overlay", class = "overlay"),
    
    div(
      class = "content",
      plotlyOutput("plot"),
      br(),
      DTOutput("table")
    )
  )
)
  )
)

########################### SERVER #################################
server <- function(input, output, session) {
  
  rv <- reactiveValues() # création du 1er objet réactif
  rv_filters <- reactiveValues(n = 0) # création du 2ème objet réactif correspondant au nombre de filtres
  
  observe({
    req(rv$plot) 
    
    runjs("document.getElementById('overlay').classList.add('active');")
  })
  
  # Ajouter filtre
  observeEvent(input$ajout_filtre, {
    rv_filters$n <- rv_filters$n + 1 # le nombre de filtres augmente de 1 chaque fois qu'on clique sur "Ajouter un filtre"
  })
  
  # UI dynamique des filtres
  output$filtres_ui <- renderUI({
    
    req(rv_filters$n > 0) # req() : condition de sécurité avant de permettre la saisie des filtres : il faut d'abord cliqué au moins une fois sur "Ajouter un filtre"
    
    lapply(seq_len(rv_filters$n), function(i) {
      
      tagList(
        selectInput(
          paste0("var_", i),
          label = "Nom de la variable à filtrer :",
          choices = vars_moda150
        ),
        
        selectInput(
          paste0("val_", i),
          label = "Nom de la modalité :",
          choices = NULL
        ),
        
        hr()
      )
    })
  })
  
  # Mise à jour des modalités 
  observeEvent(rv_filters$n, {
    
    i <- rv_filters$n
    
    observeEvent(input[[paste0("var_", i)]], {
      
      var <- input[[paste0("var_", i)]]
      req(var)
      
      valeurs <- levels(baro[[var]])
      valeurs <- valeurs[!is.na(valeurs)]
      
      updateSelectInput(
        session,
        inputId = paste0("val_", i),
        choices = c("Tous", valeurs)
      )
      
    }, ignoreInit = TRUE)
    
  })
  
 
############################ Analyse ###################################
  observeEvent(input$bouton_graph, {
    req(input$q1)
    df <- baro
    
    # Filtrage des individus ayant répondu aux questions d'actualité
    if (
      input$q1 %in% baro_qa ||
      input$q2 %in% baro_qa
    ) {
      df <- df %>%
        filter(Q..tradi....Encore.quelques.questions.pour.mieux.exprimer.vos.attentes..vos.préoccupations.. == "Oui")
    }
    
    # Filtres dynamiques
    if (input$filtrer == "Oui" && rv_filters$n > 0) {
      
      for (i in seq_len(rv_filters$n)) {
        
        var <- input[[paste0("var_", i)]]
        val <- input[[paste0("val_", i)]]
        
        if (!is.null(var) && !is.null(val) && val != "Tous") {
          df <- df %>% filter(.data[[var]] == val)
        }
      }
    }
    
    
    
############################## Tri à plat ################################
    if (input$croiser == "Non") {
      
      ##Tous les "NSPP" en dernière position (ils étaient au début dans le fichier source) 
      df <- df %>%
        mutate(across(where(is.factor), ~ fct_relevel(., "NSPP", after = Inf))) # after = 0 : pour mettre en début
      
      tab <- df %>% 
        count(.data[[input$q1]]) %>% 
        mutate(Prop = round(n*100/sum(n),1))
      
      graph <- ggplot(tab, aes(x = .data[[input$q1]], y = n, fill = .data[[input$q1]])) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(Prop,"%")), vjust=-0.5) +
        labs(
          title = paste0("Distribution de ", input$q1),
          x = input$q1,
          y = "Effectif",
          fill = input$q1
        ) +
        scale_fill_viridis_d(option = "C") +
        theme_minimal()
      
      rv$plot <- ggplotly(graph)
      rv$table <- tab
      
    } else {
      
############################ Tri croisé ##############################
      
      lev_q1 <- levels(baro[[input$q1]])
      lev_q2 <- levels(baro[[input$q2]]) # Pour le respect des ordres des modalités
      
      # Table brute (tableau de contingence en effectif)
      
      tab <- addmargins(
        table(
          factor(df[[input$q2]], levels = lev_q2),
          factor(df[[input$q1]], levels = lev_q1),
          useNA = "ifany"
        )
      )
      
      # Convertir en data.frame pour Shiny
      tab_final <- as.data.frame.matrix(tab)
      
      # =========================
      # GRAPH (r(prop(q2, q1)))
      # =========================
      
      tab_plot <- df %>%
        mutate(
          q1 = factor(.data[[input$q1]], levels = lev_q1),
          q2 = factor(.data[[input$q2]], levels = lev_q2)
        ) %>% # Forcer le respect des ordres des modalités des variables q1 et q2
        count(q1, q2) %>%
        group_by(q2) %>%
        mutate(prop = round(n*100/sum(n),1))
      
      # Nombre de modalités de q2
      flip <- n_distinct(df[[input$q2]]) > 5
      
      # Graphique
      if (!flip) {
        
        graph <- ggplot(tab_plot, aes(
          x=q2, 
          y=prop, 
          fill=q1)) +
          geom_bar(stat="identity") +
          labs(
            title = paste("Distribution de", input$q1, "selon", input$q2),
            x = input$q2,
            y = "Proportion (%)",
            fill = input$q1
          ) +
          scale_fill_viridis_d(option = "C") +
          theme_minimal()

      } else {
        
        graph <- ggplot(tab_plot, aes(
          x=q2, 
          y=prop, 
          fill=q1)) +
          geom_bar(stat="identity") +
          coord_flip() +
          labs(
            title = paste("Distribution de", input$q1, "selon", input$q2),
            x = input$q1,
            y = "Proportion (%)",
            fill = input$q2
          ) +
          scale_fill_viridis_d(option = "C") +
          theme_minimal()
        
        }
      
      rv$plot <- ggplotly(graph)
      rv$table <- tab_final
    }
  
  output$plot <- renderPlotly({
    req(rv$plot)
    rv$plot
  })
  
  output$table <- renderDT({
    req(rv$table)
    rv$table
  })
  
  })
  
  }



############################# APP ##################################

shinyApp(ui = ui, server = server)

