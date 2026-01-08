# --- Librairies ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)

# --- 1. Importation et Préparation des données ---
# Utilisation de vos chemins d'accès exacts
import_data <- function(path) {
  df <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, na.strings = "")
  
  # Nettoyage immédiat
  df <- df %>%
    mutate(
      # Nettoyage de la date (on garde les 10 premiers caractères pour as.Date)
      date_mesure = as.Date(substr(date_mesure, 1, 10)),
      annee = year(date_mesure),
      # On s'assure que la valeur est numérique
      valeur = as.numeric(valeur)
    ) %>%
    # Suppression de la colonne filiere comme demandé
    select(-filiere)
  
  return(df)
}

# Chargement des fichiers
heat_reg  <- import_data("Volume de chaleur/volume_de_chaleur_region.csv")
heat_dept <- import_data("Volume de chaleur/volume_de_chaleur_departement.csv")
heat_com  <- import_data("Volume de chaleur/volume_de_chaleur_commune.csv")

# --- 2. Interface Utilisateur (UI) ---
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Analyse Chaleur CGDD"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Explorateur de données", tabName = "data", icon = icon("database")),
      hr(),
      # Sélecteurs
      selectInput("selected_year", "Choisir l'année :", 
                  choices = sort(unique(heat_reg$annee), decreasing = TRUE)),
      
      radioButtons("geo_level", "Échelle territoriale :",
                   choices = c("Régionale" = "reg", 
                               "Départementale" = "dept", 
                               "Communale" = "com"),
                   selected = "reg")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet Principal
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_val", width = 6),
                valueBoxOutput("top_territory", width = 6)
              ),
              fluidRow(
                box(title = "Top 15 des territoires", status = "success", solidHeader = TRUE,
                    plotlyOutput("bar_plot"), width = 8),
                box(title = "Part du volume", status = "success", solidHeader = TRUE,
                    plotlyOutput("pie_plot"), width = 4)
              ),
              fluidRow(
                box(title = "Évolution historique du volume total", status = "info", 
                    solidHeader = TRUE, plotlyOutput("trend_plot"), width = 12)
              )
      ),
      # Onglet Données
      tabItem(tabName = "data",
              box(title = "Recherche et filtrage avancé", width = 12,
                  DTOutput("table_raw"))
      )
    )
  )
)

# --- 3. Logique Serveur ---
server <- function(input, output) {
  
  # Réactivité : Sélection du dataset selon le bouton radio
  reactive_df <- reactive({
    if (input$geo_level == "reg") return(heat_reg)
    if (input$geo_level == "dept") return(heat_dept)
    return(heat_com)
  })
  
  # Réactivité : Filtrage par année
  filtered_df <- reactive({
    reactive_df() %>% filter(annee == input$selected_year)
  })
  
  # KPI 1 : Volume Total
  output$total_val <- renderValueBox({
    v <- sum(filtered_df()$valeur, na.rm = TRUE)
    valueBox(paste0(round(v, 2), " GWh"), "Volume Total Consommé", 
             icon = icon("fire"), color = "red")
  })
  
  # KPI 2 : Territoire leader
  output$top_territory <- renderValueBox({
    col_name <- names(filtered_df())[3] # Récupère le libellé (reg, dept ou com)
    top_1 <- filtered_df() %>% arrange(desc(valeur)) %>% slice(1)
    valueBox(top_1[[col_name]], "Premier contributeur", 
             icon = icon("trophy"), color = "yellow")
  })
  
  # Graphique : Barres (Ranking)
  output$bar_plot <- renderPlotly({
    col_label <- names(filtered_df())[3]
    
    p <- filtered_df() %>%
      arrange(desc(valeur)) %>%
      head(15) %>%
      ggplot(aes(x = reorder(!!sym(col_label), valeur), y = valeur, fill = valeur)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "mako") +
      theme_minimal() +
      labs(x = NULL, y = "Volume (GWh)")
    
    ggplotly(p)
  })
  
  # Graphique : Evolution temporelle
  output$trend_plot <- renderPlotly({
    # On prend la tendance globale du dataset sélectionné
    trend <- reactive_df() %>%
      group_by(annee) %>%
      summarise(total = sum(valeur, na.rm = TRUE))
    
    p <- ggplot(trend, aes(x = annee, y = total)) +
      geom_line(color = "#27ae60", size = 1) +
      geom_point(color = "#27ae60") +
      theme_minimal() +
      labs(x = "Année", y = "Volume global")
    
    ggplotly(p)
  })
  
  # Graphique : Camembert (Pie)
  output$pie_plot <- renderPlotly({
    col_label <- names(filtered_df())[3]
    # On groupe les petits territoires en "Autres" pour la lisibilité
    data_pie <- filtered_df() %>%
      arrange(desc(valeur)) %>%
      mutate(label = ifelse(row_number() <= 5, !!sym(col_label), "Autres")) %>%
      group_by(label) %>%
      summarise(valeur = sum(valeur))
    
    plot_ly(data_pie, labels = ~label, values = ~valeur, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial')
  })
  
  # Table de données interactive
  output$table_raw <- renderDT({
    datatable(filtered_df(), options = list(pageLength = 10), rownames = FALSE)
  })
}

# --- Lancement ---
shinyApp(ui, server)