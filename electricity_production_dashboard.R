#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# --- 1. CHARGEMENT DES PACKAGES ---
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)

# --- 2. IMPORTATION ET PRÉPARATION ---
options(scipen = 999)

# Chargement de tes données
evolution_production <- read.csv2("RTE Data/Evolution production electrique.csv", 
                                  header = TRUE, 
                                  stringsAsFactors = FALSE, 
                                  na.strings ="")

mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
             "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")

evolution_production <- evolution_production %>%
  filter(Filière != "Production totale") %>%
  mutate(
    Date = ym(Date),
    Année = year(Date),
    Mois = factor(month(Date), levels = 1:12, labels = mois_fr, ordered = TRUE),
    Trimestre = paste0("T", quarter(Date))
  )

# --- 3. INTERFACE UTILISATEUR (UI) ---
ui <- page_sidebar(
  title = "Observatoire Énergie - CGDD",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Filtres (Sélections multiples)",
    # On ajoute multiple = TRUE partout
    selectInput("filiere_sel", "Filières :", 
                choices = unique(evolution_production$Filière), 
                selected = unique(evolution_production$Filière), 
                multiple = TRUE),
    
    selectInput("annee_sel", "Années :", 
                choices = sort(unique(evolution_production$Année), decreasing = TRUE), 
                selected = max(evolution_production$Année), 
                multiple = TRUE),
    
    selectInput("trim_sel", "Trimestres :", 
                choices = c("T1", "T2", "T3", "T4"), 
                selected = c("T1", "T2", "T3", "T4"), 
                multiple = TRUE),
    
    selectInput("mois_sel", "Mois :", 
                choices = mois_fr, 
                selected = mois_fr, 
                multiple = TRUE),
    hr(),
    downloadButton("downloadData", "Exporter les données (CSV)", class = "btn-success")
  ),
  
  # Graphiques
  navset_card_pill(
    nav_panel("Volumes (TWh)", 
              layout_column_wrap(
                width = 1,
                card(card_header("Évolution temporelle"), plotlyOutput("plot_evolution")),
                card(card_header("Répartition par filière"), plotlyOutput("plot_barres"))
              )
    ),
    nav_panel("Mix Énergétique (%)", 
              card(
                card_header("Évolution des parts de production (en %)"),
                plotlyOutput("plot_pourcentage")
              ),
              card(
                card_header("Répartition globale sur la sélection"),
                plotlyOutput("plot_pie")
              )
    )
  )
)

# --- 4. LOGIQUE SERVEUR ---
server <- function(input, output, session) {
  
  # Filtrage réactif avec %in% pour gérer le multiple
  data_filtree <- reactive({
    req(input$filiere_sel, input$annee_sel, input$trim_sel, input$mois_sel)
    
    evolution_production %>%
      filter(Filière %in% input$filiere_sel,
             Année %in% input$annee_sel,
             Trimestre %in% input$trim_sel,
             Mois %in% input$mois_sel)
  })
  
  # 1. Évolution en Volume
  output$plot_evolution <- renderPlotly({
    p <- ggplot(data_filtree(), aes(x = Date, y = Valeur..TWh., color = Filière)) +
      geom_line() + theme_minimal() + labs(y = "TWh")
    ggplotly(p)
  })
  
  # 2. Évolution en Pourcentage (Le nouveau graphique)
  output$plot_pourcentage <- renderPlotly({
    # On calcule la part de chaque filière par date
    df_pct <- data_filtree() %>%
      group_by(Date) %>%
      mutate(Total_Date = sum(Valeur..TWh., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Part = (Valeur..TWh. / Total_Date) * 100)
    
    p <- ggplot(df_pct, aes(x = Date, y = Part, fill = Filière)) +
      geom_area(alpha = 0.8, stat = "identity") +
      theme_minimal() +
      labs(y = "Part de la production (%)", x = NULL)
    
    ggplotly(p)
  })
  
  # 3. Barres
  output$plot_barres <- renderPlotly({
    res <- data_filtree() %>% group_by(Filière) %>% summarise(Total = sum(Valeur..TWh.))
    p <- ggplot(res, aes(x = reorder(Filière, Total), y = Total, fill = Filière)) +
      geom_col() + coord_flip() + theme_minimal() + labs(x = NULL, y = "TWh")
    ggplotly(p)
  })
  
  # 4. Pie Chart
  output$plot_pie <- renderPlotly({
    res <- data_filtree() %>% group_by(Filière) %>% summarise(Total = sum(Valeur..TWh.))
    plot_ly(res, labels = ~Filière, values = ~Total, type = 'pie')
  })
  
  # 5. Export des données
  output$downloadData <- downloadHandler(
    filename = function() { paste("data-energie-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(data_filtree(), file, row.names = FALSE) }
  )
}

# Lancer l'app
shinyApp(ui, server)