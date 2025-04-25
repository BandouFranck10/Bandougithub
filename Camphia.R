library(shiny)
library(highcharter)
library(dplyr)
library(quarto)
library(DT)

# Interface Utilisateur
ui <- fluidPage(
  titlePanel("Tableau de bord KoboToolbox"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Région :", 
                  choices = c("Toutes", unique(kobo_data$region_origine)), 
                  selected = "Toutes"),
      checkboxGroupInput("sexe", "Sexe :", 
                         choices = unique(na.omit(kobo_data$sexe)), 
                         selected = unique(na.omit(kobo_data$sexe))),
      actionButton("generate", "Générer le rapport"),
      br(), br(),
      downloadButton("report", "Télécharger le rapport")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vue d'ensemble",
                 highchartOutput("chart1"),
                 highchartOutput("chart2")
        ),
        tabPanel("Données",
                 DTOutput("data_table")
        )
      )
    )
  )
)

# Logique serveur
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Vérification de l'existence des colonnes
    req(c("region_origine", "sexe") %in% names(kobo_data))
    
    df <- kobo_data
    
    # Filtre région avec vérification
    if (!is.null(input$region) && input$region != "Toutes") {
      df <- df %>% filter(region_origine == input$region)
    }
    
    # Filtre sexe avec vérification
    if (!is.null(input$sexe)) {
      df <- df %>% filter(sexe %in% input$sexe)
    }
    
    # Retourne les données filtrées ou un message d'erreur
    if (nrow(df) == 0) {
      showNotification("Aucune donnée ne correspond aux filtres", type = "warning")
    }
    
    df
  })
  
  output$chart1 <- renderHighchart({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data %>% 
      count(sexe) %>% 
      hchart("column", hcaes(x = sexe, y = n)) %>% 
      hc_title(text = "Répartition par sexe")
  })
  
  output$chart2 <- renderHighchart({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data %>% 
      count(region_origine) %>% 
      hchart("pie", hcaes(name = region_origine, y = n)) %>% 
      hc_title(text = "Répartition par région")
  })
  
  output$data_table <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    datatable(data)
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste("rapport_camphia_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      data <- filtered_data()
      req(nrow(data) > 0)
      
      tempReport <- file.path(tempdir(), "rapport_camphia.qmd")
      file.copy("rapport_camphia.qmd", tempReport, overwrite = TRUE)
      
      params <- list(data = data)
      
      tryCatch({
        quarto::quarto_render(tempReport, output_file = file, params = params)
      }, error = function(e) {
        showNotification("Erreur lors de la génération du rapport", type = "error")
      })
    }
  )
}

shinyApp(ui = ui, server = server)