library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
    dashboardHeader(title = "Analyse des Logements Dakarois"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
            menuItem("Analyse du Loyer", tabName = "rent", icon = icon("money-bill")),
            menuItem("Corrélations", tabName = "correlations", icon = icon("chart-line")),
            menuItem("Analyse par Quartier", tabName = "neighborhood", icon = icon("map-marker-alt")),
            menuItem("Données", tabName = "data", icon = icon("table"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                fluidRow(
                    box(plotlyOutput("overviewPlot"), width = 8),
                    box(width = 4,
                        selectInput("overviewVar", "Variable à analyser:",
                                  choices = c("Loyer", "SurfTerrain", "NbPieces", "SurfHabitable")),
                        radioButtons("plotType", "Type de graphique:",
                                   choices = c("Histogramme" = "hist",
                                              "Boîte à moustaches" = "box"))
                    )
                )
            ),
            
            tabItem(tabName = "rent",
                fluidRow(
                    box(plotlyOutput("rentAnalysis"), width = 8),
                    box(width = 4,
                        selectInput("rentVar", "Variable explicative:",
                                  choices = c("SurfHabitable", "NbPieces", "Type", "Standing")),
                        checkboxInput("logScale", "Échelle logarithmique pour le loyer", FALSE)
                    )
                )
            ),
            
            tabItem(tabName = "correlations",
                fluidRow(
                    box(width = 12,
                        title = "Groupes de variables",
                        radioButtons("varGroup", "Sélectionner un groupe de variables:",
                                   choices = c("Toutes les variables" = "all",
                                              "Surface et Pièces" = "surface",
                                              "Équipements" = "equipements",
                                              "Confort" = "confort",
                                              "Localisation" = "localisation"),
                                   inline = TRUE)
                    )
                ),
                fluidRow(
                    box(plotOutput("correlationPlot"), width = 12,
                        title = "Matrice de corrélation"),
                ),
                fluidRow(
                    box(width = 6,
                        title = "Impact sur le loyer",
                        htmlOutput("loyerAnalysis")
                    ),
                    box(width = 6,
                        title = "Analyse des facteurs de qualité",
                        htmlOutput("qualityAnalysis")
                    )
                ),
                fluidRow(
                    box(width = 12,
                        title = "Interprétation générale",
                        htmlOutput("generalInterpretation")
                    )
                )
            ),
            
            tabItem(tabName = "neighborhood",
                fluidRow(
                    box(plotlyOutput("neighborhoodPlot"), width = 12),
                    box(width = 4,
                        selectInput("neighborhoodVar", "Variable à analyser:",
                                  choices = c("Loyer moyen" = "loyer_mean",
                                            "Surface moyenne" = "surface_mean",
                                            "Nombre de logements" = "count"))
                    )
                )
            ),
            
            tabItem(tabName = "data",
                fluidRow(
                    box(DTOutput("dataTable"), width = 12)
                )
            )
        )
    )
)
