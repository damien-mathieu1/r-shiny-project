library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)

function(input, output, session) {
    
    # Vue d'ensemble
    output$overviewPlot <- renderPlotly({
        if (input$plotType == "hist") {
            p <- ggplot(data, aes_string(x = input$overviewVar)) +
                geom_histogram(fill = "#3c8dbc", color = "white") +
                theme_minimal() +
                labs(title = paste("Distribution de", input$overviewVar))
        } else {
            p <- ggplot(data, aes_string(y = input$overviewVar)) +
                geom_boxplot(fill = "#3c8dbc") +
                theme_minimal() +
                labs(title = paste("Boîte à moustaches de", input$overviewVar))
        }
        ggplotly(p)
    })
    
    # Analyse du Loyer
    output$rentAnalysis <- renderPlotly({
        if (input$rentVar %in% numeric_vars) {
            p <- ggplot(data, aes_string(x = input$rentVar, y = "Loyer")) +
                geom_point(color = "#3c8dbc", alpha = 0.6) +
                geom_smooth(method = "lm", color = "#f39c12") +
                theme_minimal()
        } else {
            p <- ggplot(data, aes_string(x = input$rentVar, y = "Loyer")) +
                geom_boxplot(fill = "#3c8dbc") +
                theme_minimal()
        }
        
        if (input$logScale) {
            p <- p + scale_y_log10()
        }
        
        p <- p + labs(title = paste("Relation entre le Loyer et", input$rentVar))
        ggplotly(p)
    })
    
    # Corrélations
    selected_vars <- reactive({
        if (input$varGroup == "all") {
            return(numeric_vars)
        } else {
            return(var_groups[[switch(input$varGroup,
                "surface" = "Surface et Pièces",
                "equipements" = "Équipements",
                "confort" = "Confort",
                "localisation" = "Localisation")]])
        }
    })
    
    cor_matrix <- reactive({
        # S'assurer que "Loyer" est toujours inclus si disponible dans les données
        vars_to_correlate <- unique(c("Loyer", selected_vars()))
        # Filtrer les variables qui existent réellement dans les données
        vars_to_correlate <- vars_to_correlate[vars_to_correlate %in% names(data)]
        
        cor_data <- data[vars_to_correlate]
        # Convertir les facteurs en numérique
        cor_data[sapply(cor_data, is.factor)] <- lapply(cor_data[sapply(cor_data, is.factor)], as.numeric)
        cor(cor_data, use = "complete.obs")
    })
    
    # Mise à jour des choix pour la sélection des paires de variables
    observe({
        vars <- selected_vars()
        # Filtrer les variables qui existent réellement dans les données
        vars <- vars[vars %in% names(data)]
        
        if (length(vars) > 1) {
            pairs <- expand.grid(var1 = vars, var2 = vars)
            pairs <- pairs[pairs$var1 < pairs$var2,]
            pair_choices <- paste(pairs$var1, "vs", pairs$var2)
            names(pair_choices) <- pair_choices
        } else {
            pair_choices <- character(0)
        }
        updateSelectInput(session, "correlPair", choices = pair_choices)
    })
    
    output$correlationPlot <- renderPlot({
        corrplot(cor_matrix(), 
                method = "color", 
                type = "upper",
                tl.col = "black", 
                tl.srt = 45,
                addCoef.col = "black", 
                number.cex = 0.7,
                col = colorRampPalette(c("#0073b7", "#ffffff", "#dc3545"))(100),
                diag = FALSE,
                title = paste("Matrice de corrélation -", 
                             switch(input$varGroup,
                                    "all" = "Toutes les variables",
                                    "surface" = "Surface et Pièces",
                                    "equipements" = "Équipements",
                                    "confort" = "Confort",
                                    "localisation" = "Localisation")),
                mar = c(0,0,1,0))
    })
    
    output$correlationText <- renderUI({
        if (is.null(input$correlPair) || input$correlPair == "") return(NULL)
        
        tryCatch({
            vars <- strsplit(input$correlPair, " vs ")[[1]]
            if (length(vars) != 2) return(NULL)
            
            var1 <- vars[1]
            var2 <- vars[2]
            
            # Vérifier que les variables existent dans la matrice de corrélation
            cor_mat <- cor_matrix()
            if (!all(c(var1, var2) %in% rownames(cor_mat))) return(NULL)
            
            cor_val <- cor_mat[var1, var2]
            
            interpretation <- case_when(
                abs(cor_val) >= 0.8 ~ "très forte",
                abs(cor_val) >= 0.6 ~ "forte",
                abs(cor_val) >= 0.4 ~ "modérée",
                abs(cor_val) >= 0.2 ~ "faible",
                TRUE ~ "très faible"
            )
            
            # Analyse détaillée en fonction du type de variables
            detail <- ""
            if (var1 %in% c("RatioHabitableTerrain", "PiecesParM2")) {
                detail <- sprintf("<br>Cette variable dérivée permet d'analyser l'efficacité de l'utilisation de l'espace.")
            } else if (var1 %in% c("EquipementScore", "ConfortScore")) {
                detail <- sprintf("<br>Ce score composite reflète le niveau global d'équipement/confort du logement.")
            }
            
            # Ajouter une interprétation spécifique en fonction des variables
            specific_detail <- ""
            if ("Loyer" %in% c(var1, var2)) {
                impact <- if (cor_val > 0) "augmente" else "diminue"
                specific_detail <- sprintf("<br><br>Quand %s augmente, le loyer %s.", 
                                         if(var1 == "Loyer") var2 else var1,
                                         impact)
            }
            
            HTML(sprintf(
                "<p>La corrélation entre <b>%s</b> et <b>%s</b> est <b>%s</b> (%.2f)</p>%s%s",
                var1, var2, interpretation, cor_val, detail, specific_detail
            ))
        }, error = function(e) {
            return(NULL)
        })
    })
    
    output$scatterCorr <- renderPlotly({
        if (is.null(input$correlPair)) return(NULL)
        
        vars <- strsplit(input$correlPair, " vs ")[[1]]
        var1 <- vars[1]
        var2 <- vars[2]
        
        p <- ggplot(data, aes_string(x = var1, y = var2)) +
            geom_point(aes(color = Type), alpha = 0.6) +
            geom_smooth(method = "lm", color = "#f39c12") +
            theme_minimal() +
            labs(title = paste("Relation entre", var1, "et", var2))
        
        ggplotly(p)
    })
    
    output$loyerAnalysis <- renderUI({
        tryCatch({
            cor_mat <- cor_matrix()
            
            # Vérifier si Loyer est dans la matrice
            if (!"Loyer" %in% rownames(cor_mat)) {
                return(HTML("<p>Sélectionnez un groupe de variables incluant le loyer pour voir l'analyse.</p>"))
            }
            
            # Récupérer les corrélations avec le loyer
            loyer_cors <- cor_mat["Loyer", ]
            loyer_cors <- loyer_cors[!is.na(loyer_cors)]  # Supprimer les NA
            loyer_cors <- loyer_cors[names(loyer_cors) != "Loyer"]  # Exclure l'auto-corrélation
            
            # Trier par valeur absolue
            loyer_cors <- sort(abs(loyer_cors), decreasing = TRUE)
            
            # Préparer l'analyse des facteurs de prix
            html_content <- "<p><b>Facteurs clés influenant le loyer :</b></p><ul>"
            
            # Ajouter les corrélations disponibles
            for (var in names(loyer_cors)[1:min(3, length(loyer_cors))]) {
                cor_val <- cor_mat["Loyer", var]
                impact <- if(cor_val > 0) "positive" else "négative"
                force <- case_when(
                    abs(cor_val) >= 0.8 ~ "très forte",
                    abs(cor_val) >= 0.6 ~ "forte",
                    abs(cor_val) >= 0.4 ~ "modérée",
                    TRUE ~ "faible"
                )
                html_content <- paste0(html_content,
                    sprintf("<li><b>%s</b> : corrélation %s %s (%.2f)</li>",
                            var, force, impact, cor_val))
            }
            
            # Ajouter des statistiques supplémentaires si disponibles
            if (all(c("Type", "Standing") %in% names(data))) {
                type_impact <- tryCatch({
                    sprintf("<li>Les villas ont en moyenne un loyer %.1f%% %s que les appartements</li>",
                            abs((mean(data$Loyer[data$Type == "Villa"]) / 
                                mean(data$Loyer[data$Type == "Appart"]) - 1) * 100),
                            if(mean(data$Loyer[data$Type == "Villa"]) > 
                               mean(data$Loyer[data$Type == "Appart"])) "plus élevé" else "moins élevé")
                }, error = function(e) "")
                
                standing_impact <- tryCatch({
                    sprintf("<li>Un logement avec standing coûte en moyenne %.1f%% plus cher</li>",
                            (mean(data$Loyer[data$Standing == "Oui"]) / 
                             mean(data$Loyer[data$Standing == "Non"]) - 1) * 100)
                }, error = function(e) "")
                
                html_content <- paste0(html_content, type_impact, standing_impact)
            }
            
            html_content <- paste0(html_content, "</ul>")
            HTML(html_content)
            
        }, error = function(e) {
            HTML("<p>Une erreur s'est produite lors de l'analyse des impacts sur le loyer.</p>")
        })
    })
    
    output$qualityAnalysis <- renderUI({
        # Analyse des facteurs de qualité
        quality_summary <- data %>%
            group_by(Standing) %>%
            summarise(
                loyer_moyen = mean(Loyer),
                confort_moyen = mean(ConfortScore),
                equip_moyen = mean(EquipementScore)
            )
        
        HTML(sprintf(
            "<p><b>Analyse des facteurs de qualité :</b></p>
            <ul>
                <li>Score de confort moyen : %.1f sur 6</li>
                <li>Score d'équipement moyen : %.1f sur 3</li>
                <li>Les logements de standing ont :
                    <ul>
                        <li>%.1f%% de score de confort en plus</li>
                        <li>%.1f%% de score d'équipement en plus</li>
                    </ul>
                </li>
            </ul>",
            mean(data$ConfortScore),
            mean(data$EquipementScore),
            (quality_summary$confort_moyen[quality_summary$Standing == "Oui"] /
             quality_summary$confort_moyen[quality_summary$Standing == "Non"] - 1) * 100,
            (quality_summary$equip_moyen[quality_summary$Standing == "Oui"] /
             quality_summary$equip_moyen[quality_summary$Standing == "Non"] - 1) * 100
        ))
    })
    
    output$generalInterpretation <- renderUI({
        tryCatch({
            cor_mat <- cor_matrix()
            html_content <- "<p><b>Analyse des relations entre variables :</b></p><ul>"
            
            # Fonction helper pour vérifier la disponibilité des variables
            vars_available <- function(vars) {
                all(vars %in% rownames(cor_mat))
            }
            
            # Analyse Surface et Pièces
            if (input$varGroup %in% c("all", "surface")) {
                html_content <- paste0(html_content, "<li><b>Surface et organisation :</b><ul>")
                
                if (vars_available(c("SurfHabitable", "NbPieces"))) {
                    html_content <- paste0(html_content,
                        sprintf("<li>La surface habitable et le nombre de pièces sont %s corrélés (%.2f)</li>",
                                if(abs(cor_mat["SurfHabitable", "NbPieces"]) > 0.6) "fortement" else "modérément",
                                cor_mat["SurfHabitable", "NbPieces"]))
                }
                
                if (vars_available(c("RatioHabitableTerrain"))) {
                    html_content <- paste0(html_content,
                        "<li>Le ratio habitable/terrain indique l'efficacité d'utilisation de l'espace</li>")
                }
                
                html_content <- paste0(html_content, "</ul></li>")
            }
            
            # Analyse Équipements
            if (input$varGroup %in% c("all", "equipements")) {
                html_content <- paste0(html_content, "<li><b>Équipements :</b><ul>")
                
                if (vars_available(c("NbSDB", "NbWC"))) {
                    html_content <- paste0(html_content,
                        sprintf("<li>Correlation entre salles de bain et WC : %.2f</li>",
                                cor_mat["NbSDB", "NbWC"]))
                }
                
                if (vars_available("EquipementScore")) {
                    html_content <- paste0(html_content,
                        "<li>Le score d'équipement reflète le niveau global d'aménagement</li>")
                }
                
                html_content <- paste0(html_content, "</ul></li>")
            }
            
            # Analyse Confort
            if (input$varGroup %in% c("all", "confort")) {
                html_content <- paste0(html_content, "<li><b>Confort et qualité :</b><ul>")
                
                if (vars_available("ConfortScore")) {
                    html_content <- paste0(html_content,
                        "<li>Le score de confort combine plusieurs critères de qualité</li>")
                }
                
                html_content <- paste0(html_content, "</ul></li>")
            }
            
            # Analyse des corrélations les plus fortes dans le groupe sélectionné
            vars <- rownames(cor_mat)
            if (length(vars) > 1) {
                top_cors <- c()
                for (i in 1:(length(vars)-1)) {
                    for (j in (i+1):length(vars)) {
                        top_cors <- c(top_cors, abs(cor_mat[vars[i], vars[j]]))
                    }
                }
                max_cor <- max(top_cors, na.rm = TRUE)
                
                if (!is.na(max_cor) && max_cor > 0.5) {
                    html_content <- paste0(html_content,
                        sprintf("<li><b>Corrélation la plus forte :</b> %.2f</li>", max_cor))
                }
            }
            
            html_content <- paste0(html_content, "</ul>")
            HTML(html_content)
            
        }, error = function(e) {
            HTML("<p>Sélectionnez un groupe de variables pour voir l'interprétation.</p>")
        })
    })
    
    # Analyse par Quartier
    output$neighborhoodPlot <- renderPlotly({
        if (input$neighborhoodVar == "loyer_mean") {
            neighborhood_stats <- data %>%
                group_by(Quartier) %>%
                summarise(value = mean(Loyer, na.rm = TRUE)) %>%
                arrange(desc(value))
            y_label <- "Loyer moyen"
        } else if (input$neighborhoodVar == "surface_mean") {
            neighborhood_stats <- data %>%
                group_by(Quartier) %>%
                summarise(value = mean(SurfHabitable, na.rm = TRUE)) %>%
                arrange(desc(value))
            y_label <- "Surface habitable moyenne"
        } else {
            neighborhood_stats <- data %>%
                group_by(Quartier) %>%
                summarise(value = n()) %>%
                arrange(desc(value))
            y_label <- "Nombre de logements"
        }
        
        p <- ggplot(neighborhood_stats, aes(x = reorder(Quartier, -value), y = value)) +
            geom_bar(stat = "identity", fill = "#3c8dbc") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Quartier", y = y_label,
                 title = paste("Analyse par quartier:", y_label))
        
        ggplotly(p)
    })
    
    # Table de données
    output$dataTable <- renderDT({
        datatable(data, options = list(
            pageLength = 10,
            scrollX = TRUE
        ))
    })
}
