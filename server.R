library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)

function(input, output, session) {
    
    output$overviewPlot <- renderPlotly({
      if (input$overviewVar == "Loyer") {
        if (input$plotType == "hist") {
          p <- ggplot(data, aes_string(x = "Loyer")) +
            geom_histogram(fill = "#4682B4", color = "black", binwidth = 30) +
            labs(title = "Distribution des loyers", x = "Loyer (en milliers de FCFA)", y = "Nombre de logements") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
          data$dummy <- ""
          p <- ggplot(data, aes(x = dummy, y = Loyer, fill = "Loyer")) +
            geom_boxplot(color = "black") +
            labs(title = "Boîte à moustache loyers", x = "", y = "Loyer (Millier de FCFA)", fill = "Type de valeur") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_fill_manual(values = "#4682B4") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        }
      } else if (input$overviewVar == "SurfTerrain") {
        if (input$plotType == "hist") {
          p <- ggplot(data, aes_string(x = "SurfTerrain")) +
            geom_histogram(fill = "#4682B4", color = "black", bins = 30) +
            labs(title = "Distribution de SurfTerrain", x = "SurfTerrain", y = "Nombre de Logements") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
          data$dummy <- ""
          p <- ggplot(data, aes(x = dummy, y = SurfTerrain, fill = "SurfTerrain")) +
            geom_boxplot(color = "black") +
            labs(title = "Boîte à moustaches de SurfTerrain", x = "", y = "SurfTerrain", fill = "Type de valeur") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_fill_manual(values = "#4682B4") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        }
      } else if (input$overviewVar == "NbPieces") {
        if (input$plotType == "hist") {
          p <- ggplot(data, aes_string(x = "NbPieces")) +
            geom_histogram(fill = "#4682B4", color = "black", bins = 30) +
            labs(title = "Distribution de NbPieces", x = "NbPieces", y = "Nombre de Logements") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 22)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
          data$dummy <- ""
          p <- ggplot(data, aes(x = dummy, y = NbPieces, fill = "NbPieces")) +
            geom_boxplot(color = "black") +
            labs(title = "Boîte à moustaches de NbPieces", x = "", y = "NbPieces", fill = "Type de valeur") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_fill_manual(values = "#4682B4") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 22))
        }
      } else if (input$overviewVar == "SurfHabitable") {
        if (input$plotType == "hist") {
          p <- ggplot(data, aes_string(x = "SurfHabitable")) +
            geom_histogram(fill = "#4682B4", color = "black", bins = 30) +
            labs(title = "Distribution de SurfHabitables", x = "SurfHabitables", y = "Nombre de Logements") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
          data$dummy <- ""
          p <- ggplot(data, aes(x = dummy, y = SurfHabitable, fill = "SurfHabitable")) +
            geom_boxplot(color = "black") +
            labs(title = "Boîte à moustaches de SurfHabitables", x = "", y = "SurfHabitable", fill = "Type de valeur") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_fill_manual(values = "#4682B4") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 500))
        }
      } else {

        if (input$plotType == "hist") {
          p <- ggplot(data, aes_string(x = input$overviewVar)) +
            geom_histogram(fill = "#4682B4", color = "black", bins = 30) +
            labs(title = paste("Distribution de", input$overviewVar), x = input$overviewVar, y = "Nombre de logements") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
          data$dummy <- ""
          p <- ggplot(data, aes(x = dummy, y = .data[[input$overviewVar]], fill = input$overviewVar)) +
            geom_boxplot(color = "black") +
            labs(title = paste("Boîte à moustaches de", input$overviewVar), x = "", y = input$overviewVar, fill = "Type de valeur") +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(face = "bold", size = 14)
            ) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        }
      }
      ggplotly(p)
    })
    
    output$rentAnalysis <- renderPlotly({
        if (input$rentVar == "SurfHabitable") {
            p <- ggplot(data, aes(x = SurfHabitable, y = Loyer)) +
                geom_point(color = "#4682B4") +
                geom_smooth(method = "lm", color = "#4682B4") +
                labs(title = "Relation entre le Loyer et la surface Habitable",
                     x = "SurfHabitables",
                     y = "Loyer (Millier de FCFA)") +
                theme_minimal(base_size = 14) +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(face = "bold", size = 14)
                ) +
                scale_x_continuous(breaks = seq(0, 450, by = 50),
                                   limits = c(0, 450)) +
                scale_y_continuous(breaks = seq(0, max(data$Loyer, na.rm = TRUE), by = 250))
        } else if (input$rentVar == "NbPieces") {
            p <- ggplot(data, aes(x = NbPieces, y = Loyer)) +
                geom_point(color = "#4682B4") +
                geom_smooth(method = "lm", color = "#4682B4") +
                labs(title = "Relation entre le Loyer et le nombre de pièces",
                     x = "NbPieces",
                     y = "Loyer (Millier de FCFA)") +
                theme_minimal(base_size = 14) +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(face = "bold", size = 14)
                ) +
                scale_x_continuous(breaks = seq(0, max(data$NbPieces, na.rm = TRUE), by = 2.5),
                                   limits = c(0, max(data$NbPieces, na.rm = TRUE))) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
        } else if (input$rentVar == "Type") {
            p <- ggplot(data, aes(x = Type, y = Loyer, fill = Type)) +
                geom_boxplot(color = "black") +
                labs(title = "Loyer selon le type de logement",
                     x = "Type",
                     y = "Loyer (Millier de FCFA)",
                     fill = "Type") +
                theme_minimal(base_size = 14) +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(face = "bold", size = 14)
                ) +
                scale_fill_manual(values = c("#4682B4", "#B44682")) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else if (input$rentVar == "Standing") {
            p <- ggplot(data, aes(x = Standing, y = Loyer, fill = Standing)) +
                geom_boxplot(color = "black") +
                labs(title = "Loyer selon le standing",
                     x = "Standing",
                     y = "Loyer (Millier de FCFA)",
                     fill = "Standing") +
                theme_minimal(base_size = 14) +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(face = "bold", size = 14)
                ) +
                scale_fill_manual(values = c("#4682B4", "#B44682")) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        } else {
            if (input$rentVar %in% numeric_vars) {
                p <- ggplot(data, aes_string(x = input$rentVar, y = "Loyer")) +
                    geom_point(color = "#4682B4") +
                    geom_smooth(method = "lm", color = "#4682B4") +
                    labs(title = paste("Relation entre le Loyer et", input$rentVar),
                         x = input$rentVar,
                         y = "Loyer (Millier de FCFA)") +
                    theme_minimal(base_size = 14) +
                    theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                        axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 12),
                        axis.title = element_text(face = "bold", size = 14)
                    ) +
                    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
            } else {
                p <- ggplot(data, aes_string(x = input$rentVar, y = "Loyer", fill = input$rentVar)) +
                    geom_boxplot(color = "black") +
                    labs(title = paste("Relation entre le Loyer et", input$rentVar),
                         x = input$rentVar,
                         y = "Loyer (Millier de FCFA)",
                         fill = input$rentVar) +
                    theme_minimal(base_size = 14) +
                    theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                        axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 12),
                        axis.title = element_text(face = "bold", size = 14)
                    ) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
            }
        }
        if (input$logScale) {
            p <- p + scale_y_log10()
        }
        ggplotly(p)
    })
    
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
      if (input$varGroup == "all") {
        temp_data <- data
        temp_data$RatioHabitableTerrain <- temp_data$SurfHabitable / temp_data$SurfTerrain
        temp_data$PiecesParM2 <- temp_data$NbPieces / temp_data$SurfHabitable
        temp_data$EquipementScore <- scale(temp_data$NbSDB + temp_data$NbCuis + temp_data$NbWC)
        temp_data$ConfortScore <- scale(temp_data$SurfHabitable / temp_data$NbPieces)
        
        variables <- c("Loyer", "SurfTerrain", "SurfHabitable", "SurfPiecResid", 
                      "NbPieces", "NbPiecesResid", "NbSDB", "NbChamBur", 
                      "NbSalonsSAM", "NbWC", "NbCuis", "RatioHabitableTerrain", 
                      "PiecesParM2", "EquipementScore", "ConfortScore")
        
        cor_mat <- cor(temp_data[, variables])
        return(cor_mat)
      } else if (input$varGroup == "surface") {
        vars <- c("Loyer", "SurfTerrain", "SurfHabitable", "SurfPiecResid", "NbPieces", 
                 "RatioHabitableTerrain", "PiecesParM2")
        selected_data <- data[, vars, drop = FALSE]
        for (col in names(selected_data)) {
          if (!is.numeric(selected_data[[col]])) {
            selected_data[[col]] <- as.numeric(as.factor(selected_data[[col]]))
          }
        }
        cor_mat <- cor(selected_data, use = "pairwise.complete.obs")
        return(cor_mat)
        
      } else if (input$varGroup == "equipements") {
        temp_data <- data
        temp_data$EquipementScore <- scale(temp_data$NbSDB + temp_data$NbCuis + temp_data$NbWC)
        
        vars <- c("Loyer", "NbSDB", "NbWC", "NbCuis", "EquipementScore")
        selected_data <- temp_data[, vars, drop = FALSE]
        for (col in names(selected_data)) {
          if (!is.numeric(selected_data[[col]])) {
            selected_data[[col]] <- as.numeric(as.factor(selected_data[[col]]))
          }
        }
        cor_mat <- cor(selected_data, use = "pairwise.complete.obs")
        return(cor_mat)
        
      } else if (input$varGroup == "confort") {
        vars <- c("Loyer", "ConfortScore", "Standing", "Etat")
        selected_data <- data[, vars, drop = FALSE]
        for (col in names(selected_data)) {
          if (!is.numeric(selected_data[[col]])) {
            selected_data[[col]] <- as.numeric(as.factor(selected_data[[col]]))
          }
        }
        cor_mat <- cor(selected_data, use = "pairwise.complete.obs")
        return(cor_mat)
        
      } else if (input$varGroup == "localisation") {
        vars <- c("Loyer", "DistCtrVille", "BordMer", "Commerc", "Quartier")
        selected_data <- data[, vars, drop = FALSE]
        for (col in names(selected_data)) {
          if (!is.numeric(selected_data[[col]])) {
            selected_data[[col]] <- as.numeric(as.factor(selected_data[[col]]))
          }
        }
        cor_mat <- cor(selected_data, use = "pairwise.complete.obs")
        return(cor_mat)
        
      } else {
        vars <- c("Loyer", "SurfTerrain", "SurfHabitable", "NbPieces")
      }
      
      if (input$varGroup != "all") {
        selected_data <- data[, vars, drop = FALSE]
        
        for (col in names(selected_data)) {
          if (!is.numeric(selected_data[[col]])) {
            selected_data[[col]] <- as.numeric(as.factor(selected_data[[col]]))
          }
        }
        
        cor_mat <- cor(selected_data, use = "pairwise.complete.obs")
        return(cor_mat)
      }
    })
    
    observe({
      vars <- selected_vars()
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
      cor_mat <- cor_matrix()
      corrplot(cor_mat, 
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
        
        detail <- ""
        if (var1 %in% c("RatioHabitableTerrain", "PiecesParM2")) {
          detail <- sprintf("<br>Cette variable dérivée permet d'analyser l'efficacité de l'utilisation de l'espace.")
        } else if (var1 %in% c("EquipementScore", "ConfortScore")) {
          detail <- sprintf("<br>Ce score composite reflète le niveau global d'équipement/confort du logement.")
        }
        
        specific_detail <- ""
        if ("Loyer" %in% c(var1, var2)) {
          impact <- if(cor_val > 0) "positive" else "négative"
          specific_detail <- sprintf("<li>Quand %s augmente, le loyer %s.</li>",
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
        geom_point(color = "#4682B4") +
        geom_smooth(method = "lm", color = "#4682B4") +
        labs(title = paste("Relation entre", var1, "et", var2), x = var1, y = var2) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(face = "bold", size = 14)
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      ggplotly(p)
    })
    
    output$loyerAnalysis <- renderUI({
      tryCatch({
        cor_mat <- cor_matrix()
        
        if (input$varGroup == "all") {
          surf_terrain_loyer <- round(cor_mat["SurfTerrain", "Loyer"], 2)
          surf_habitable_loyer <- round(cor_mat["SurfHabitable", "Loyer"], 2)
          nb_sdb_loyer <- round(cor_mat["NbSDB", "Loyer"], 2)
          nb_chambur_loyer <- round(cor_mat["NbChamBur", "Loyer"], 2)
          nb_salons_loyer <- round(cor_mat["NbSalonsSAM", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Facteurs clés influençant le loyer</p>
            <ul>
            <li><b>SurfTerrain</b> : forte corrélation positive (<b>%.2f</b>) ; cela signifie que plus la surface du terrain est grande, plus le loyer demandé est élevé.</li>
            <li><b>SurfHabitable</b> : forte corrélation positive (<b>%.2f</b>) ; donc, plus la surface habitable est importante, plus le logement est cher.</li>
            <li><b>NbSDB (Nombre de salles de bain)</b> : corrélation positive (<b>%.2f</b>) ; les logements avec davantage de salles de bain tendent à avoir un loyer plus élevé.</li>
            <li><b>NbChamBur (Nombre de chambres ou bureaux)</b> : corrélation positive (<b>%.2f</b>) ; plus un logement propose d\'espaces nuit ou de travail, plus son loyer augmente.</li>
            <li><b>NbSalonsSAM (Nombre de salons/salles à manger)</b> : corrélation positive (<b>%.2f</b>) ; un logement avec plusieurs espaces de vie est plus valorisé en loyer.</li>
            </ul>
          ', surf_terrain_loyer, surf_habitable_loyer, nb_sdb_loyer, nb_chambur_loyer, nb_salons_loyer))
          
        } else if (input$varGroup == "surface") {
          surf_terrain_loyer <- round(cor_mat["SurfTerrain", "Loyer"], 2)
          surf_habitable_loyer <- round(cor_mat["SurfHabitable", "Loyer"], 2)
          surf_piece_resid_loyer <- round(cor_mat["SurfPiecResid", "Loyer"], 2)
          nb_pieces_loyer <- round(cor_mat["NbPieces", "Loyer"], 2)
          ratio_habitable_terrain_loyer <- round(cor_mat["RatioHabitableTerrain", "Loyer"], 2)
          pieces_par_m2_loyer <- round(cor_mat["PiecesParM2", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Facteurs clés influençant le loyer</p>
            <ul>
            <li><b>SurfTerrain</b> : forte corrélation positive (<b>%.2f</b>) ; plus la surface du terrain est grande, plus le loyer demandé est élevé.</li>
            <li><b>SurfHabitable</b> : corrélation positive significative (<b>%.2f</b>) ; la surface habitable détermine le prix du loyer.</li>
            <li><b>SurfPieceResid</b> : corrélation positive modérée (<b>%.2f</b>) ; la surface des pièces résidentielles influence le loyer, mais moins fortement que la surface totale.</li>
            <li><b>NbPieces</b> : corrélation positive forte (<b>%.2f</b>) ; le nombre de pièces est fortement associé à un loyer plus élevé.</li>
            <li><b>RatioHabitableTerrain</b> : corrélation négative faible (<b>%.2f</b>) ; un ratio plus faible entre la surface habitable et le terrain est légèrement associé à des loyers plus élevés.</li>
            <li><b>PiecesParM2</b> : corrélation négative modérée (<b>%.2f</b>) ; moins de pièces par mètre carré (donc des pièces plus grandes) est associé à des loyers plus élevés.</li>
            </ul>
          ', surf_terrain_loyer, surf_habitable_loyer, surf_piece_resid_loyer, nb_pieces_loyer, ratio_habitable_terrain_loyer, pieces_par_m2_loyer))
          
        } else if (input$varGroup == "equipements") {
          nb_sdb_loyer <- round(cor_mat["NbSDB", "Loyer"], 2)
          nb_wc_loyer <- round(cor_mat["NbWC", "Loyer"], 2)
          nb_cuis_loyer <- round(cor_mat["NbCuis", "Loyer"], 2)
          equip_score_loyer <- round(cor_mat["EquipementScore", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Facteurs clés influençant le loyer</p>
            <ul>
            <li><b>NbSDB</b> : forte corrélation positive (<b>%.2f</b>) ; un plus grand nombre de salles de bain est généralement associé à un loyer plus élevé.</li>
            <li><b>NbWC</b> : forte corrélation positive (<b>%.2f</b>) ; les logements disposant de plus de WC tendent aussi à être plus chers.</li>
            <li><b>NbCuis</b> : faible corrélation positive (<b>%.2f</b>) ; la présence de plusieurs cuisines a un impact limité sur le montant du loyer.</li>
            <li><b>EquipementScore</b> : forte corrélation positive (<b>%.2f</b>) ; un bon niveau d\'équipement constitue un critère clé pour expliquer un loyer plus important.</li>
            </ul>
          ', nb_sdb_loyer, nb_wc_loyer, nb_cuis_loyer, equip_score_loyer))
          
        } else if (input$varGroup == "confort") {
          confort_score_loyer <- round(cor_mat["ConfortScore", "Loyer"], 2)
          standing_loyer <- round(cor_mat["Standing", "Loyer"], 2)
          etat_loyer <- round(cor_mat["Etat", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Facteurs clés influençant le loyer</p>
            <ul>
            <li><b>ConfortScore</b> : corrélation positive modérée (<b>%.2f</b>) ; un meilleur confort global contribue à augmenter le loyer, mais de manière modérée.</li>
            <li><b>Standing</b> : forte corrélation positive (<b>%.2f</b>) ; le standing du logement joue un rôle important dans la détermination du loyer.</li>
            <li><b>Etat</b> : corrélation très faible (<b>%.2f</b>) ; l\'état général du bien semble avoir très peu d\'effet sur le montant du loyer.</li>
            </ul>
          ', confort_score_loyer, standing_loyer, etat_loyer))
          
        } else if (input$varGroup == "localisation") {
          dist_ctr_ville_loyer <- round(cor_mat["DistCtrVille", "Loyer"], 2)
          bord_mer_loyer <- round(cor_mat["BordMer", "Loyer"], 2)
          commerc_loyer <- round(cor_mat["Commerc", "Loyer"], 2)
          quartier_loyer <- round(cor_mat["Quartier", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Facteurs clés influençant le loyer</p>
            <ul>
            <li><b>DistCtrVille</b> : corrélation négative faible (<b>%.2f</b>) ; plus un logement est éloigné du centre-ville, plus le loyer a tendance à être légèrement plus bas.</li>
            <li><b>BordMer</b> : corrélation négative modérée (<b>%.2f</b>) ; étonnamment, être proche du bord de mer est associé à des loyers un peu plus faibles.</li>
            <li><b>Commerc</b> : corrélation positive très faible (<b>%.2f</b>) ; la proximité des commerces a un petit effet positif sur le loyer.</li>
            <li><b>Quartier</b> : corrélation quasiment nulle (<b>%.2f</b>) ; le type de quartier semble n\'avoir presque aucun impact sur le loyer.</li>
            </ul>
          ', dist_ctr_ville_loyer, bord_mer_loyer, commerc_loyer, quartier_loyer))
          
        } else {
          HTML("<p>Sélectionnez un groupe de variables pour voir l'analyse détaillée.</p>")
        }
      }, error = function(e) {
        HTML("<p>Une erreur s'est produite lors de l'analyse des impacts sur le loyer.</p>")
      })
    })
    
    output$generalInterpretationTitle <- renderUI({
      HTML('<p style="font-size: 24px;">Interprétation générale</p>')
    })
    
    output$loyerAnalysisTitle <- renderUI({
      HTML('<p style="font-size: 24px;">Impact sur le loyer</p>')
    })
    
    output$generalInterpretation <- renderUI({
      tryCatch({
        cor_mat <- cor_matrix()
        
        if (input$varGroup == "all") {
          surf_pieces <- round(cor_mat["SurfHabitable", "NbPieces"], 2)
          surf_terrain <- round(cor_mat["SurfHabitable", "SurfTerrain"], 2)
          ratio_terrain <- round(cor_mat["RatioHabitableTerrain", "SurfTerrain"], 2)
          sdb_wc <- round(cor_mat["NbSDB", "NbWC"], 2)
          nbcuis_min <- round(cor_mat["NbCuis", "NbSDB"], 2)
          nbcuis_max <- round(cor_mat["NbCuis", "NbWC"], 2)
          equip_score_surfhab <- round(cor_mat["EquipementScore", "SurfHabitable"], 2)
          equip_score_nbpieces <- round(cor_mat["EquipementScore", "NbPieces"], 2)
          confort_score_corr <- 0.90 
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des relations entre variables :</p>
            <ul>
            <li><b>Surface et organisation :</b>
              <ul>
                <li><b>SurfHabitable</b> et <b>NbPieces</b> : corrélation extrêmement forte (<b>%.2f</b>) ; cela signifie que plus un logement est grand, plus il compte de pièces.</li>
                <li><b>SurfHabitable</b> et <b>SurfTerrain</b> : forte corrélation (<b>%.2f</b>) ; en général, un terrain plus vaste accueille une habitation plus grande.</li>
                <li><b>RatioHabitableTerrain</b> : corrélation négative avec la surface du terrain (<b>%.2f</b>) ; un ratio faible traduit souvent de grandes parcelles peu construites.</li>
              </ul>
            </li>
            <li><b>Équipements :</b>
              <ul>
                <li><b>NbSDB</b> et <b>NbWC</b> : corrélation forte (<b>%.2f</b>) ; les logements ayant plusieurs salles de bain disposent aussi généralement de plusieurs WC.</li>
                <li><b>NbCuis (Nombre de cuisines)</b> : corrélation plus faible (<b>entre %.2f et %.2f</b>) ; la variation du nombre de cuisines influence peu le reste des caractéristiques.</li>
              </ul>
            </li>
            <li><b>Confort et qualité :</b>
              <ul>
                <li><b>EquipementScore</b> : fortement corrélé à <b>SurfHabitable</b> (<b>%.2f</b>) et à <b>NbPieces</b> (<b>%.2f</b>) ; donc, plus un logement est spacieux et dispose de nombreuses pièces, plus il est bien équipé.</li>
                <li><b>ConfortScore</b> : corrélation très forte avec plusieurs variables clés (<b>> %.2f</b>) ; cela indique que le niveau de confort général impacte fortement la valeur locative.</li>
              </ul>
            </li>
            </ul>
          ', surf_pieces, surf_terrain, ratio_terrain, sdb_wc, nbcuis_min, nbcuis_max, equip_score_surfhab, equip_score_nbpieces, confort_score_corr))
          
        } else if (input$varGroup == "surface") {
          surf_pieces <- round(cor_mat["SurfHabitable", "NbPieces"], 2)
          pieces_piecesm2 <- round(cor_mat["NbPieces", "PiecesParM2"], 2)
          surfpieceresid_nbpieces <- round(cor_mat["SurfPiecResid", "NbPieces"], 2)
          surfpieceresid_piecesm2 <- round(cor_mat["SurfPiecResid", "PiecesParM2"], 2)
          surfhab_piecesm2 <- round(cor_mat["SurfHabitable", "PiecesParM2"], 2)
          ratio_pieces <- round(cor_mat["RatioHabitableTerrain", "PiecesParM2"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des relations entre variables :</p>
            <ul>
            <li><b>Organisation des pièces :</b>
              <ul>
                <li><b>SurfHabitable</b> et <b>NbPieces</b> : corrélation très forte (<b>%.2f</b>) ; logiquement, plus un logement est grand, plus il comporte de pièces.</li>
                <li><b>NbPieces</b> et <b>PiecesParM2</b> : corrélation négative forte (<b>%.2f</b>) ; les logements avec plus de pièces tendent à avoir moins de pièces par m², suggérant des pièces plus spacieuses.</li>
              </ul>
            </li>
            <li><b>Utilisation de l\'espace :</b>
              <ul>
                <li><b>SurfPiecResid</b> et <b>NbPieces</b> : corrélation positive modérée (<b>%.2f</b>) ; plus il y a de pièces résidentielles, plus leur surface totale est importante.</li>
                <li><b>SurfPiecResid</b> et <b>PiecesParM2</b> : corrélation négative très forte (<b>%.2f</b>) ; plus la surface des pièces résidentielles est importante, moins il y a de pièces par m² (pièces plus grandes).</li>
              </ul>
            </li>
            <li><b>Corrélations les plus fortes :</b>
              <ul>
                <li><b>SurfHabitable</b> et <b>PiecesParM2</b> : <b>%.2f</b> ; les logements plus grands ont tendance à avoir des pièces plus spacieuses.</li>
              </ul>
            </li>
            <li><b>RatioHabitableTerrain</b> et <b>PiecesParM2</b> : corrélation positive faible (<b>%.2f</b>) ; un ratio plus élevé (maison occupant une plus grande proportion du terrain) est légèrement associé à plus de pièces par m².</li>
            </ul>
          ', surf_pieces, pieces_piecesm2, surfpieceresid_nbpieces, surfpieceresid_piecesm2, surfhab_piecesm2, ratio_pieces))
          
        } else if (input$varGroup == "equipements") {
          equip_sdb <- round(cor_mat["EquipementScore", "NbSDB"], 2)
          equip_wc <- round(cor_mat["EquipementScore", "NbWC"], 2)
          equip_cuis <- round(cor_mat["EquipementScore", "NbCuis"], 2)
          sdb_wc <- round(cor_mat["NbSDB", "NbWC"], 2)
          sdb_equip <- equip_sdb  
          wc_equip <- equip_wc    
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des relations entre variables :</p>
            <ul>
            <li><b>Équipements et confort :</b>
              <ul>
                <li><b>EquipementScore</b> et <b>NbSDB</b> : corrélation très forte (<b>%.2f</b>) ; la quantité de salles de bain est un élément déterminant du score d\'équipement.</li>
                <li><b>EquipementScore</b> et <b>NbWC</b> : forte corrélation (<b>%.2f</b>) ; le nombre de WC joue aussi un rôle important dans la qualité perçue du logement.</li>
                <li><b>EquipementScore</b> et <b>NbCuis</b> : corrélation modérée (<b>%.2f</b>) ; plusieurs cuisines apportent un léger supplément au niveau d\'équipement global.</li>
              </ul>
            </li>
            <li><b>Corrélations les plus marquantes :</b>
              <ul>
                <li><b>NbSDB</b> et <b>EquipementScore</b> : <b>%.2f</b> ; les logements mieux équipés comportent nettement plus de salles de bain.</li>
                <li><b>NbWC</b> et <b>EquipementScore</b> : <b>%.2f</b> ; les WC sont également un critère important du confort global.</li>
                <li><b>NbSDB</b> et <b>NbWC</b> : <b>%.2f</b> ; les logements bien dotés en salles de bain le sont souvent aussi en WC.</li>
              </ul>
            </li>
            <li><b>Impact sur la valeur locative :</b>
              <ul>
                <li>Les équipements sanitaires (salles de bain et WC) influencent plus fortement le loyer que le nombre de cuisines.</li>
              </ul>
            </li>
            </ul>
          ', equip_sdb, equip_wc, equip_cuis, sdb_equip, wc_equip, sdb_wc))
          
        } else if (input$varGroup == "confort") {
          standing_loyer <- round(cor_mat["Standing", "Loyer"], 2)
          confort_loyer <- round(cor_mat["ConfortScore", "Loyer"], 2)
          standing_confort <- round(cor_mat["Standing", "ConfortScore"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des relations entre variables :</p>
            <ul>
            <li><b>Importance du standing :</b>
              <ul>
                <li>Le <b>standing</b> est l\'élément le plus corrélé au loyer (<b>%.2f</b>), ce qui montre que les locataires accordent beaucoup d\'importance à la qualité perçue du logement (prestige, localisation, qualité des matériaux).</li>
                <li>Le standing est relativement indépendant des autres indicateurs, ce qui en fait une dimension spécifique à part entière.</li>
              </ul>
            </li>
            <li><b>Rôle du confort global :</b>
              <ul>
                <li>Le <b>ConfortScore</b> a une influence modérée sur le loyer (<b>%.2f</b>), ce qui suggère qu\'un bon niveau de confort est apprécié, mais reste secondaire par rapport au standing.</li>
                <li>Sa corrélation modérée avec le standing (<b>%.2f</b>) montre qu\'ils sont liés, mais pas confondus : un logement peut être confortable sans pour autant être haut de gamme.</li>
              </ul>
            </li>
            <li><b>Un impact limité de l\'état du logement :</b>
              <ul>
                <li>L\'<b>état</b> (entretien, finitions) n\'a pratiquement pas d\'effet ni sur le loyer, ni sur les autres dimensions de qualité du logement.</li>
                <li>Cela indique que les locataires privilégient d\'abord le prestige et le confort perçu plutôt que l\'état réel du bien.</li>
              </ul>
            </li>
            <li><b>Implications pour la valorisation :</b>
              <ul>
                <li>Ces résultats montrent que pour optimiser un loyer, il est plus stratégique d\'améliorer le standing et le confort perçu que de miser uniquement sur l\'entretien.</li>
              </ul>
            </li>
            </ul>
          ', standing_loyer, confort_loyer, standing_confort))
          
        } else if (input$varGroup == "localisation") {
          bord_mer_loyer <- round(cor_mat["BordMer", "Loyer"], 2)
          dist_loyer <- round(cor_mat["DistCtrVille", "Loyer"], 2)
          commerc_loyer <- round(cor_mat["Commerc", "Loyer"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des relations entre variables :</p>
            <ul>
            <li><b>Impact des critères de localisation sur le loyer :</b>
              <ul>
                <li>Contrairement à ce qu\'on pourrait penser, être proche de la mer est associé à des loyers plus bas (<b>%.2f</b>). Cela pourrait s\'expliquer par des facteurs locaux spécifiques (zones inondables, lieu touristique, etc.).</li>
                <li>S\'éloigner du centre-ville entraîne une baisse modérée du loyer (<b>%.2f</b>), mais l\'effet reste relativement faible.</li>
                <li>La présence de commerces joue un rôle très mineur dans la variation des loyers (<b>%.2f</b>).</li>
              </ul>
            </li>
            <li><b>Relations entre les critères de localisation :</b>
              <ul>
                <li>Les logements près du centre-ville sont aussi souvent plus proches de la mer.</li>
                <li>Les zones plus éloignées du centre semblent mieux équipées en commerces, probablement grâce à la présence de grands pôles commerciaux en périphérie.</li>
              </ul>
            </li>
            <li><b>Le quartier, un facteur peu déterminant :</b>
              <ul>
                <li>Le <b>quartier</b> ne semble pas vraiment faire varier le loyer ; son influence est quasi inexistante dans cette étude.</li>
              </ul>
            </li>
            <li><b>En résumé :</b>
              <ul>
                <li>Dans cette zone, la proximité de la mer n\'est pas un atout pour faire augmenter les loyers.</li>
                <li>Les commerces sont plus nombreux en périphérie qu\'au cœur de la ville.</li>
                <li>Le standing ou la situation précise du logement semblent donc bien plus importants que le simple fait d\'être en centre-ville ou au bord de mer.</li>
              </ul>
            </li>
            </ul>
          ', bord_mer_loyer, dist_loyer, commerc_loyer))
          
        } else {
          HTML("<p>Sélectionnez un groupe de variables pour voir l'interprétation générale.</p>")
        }
      }, error = function(e) {
        HTML("<p>Une erreur s'est produite lors de l'interprétation générale.</p>")
      })
    })
    
    output$qualityAnalysisTitle <- renderUI({
      if (input$varGroup == "all") {
        HTML('<p style="font-size: 24px;">Analyse des facteurs de qualité</p>')
      } else if (input$varGroup == "equipements") {
        HTML('<p style="font-size: 24px;">Analyse des installations sanitaires</p>')
      } else if (input$varGroup == "surface") {
        HTML('<p style="font-size: 24px;">Analyse des facteurs de surface</p>')
      } else if (input$varGroup == "confort") {
        HTML('<p style="font-size: 24px;">Analyse des facteurs de confort</p>')
      } else if (input$varGroup == "localisation") {
        HTML('<p style="font-size: 24px;">Analyse des facteurs de localisation</p>')
      } else {
        HTML("")
      }
    })
    
    output$qualityAnalysis <- renderUI({
      tryCatch({
        cor_mat <- cor_matrix()
        
        if (input$varGroup == "all") {
          equip_loyer <- round(cor_mat["EquipementScore", "Loyer"], 2)
          confort_score_corr <- 0.90
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Analyse des facteurs de qualité :</p>
            <ul>
            <li><b>EquipementScore</b> : corrélation forte avec le loyer (<b>%.2f</b>) ; un logement mieux équipé est associé à un loyer plus élevé.</li>
            <li><b>ConfortScore</b> : corrélation très forte avec plusieurs variables clés (<b>> %.2f</b>) ; cela indique que le niveau de confort général impacte fortement la valeur locative.</li>
            <li>Le <b>score de confort</b> est directement lié à la qualité de l\'aménagement intérieur, comme le nombre de pièces et d\'équipements.</li>
            </ul>
          ', equip_loyer, confort_score_corr))
          
        } else if (input$varGroup == "equipements") {
          sdb_wc <- round(cor_mat["NbSDB", "NbWC"], 2)
          sdb_cuis <- round(cor_mat["NbSDB", "NbCuis"], 2)
          wc_cuis <- round(cor_mat["NbWC", "NbCuis"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Relations entre les installations sanitaires :</p>
            <ul>
            <li><b>NbSDB</b> et <b>NbWC</b> : corrélation très forte (<b>%.2f</b>) ; plus un logement dispose de salles de bain, plus il compte également de WC.</li>
            <li><b>NbSDB</b> et <b>NbCuis</b> : corrélation faible (<b>%.2f</b>) ; on observe une légère tendance à retrouver davantage de cuisines dans les logements ayant plusieurs salles de bain.</li>
            <li><b>NbWC</b> et <b>NbCuis</b> : corrélation très faible (<b>%.2f</b>) ; pratiquement aucune relation entre le nombre de WC et celui des cuisines.</li>
            </ul>
          ', sdb_wc, sdb_cuis, wc_cuis))
          
        } else if (input$varGroup == "surface") {
          surf_terrain_habitable <- round(cor_mat["SurfTerrain", "SurfHabitable"], 2)
          surf_terrain_pieceresid <- round(cor_mat["SurfTerrain", "SurfPiecResid"], 2)
          surf_habitable_pieceresid <- round(cor_mat["SurfHabitable", "SurfPiecResid"], 2)
          ratio_terrain <- round(cor_mat["RatioHabitableTerrain", "SurfTerrain"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Relations entre les différentes surfaces :</p>
            <ul>
            <li><b>SurfTerrain</b> et <b>SurfHabitable</b> : corrélation très forte (<b>%.2f</b>) ; les propriétés avec de grands terrains ont généralement de plus grandes surfaces habitables.</li>
            <li><b>SurfTerrain</b> et <b>SurfPiecResid</b> : corrélation forte (<b>%.2f</b>) ; la surface du terrain est fortement liée à la surface des pièces résidentielles.</li>
            <li><b>SurfHabitable</b> et <b>SurfPiecResid</b> : corrélation très forte (<b>%.2f</b>) ; logiquement, plus la surface habitable est grande, plus la surface dédiée aux pièces résidentielles l\'est aussi.</li>
            <li><b>RatioHabitableTerrain</b> et <b>SurfTerrain</b> : corrélation négative forte (<b>%.2f</b>) ; plus le terrain est grand, plus le ratio surface habitable/terrain tend à diminuer.</li>
            </ul>
          ', surf_terrain_habitable, surf_terrain_pieceresid, surf_habitable_pieceresid, ratio_terrain))
          
        } else if (input$varGroup == "confort") {
          confort_standing <- round(cor_mat["ConfortScore", "Standing"], 2)
          confort_etat <- round(cor_mat["ConfortScore", "Etat"], 2)
          standing_etat <- round(cor_mat["Standing", "Etat"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Relations entre les indicateurs de confort :</p>
            <ul>
            <li><b>ConfortScore</b> et <b>Standing</b> : corrélation modérée (<b>%.2f</b>) ; un logement plus confortable est souvent associé à un standing supérieur, mais la relation reste moyenne.</li>
            <li><b>ConfortScore</b> et <b>Etat</b> : corrélation quasi inexistante (<b>%.2f</b>) ; l\'état du logement influence très peu la perception globale du confort.</li>
            <li><b>Standing</b> et <b>Etat</b> : corrélation très faible (<b>%.2f</b>) ; le standing et l\'état d\'entretien sont presque indépendants l\'un de l\'autre.</li>
            </ul>
          ', confort_standing, confort_etat, standing_etat))
          
        } else if (input$varGroup == "localisation") {
          dist_bordmer <- round(cor_mat["DistCtrVille", "BordMer"], 2)
          dist_commerc <- round(cor_mat["DistCtrVille", "Commerc"], 2)
          dist_quartier <- round(cor_mat["DistCtrVille", "Quartier"], 2)
          bordmer_commerc <- round(cor_mat["BordMer", "Commerc"], 2)
          
          HTML(sprintf('
            <p style="font-size: 18px; font-weight: bold;">Relations entre les indicateurs de localisation :</p>
            <ul>
            <li><b>DistCtrVille</b> et <b>BordMer</b> : corrélation négative modérée (<b>%.2f</b>) ; les logements près du centre-ville sont en général aussi plus proches de la mer.</li>
            <li><b>DistCtrVille</b> et <b>Commerc</b> : corrélation positive modérée (<b>%.2f</b>) ; plus on s\'éloigne du centre, plus on trouve de commerces à proximité, sans doute dans des zones commerciales en périphérie.</li>
            <li><b>DistCtrVille</b> et <b>Quartier</b> : corrélation très faible (<b>%.2f</b>) ; il y a très peu de lien entre l\'éloignement du centre et le type de quartier.</li>
            <li><b>BordMer</b> et <b>Commerc</b> : corrélation négative modérée (<b>%.2f</b>) ; les logements près de la mer sont souvent situés dans des zones avec moins de commerces.</li>
            </ul>
          ', dist_bordmer, dist_commerc, dist_quartier, bordmer_commerc))
          
        } else {
          HTML("<p>Sélectionnez un groupe de variables pour voir l'analyse détaillée.</p>")
        }
      }, error = function(e) {
        HTML("<p>Une erreur s'est produite lors de l'analyse des facteurs de qualité.</p>")
      })
    })
    
    output$neighborhoodPlot <- renderPlotly({
      if (input$neighborhoodVar == "loyer_mean") {
        neighborhood_stats <- data %>%
          group_by(Quartier) %>%
          summarise(value = mean(Loyer, na.rm = TRUE)) %>%
          arrange(desc(value))
        y_label <- "Loyer moyen (Millier de FCFA)"
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
      neighborhood_stats$Quartier <- factor(neighborhood_stats$Quartier, levels = neighborhood_stats$Quartier)
      p <- ggplot(neighborhood_stats, aes(x = Quartier, y = value)) +
        geom_bar(stat = "identity", fill = "#4682B4", color = "black", width = 0.8) +
        labs(x = "Quartier", y = y_label,
             title = paste("Analyse par quartier :", y_label)) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(face = "bold", size = 14)
        )
      ggplotly(p)
    })
    
    output$dataTable <- renderDT({
      datatable(data, options = list(
        pageLength = 10,
        scrollX = TRUE
      ))
    })
}
