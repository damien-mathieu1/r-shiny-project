source("requirements.R")

data <- read.csv("dakar.csv", row.names = 1)

categorical_vars <- c("Type", "Standing", "Etat", "Jardin", "Cour", "Piscine", 
                     "Garage", "Egout", "HiTech", "DistCtrVille", "Commerc", 
                     "BordMer", "Quartier")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

numeric_vars <- c("Loyer", "SurfTerrain", "SurfHabitable", "SurfPiecResid", 
                 "NbPieces", "NbPiecesResid", "NbSDB", "NbChamBur", 
                 "NbSalonsSAM", "NbWC", "NbCuis")

data$RatioHabitableTerrain <- data$SurfHabitable / data$SurfTerrain
data$PiecesParM2 <- data$NbPieces / data$SurfHabitable
data$EquipementScore <- rowSums(data[c("NbSDB", "NbWC", "NbCuis")] > 0)
data$ConfortScore <- rowSums(data[c("Jardin", "Cour", "Piscine", "Garage", "Egout", "HiTech")] == "Oui", na.rm = TRUE)

numeric_vars <- c(numeric_vars, "RatioHabitableTerrain", "PiecesParM2", "EquipementScore", "ConfortScore")


var_groups <- list(
    "Surface et Pièces" = c("SurfTerrain", "SurfHabitable", "SurfPiecResid", "NbPieces", "RatioHabitableTerrain", "PiecesParM2"),
    "Équipements" = c("NbSDB", "NbWC", "NbCuis", "EquipementScore"),
    "Confort" = c("ConfortScore", "Standing", "Etat"),
    "Localisation" = c("DistCtrVille", "BordMer", "Commerc", "Quartier")
)
