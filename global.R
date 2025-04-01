source("requirements.R")

# Load and prepare data
data <- read.csv("dakar.csv", row.names = 1)

# Convert character columns to factors where appropriate
categorical_vars <- c("Type", "Standing", "Etat", "Jardin", "Cour", "Piscine", 
                     "Garage", "Egout", "HiTech", "DistCtrVille", "Commerc", 
                     "BordMer", "Quartier")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Prepare numeric variables for analysis
numeric_vars <- c("Loyer", "SurfTerrain", "SurfHabitable", "SurfPiecResid", 
                 "NbPieces", "NbPiecesResid", "NbSDB", "NbChamBur", 
                 "NbSalonsSAM", "NbWC", "NbCuis")

# Créer des variables dérivées pour l'analyse
data$RatioHabitableTerrain <- data$SurfHabitable / data$SurfTerrain
data$PiecesParM2 <- data$NbPieces / data$SurfHabitable
data$EquipementScore <- rowSums(data[c("NbSDB", "NbWC", "NbCuis")] > 0)
data$ConfortScore <- rowSums(data[c("Jardin", "Cour", "Piscine", "Garage", "Egout", "HiTech")] == "Oui", na.rm = TRUE)

# Ajouter les nouvelles variables numériques à la liste
numeric_vars <- c(numeric_vars, "RatioHabitableTerrain", "PiecesParM2", "EquipementScore", "ConfortScore")

# Créer des groupes de variables pour l'analyse
var_groups <- list(
    "Surface et Pièces" = c("SurfTerrain", "SurfHabitable", "SurfPiecResid", "NbPieces", "RatioHabitableTerrain", "PiecesParM2"),
    "Équipements" = c("NbSDB", "NbWC", "NbCuis", "EquipementScore"),
    "Confort" = c("ConfortScore", "Standing", "Etat"),
    "Localisation" = c("DistCtrVille", "BordMer", "Commerc", "Quartier")
)
