packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "dplyr",
  "plotly",
  "corrplot",
  "tidyr"
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(packages, library, character.only = TRUE)
