# ============================================================
# app.R — Point d'entrée de l'application Shiny CEM
# CEM Rennes – Dashboard Oncologie v3
#
# Ce fichier est le SEUL à lancer pour démarrer l'application.
#
# Comment lancer l'application :
#   Option 1 : Dans RStudio, cliquer sur le bouton "Run App"
#   Option 2 : Dans la console R, taper : shiny::runApp()
#   Option 3 : Dans le terminal : Rscript -e "shiny::runApp('.',port=3838)"
#
# L'application est organisée en 4 fichiers :
#   ┌─────────────────────────────────────────────────────┐
#   │  app.R     → Ce fichier (point d'entrée uniquement) │
#   │  global.R  → Données + transformations + fonctions  │
#   │  ui.R      → Interface visuelle (layout, login)     │
#   │  server.R  → Logique serveur (calculs, graphiques)  │
#   └─────────────────────────────────────────────────────┘
#
# Packages nécessaires (installer une seule fois) :
# install.packages(c(
#   "shiny", "bslib", "dplyr", "ggplot2", "DT",
#   "lubridate", "tidyr", "scales", "survival", "broom",
#   "forcats", "stringr", "shinycssloaders",
#   "plotly", "shinyjs"
# ))
# ============================================================

# Chargement des 3 fichiers constitutifs de l'application.
# source() exécute un fichier R comme s'il était écrit ici.
source("global.R", local = FALSE) # → charge les données et fonctions utilitaires
source("ui.R", local = FALSE) # → définit login_ui, dashboard_ui, ui (interface)
source("server.R", local = FALSE) # → définit la fonction "server" (logique)

# Démarrage de l'application Shiny.
# shinyApp() assemble l'interface (ui) et la logique (server).
shinyApp(ui = ui, server = server)
