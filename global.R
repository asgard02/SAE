# ============================================================
# global.R — Données, transformations et fonctions partagées
# CEM Rennes – Dashboard Oncologie v3
#
# Ce fichier est chargé UNE SEULE FOIS au démarrage de Shiny.
# Il prépare tout ce dont ui.R et server.R auront besoin.
#
# Plan de ce fichier :
#   1. Packages (bibliothèques)
#   2. Identifiants de connexion
#   3. Palette de couleurs CEM
#   4. Lecture du fichier CSV
#   5. Transformation des données (calcul de nouvelles colonnes)
#   6. Résumé par patient
#   7. Fonctions utilitaires (thème graphique, helpers)
# ============================================================


# ──────────────────────────────────────────────────────────
# 1. PACKAGES
#
# Un "package" en R est une boîte à outils qu'on charge avant
# de pouvoir utiliser ses fonctions.
#
# Pour installer tous les packages en une fois, exécutez :
# install.packages(c("shiny","bslib","dplyr","ggplot2","DT",
#   "lubridate","tidyr","scales","survival","broom","forcats",
#   "stringr","shinycssloaders","plotly","shinyjs"))
# ──────────────────────────────────────────────────────────

library(shiny) # Le coeur de l'application web interactive
library(bslib) # Thèmes Bootstrap modernes pour Shiny
library(dplyr) # Manipulation de tableaux de données (filtrer, grouper, etc.)
library(ggplot2) # Création de graphiques de qualité
library(DT) # Tableaux interactifs (tri, pagination)
library(lubridate) # Manipulation des dates (dmy, interval, etc.)
library(tidyr) # Mise en forme des données (pivotage, etc.)
library(scales) # Mise en forme des axes de graphiques (%, virgules, etc.)
library(survival) # Analyse de survie (courbes de Kaplan-Meier)
library(broom) # Convertit les résultats de modèles en tableaux propres
library(forcats) # Manipulation des facteurs (variables catégorielles ordonnées)
library(stringr) # Manipulation des chaînes de caractères
library(shinycssloaders) # Spinners de chargement pour les graphiques
library(plotly) # Graphiques interactifs avec survol (hover) comme PowerBI
library(shinyjs) # Actions JavaScript depuis R (animations, etc.)


# ──────────────────────────────────────────────────────────
# 2. IDENTIFIANTS DE CONNEXION
#
# Modifiez ici les identifiants et mots de passe.
# IMPORTANT : En production, utiliser un système de hashage
# des mots de passe (ex: package "sodium").
#
# Format : liste nommée  identifiant → liste(mdp, role)
# ──────────────────────────────────────────────────────────

IDENTIFIANTS <- list(
  medecin      = list(mdp = "CEM-Medecin2024!", role = "Médecin"),
  statisticien = list(mdp = "CEM-Stats2024!", role = "Statisticien")
)


# ──────────────────────────────────────────────────────────
# 3. PALETTE DE COULEURS CEM
#
# Couleurs extraites du logo officiel CEM Rennes (logo.svg).
# La variable col_primary (#005D92) correspond au style .st16
# du fichier SVG.
# ──────────────────────────────────────────────────────────

col_primary <- "#005D92" # Bleu CEM officiel
col_accent <- "#8CC63F" # Vert accent (positif, vivant)
col_warn <- "#E6A817" # Orange avertissement
col_danger <- "#C0392B" # Rouge critique / décès
col_dark <- "#001B35" # Fond sombre (navbar, login)
col_light <- "#F4F6F8" # Fond clair (sidebar)
col_text <- "#2C3E50" # Texte principal
col_subtext <- "#6C757D" # Texte secondaire


# ──────────────────────────────────────────────────────────
# 4. LECTURE DU FICHIER CSV
#
# read.csv() lit le fichier patients_data.csv et crée un
# "data frame" (tableau de données) dans R.
#
# stringsAsFactors = FALSE → les textes restent du texte
# na.strings        → ces valeurs seront considérées comme "vide" (NA)
# ──────────────────────────────────────────────────────────

df_brut <- read.csv(
  "patients_data.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


# ──────────────────────────────────────────────────────────
# 5. TRANSFORMATION DES DONNÉES
#
# On part du tableau brut (df_brut) et on ajoute des colonnes
# calculées dont on aura besoin dans les graphiques.
#
# dplyr::mutate() ajoute ou modifie des colonnes.
# ──────────────────────────────────────────────────────────

df <- df_brut %>%
  mutate(
    # ── Conversion des dates ──────────────────────────────
    # Les dates dans le CSV sont au format "JJ/MM/AAAA" ou similaire.
    date_naissance = if ("date_birth" %in% names(.)) dmy(date_birth) else as.Date(NA),
    date_diagnostic = if ("date_diagnosis" %in% names(.)) dmy(date_diagnosis) else as.Date(NA),
    date_entree = if ("date_sej" %in% names(.)) dmy(date_sej) else as.Date(NA),
    date_sortie = if ("date_discharge" %in% names(.)) dmy(date_discharge) else as.Date(NA),
    date_dern_vis = if ("date_last_visit" %in% names(.)) dmy(date_last_visit) else if ("date_last_vis" %in% names(.)) dmy(date_last_vis) else as.Date(NA),
    date_deces = if ("date_death" %in% names(.)) dmy(date_death) else as.Date(NA),
    date_contact = if ("date_cre_sej" %in% names(.)) dmy(date_cre_sej) else as.Date(NA),

    # ── Nettoyage des variables selon le nouveau plan ────
    type_sejour = case_when(
      type_sejour == "E" ~ "Externe",
      type_sejour == "H" ~ "Hospitalisation",
      type_sejour == "R" ~ "Radiothérapie",
      type_sejour == "S" ~ "Chimiothérapie",
      type_sejour == "?" ~ "Inconnu",
      TRUE ~ as.character(type_sejour)
    ),
    type_pec = if ("type_pec" %in% names(.)) {
      case_when(
        type_pec == "1" ~ "Pathologie cancéreuse",
        type_pec == "2" ~ "Pathologie connue",
        type_pec == "3" ~ "Pathologie inconnue",
        type_pec == "4" ~ "Patients non CEM",
        TRUE ~ as.character(type_pec)
      )
    } else {
      NA_character_
    },

    # ── Définition du groupe cancer (Les 4 organes cibles) ──
    groupe_cancer = if ("diag_cancer" %in% names(.)) {
      case_when(
        str_starts(diag_cancer, "^(C50|D05)") ~ "Sein",
        str_starts(diag_cancer, "^(C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26)") ~ "Appareil digestif",
        str_starts(diag_cancer, "^C61") ~ "Organes génitaux masculins",
        str_starts(diag_cancer, "^(C43|C44|D03|D04)") ~ "Peau",
        !is.na(diag_cancer) & diag_cancer != "" ~ "Autres",
        TRUE ~ "Non précisé"
      )
    } else {
      NA_character_
    },

    # ── Durée de séjour (DMS) en jours ───────────────────
    duree_sejour = if ("date_discharge" %in% names(.)) as.numeric(date_sortie - date_entree) else 0,

    # ── Âge du patient au moment du séjour ───────────────
    age_sejour = as.numeric(interval(date_naissance, date_entree) / years(1)),

    # ── Délai diagnostic → première admission (en mois) ──
    delai_diag_admit = case_when(
      !is.na(date_diagnostic) ~ as.numeric(interval(date_diagnostic, date_entree) / months(1)),
      !is.na(date_contact) ~ as.numeric(interval(date_contact, date_entree) / months(1)),
      TRUE ~ NA_real_
    ),

    # ── Tranche d'âge pour les graphiques ────────────────
    tranche_age = cut(
      age_sejour,
      breaks = c(0, 40, 55, 65, 75, 120),
      labels = c("<40", "40-54", "55-64", "65-74", "75+"),
      right  = FALSE # borne gauche incluse
    ),

    # ── Statut métastatique (meta ou M1) ─────────
    metastase = if ("diag_meta" %in% names(.)) {
      case_when(
        diag_meta == "meta" ~ "M1",
        diag_meta == "M1 (Métastases)" ~ "M1",
        diag_meta == "M0 (Pas de métastase)" ~ "M0",
        is.na(diag_meta) ~ "M0",
        TRUE ~ "M0"
      )
    } else {
      "M0"
    },

    # ── Indicateurs binaires (TRUE/FALSE) ────────────────
    a_comorbidite = if ("diag_associe" %in% names(.)) !is.na(diag_associe) & diag_associe != "" else FALSE,
    a_antecedent = if ("diag_antecedents" %in% names(.)) !is.na(diag_antecedents) & diag_antecedents != "" else FALSE,

    # ── Libellé du sexe ──────────────────────────────────
    sexe_libelle = ifelse(sexe == 1, "Homme", "Femme"),

    # ── Survie en mois (pour Kaplan-Meier) ───────────────
    mois_survie = case_when(
      !is.na(date_deces) & !is.na(date_diagnostic) ~ as.numeric(interval(date_diagnostic, date_deces) / months(1)),
      is.na(date_deces) & !is.na(date_diagnostic) ~ as.numeric(interval(date_diagnostic, date_dern_vis) / months(1)),
      !is.na(date_deces) & !is.na(date_contact) ~ as.numeric(interval(date_contact, date_deces) / months(1)),
      is.na(date_deces) & !is.na(date_contact) ~ as.numeric(interval(date_contact, date_dern_vis) / months(1)),
      TRUE ~ NA_real_
    ),

    # ── Événement décès (1 = décédé, 0 = vivant) ─────────
    evenement = as.integer(!is.na(date_deces)),

    # ── Catégorie de durée de séjour ─────────────────────
    cat_duree = case_when(
      duree_sejour == 0 ~ "Ambulatoire",
      duree_sejour <= 3 ~ "Court (<= 3j)",
      duree_sejour <= 10 ~ "Moyen (4-10j)",
      TRUE ~ "Long (>10j)"
    ),

    # ── Catégorie IMC ────────────────────────────────────
    cat_imc = if ("imc" %in% names(.)) {
      case_when(
        imc < 18.5 ~ "Maigreur",
        imc < 25.0 ~ "Normal",
        imc < 30.0 ~ "Surpoids",
        TRUE ~ "Obésité"
      )
    } else {
      NA_character_
    },

    # ── Stade TNM numérique (pour corrélations) ──────────
    stade_num = if ("stade_tnm" %in% names(.)) {
      case_when(
        stade_tnm == "I" ~ 1L,
        stade_tnm == "II" ~ 2L,
        stade_tnm == "III" ~ 3L,
        stade_tnm == "IV" ~ 4L,
        TRUE ~ NA_integer_
      )
    } else {
      NA_integer_
    },

    # ── Libellé score OMS / ECOG ─────────────────────────
    score_oms_libelle = if ("score_oms" %in% names(.)) {
      case_when(
        score_oms == 0 ~ "0 – Actif",
        score_oms == 1 ~ "1 – Légère limitation",
        score_oms == 2 ~ "2 – Ambulatoire, incapable travail",
        score_oms == 3 ~ "3 – Soins limités",
        score_oms == 4 ~ "4 – Grabataire",
        TRUE ~ "Inconnu"
      )
    } else {
      NA_character_
    }
  ) %>%
  # On garde uniquement les lignes avec un âge et une durée valides
  filter(!is.na(age_sejour), age_sejour >= 0, !is.na(duree_sejour), duree_sejour >= 0)


# ──────────────────────────────────────────────────────────
# 6. RÉSUMÉ PAR PATIENT (une ligne par patient unique)
#
# df contient une ligne par séjour (un patient peut avoir
# plusieurs séjours). df_patients agrège au niveau patient.
# ──────────────────────────────────────────────────────────

df_patients <- df %>%
  arrange(id, date_entree) %>% # tri chronologique
  group_by(id) %>%
  summarise(
    # On prend les valeurs du PREMIER séjour pour les infos fixes
    sexe_libelle = first(sexe_libelle),
    age_premier = first(age_sejour),
    tranche_age = first(tranche_age),
    diag_cancer = first(diag_cancer),
    groupe_cancer = first(groupe_cancer),
    diag_associe = first(diag_associe),
    diag_antecedents = first(diag_antecedents),
    # Pour les métastases, on indique "M1" s'il a eu le statut M1 à au moins un séjour
    metastase = ifelse(any(metastase == "M1", na.rm = TRUE), "M1", "M0"),
    a_comorbidite = first(a_comorbidite),
    a_antecedent = first(a_antecedent),
    mois_survie = first(mois_survie),
    evenement = first(evenement),
    date_deces = first(date_deces),
    stade_tnm = if ("stade_tnm" %in% names(.)) first(stade_tnm) else NA_character_,
    stade_num = first(stade_num),
    score_oms = if ("score_oms" %in% names(.)) first(score_oms) else NA_integer_,
    score_oms_libelle = first(score_oms_libelle),
    imc = if ("imc" %in% names(.)) first(imc) else NA_real_,
    cat_imc = first(cat_imc),
    tabac_paquet_an = if ("tabac_paquet_an" %in% names(.)) first(tabac_paquet_an) else NA_real_,
    ville = if ("ville" %in% names(.)) first(ville) else NA_character_,
    departement = if ("departement" %in% names(.)) first(departement) else NA_character_,
    code_postal = if ("code_postal" %in% names(.)) first(code_postal) else NA_character_,
    niveau_education = if ("niveau_education" %in% names(.)) first(niveau_education) else NA_character_,
    situation_familiale = if ("situation_familiale" %in% names(.)) first(situation_familiale) else NA_character_,
    type_pec_premier = first(type_pec),
    # Statistiques sur l'ensemble des séjours du patient
    nb_sejours = n(),
    duree_totale = sum(duree_sejour, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    est_readmis    = nb_sejours > 1,
    libelle_statut = ifelse(evenement == 1, "Décédé", "En vie")
  )


# ──────────────────────────────────────────────────────────
# 7. FONCTIONS UTILITAIRES
#
# Ces fonctions sont réutilisées dans server.R pour créer
# les graphiques et composants de l'interface.
# ──────────────────────────────────────────────────────────

# ── Thème graphique CEM ────────────────────────────────────
# theme_cem() définit l'apparence commune de tous les graphiques :
# police, couleurs de fond, grilles, etc.
theme_cem <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      # Fond blanc pour tous les graphiques
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # Grilles : seulement les grandes lignes, en gris très clair
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EAECEF", linewidth = 0.5),
      # Texte des axes
      axis.text = element_text(size = 10, color = "#555"),
      axis.title = element_text(size = 11, face = "bold", color = "#333"),
      # Titre du graphique en bleu CEM
      plot.title = element_text(
        size = 13, face = "bold", color = col_primary,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(size = 10, color = "gray50", margin = margin(b = 10)),
      # Légende en bas du graphique
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 15, 10, 10)
    )
}


# ── Graphique vide ────────────────────────────────────────
# empty_plot() affiche un message si les données sont insuffisantes.
# Cela évite que Shiny affiche une erreur laide quand les filtres
# ne retournent aucune donnée.
empty_plot <- function(msg = "Données insuffisantes pour ce graphique") {
  ggplot() +
    annotate("text",
      x = 0.5, y = 0.5, label = msg,
      size = 4.5, color = "gray60", fontface = "italic", hjust = 0.5
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}


# ── Bouton d'agrandissement ───────────────────────────────
# zoom_btn() crée un bouton qui permet d'ouvrir le graphique
# en plein écran dans une "modale" (fenêtre superposée).
#
# Paramètre : id = identifiant unique du graphique
zoom_btn <- function(id) {
  actionButton(
    paste0("zoom_", id),
    label = NULL,
    icon  = icon("expand"),
    class = "btn btn-sm btn-outline-secondary btn-zoom float-end",
    style = "padding:3px 8px;"
  )
}


# ── En-tête de card avec bouton zoom ─────────────────────
# card_header_with_zoom() combine le titre et le bouton zoom
# dans l'en-tête d'une card.
card_header_with_zoom <- function(titre, plot_id) {
  tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    tags$span(titre),
    zoom_btn(plot_id)
  )
}


# ── Spinner de chargement ─────────────────────────────────
# spin() entoure un graphique d'un spinner animé pendant le
# chargement. Améliore l'expérience utilisateur.
spin <- function(x) withSpinner(x, color = col_primary, size = 0.7, type = 6)


# ── Carte KPI principale ──────────────────────────────────
# kpi_box_principal() crée une grande carte affichant un indicateur clé.
#
# Paramètres :
#   titre    : nom de l'indicateur (ex: "Patients uniques")
#   id_valeur: identifiant Shiny pour la valeur (ex: "kpi_patients")
#   icone    : nom de l'icône Font Awesome (ex: "users")
#   couleur  : couleur de l'icône et de la valeur
kpi_box_principal <- function(titre, id_valeur, icone, couleur = col_primary) {
  div(
    class = "kpi-card-principale h-100",
    div(
      class = "kpi-ligne-haut",
      div(
        div(class = "kpi-label", titre),
        div(
          style = paste0("font-size:2.1rem; font-weight:800; color:", couleur, ";"),
          textOutput(id_valeur, inline = TRUE)
        )
      ),
      tags$i(
        class = paste0("fa-solid fa-", icone, " kpi-icone"),
        style = paste0("color:", couleur, ";")
      )
    )
  )
}


# ── Carte KPI secondaire ──────────────────────────────────
# kpi_box_secondaire() crée une petite carte pour les indicateurs
# moins prioritaires (2e rangée de KPIs).
kpi_box_secondaire <- function(titre, id_valeur, icone, couleur = col_subtext) {
  div(
    class = "kpi-card-secondaire h-100",
    div(
      class = "d-flex justify-content-between align-items-center",
      div(
        div(class = "kpi-label", titre),
        div(
          style = paste0("font-size:1.35rem; font-weight:700; color:", couleur, ";"),
          textOutput(id_valeur, inline = TRUE)
        )
      ),
      tags$i(
        class = paste0("fa-solid fa-", icone),
        style = paste0("color:", couleur, "; opacity:0.3; font-size:1.2rem;")
      )
    )
  )
}


# ──────────────────────────────────────────────────────────
# 8. THÈME BSLIB + FONCTIONS UI (login_ui, dashboard_ui)
#
# Définies ici dans global.R (et non dans ui.R) pour être
# accessibles depuis server.R dans renderUI(), car global.R
# est chargé dans l'environnement global Shiny partagé.
# ──────────────────────────────────────────────────────────

theme_cem_bslib <- bs_theme(
  bootswatch   = "flatly",
  primary      = "#005D92",
  success      = "#8CC63F",
  warning      = "#E6A817",
  danger       = "#C0392B",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

login_ui <- function() {
  div(
    class = "login-overlay",
    div(
      id = "login-card",
      class = "login-card",
      tags$img(src = "logo.svg", class = "login-logo", alt = "Logo CEM Rennes"),
      div(class = "login-titre", "Dashboard Oncologie"),
      div(class = "login-sous-titre", "Centre Eugene Marquis - Rennes"),
      hr(),
      div(
        class = "login-champ-groupe",
        tags$i(class = "fa-solid fa-user login-icone"),
        tags$input(
          id = "identifiant", type = "text", class = "form-control",
          placeholder = "Identifiant", autocomplete = "username"
        )
      ),
      div(
        class = "login-champ-groupe",
        tags$i(class = "fa-solid fa-lock login-icone"),
        tags$input(
          id = "mot_de_passe", type = "password", class = "form-control",
          placeholder = "Mot de passe", autocomplete = "current-password"
        )
      ),
      actionButton(
        "btn_connexion",
        label = tags$span(tags$i(
          class = "fa-solid fa-right-to-bracket",
          style = "margin-right:8px;"
        ), "Se connecter"),
        class = "btn-login"
      ),
      div(class = "login-erreur", textOutput("login_erreur_msg")),
      div(
        class = "login-footer",
        paste0(
          "Acces reserve au personnel medical - CEM Rennes - ",
          format(Sys.Date(), "%Y")
        )
      )
    )
  )
}

sidebar_filtres <- function() {
  sidebar(
    id = "app_sidebar",
    width = 275,
    bg = col_light,
    div(class = "sidebar-titre", tags$i(class = "fa-solid fa-sliders"), "Filtres"),
    hr(style = "margin:8px 0; border-color: #dee2e6;"),
    dateRangeInput("filtre_dates",
      label = "Periode d'admission",
      start = min(df$date_entree, na.rm = TRUE),
      end = max(df$date_entree, na.rm = TRUE),
      format = "dd/mm/yyyy", language = "fr"
    ),
    sliderInput("filtre_age",
      label = "Age a l'admission",
      min = 0, max = 100, value = c(0, 100), step = 1
    ),
    selectInput("filtre_sexe",
      label = "Sexe",
      choices = c("Tous" = "All", "Homme" = "1", "Femme" = "2")
    ),
    selectInput("filtre_pec",
      label = "Type de prise en charge",
      choices = c("Tous", sort(unique(df$type_pec[!is.na(df$type_pec)])))
    ),
    selectInput("filtre_diag",
      label = "Groupe de Cancer (Organes)",
      choices = c("Tous", sort(unique(df$groupe_cancer[!is.na(df$groupe_cancer)])))
    ),
    selectInput("filtre_meta",
      label = "Statut metastatique",
      choices = c("Tous", "M0", "M1")
    ),
    selectInput("filtre_stade",
      label = "Stade TNM",
      choices = c("Tous", "I", "II", "III", "IV")
    ),
    selectInput("filtre_dept",
      label = "Departement d'origine",
      choices = c("Tous", sort(unique(df$departement[!is.na(df$departement)])))
    ),
    hr(style = "margin:8px 0; border-color: #dee2e6;"),
    uiOutput("sidebar_resume"),
    actionButton("reset_filtres",
      label = tags$span(tags$i(
        class = "fa-solid fa-rotate-left",
        style = "margin-right:5px;"
      ), "Reinitialiser"),
      class = "btn btn-outline-secondary btn-sm w-100",
      id = "reset_filtres"
    )
  )
}

dashboard_ui <- function() {
  page_navbar(
    id = "main_navbar",
    title = tags$span(
      tags$img(
        src = "logo.svg", height = "26px",
        style = "margin-right:10px; filter:brightness(0) invert(1); opacity:0.88;"
      ),
      "Dashboard Oncologie"
    ),
    theme = theme_cem_bslib,
    navbar_options = navbar_options(bg = col_dark, theme = "dark"),
    header = tagList(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "cem.css"),
      useShinyjs()
    ),
    sidebar = sidebar_filtres(),

    # ══════════════════════════════════════════════════
    # PAGE D'ACCUEIL
    # Page d'atterrissage avec hero, stats rapides et navigation
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-house me-2"), "Accueil"),
      value = "accueil",

      # ── Hero ──────────────────────────────────────────────
      div(
        class = "accueil-hero",
        div(
          class = "accueil-hero-contenu",
          tags$img(src = "logo.svg", class = "accueil-hero-logo", alt = "Logo CEM"),
          tags$h1(class = "accueil-hero-titre", "Dashboard Oncologie"),
          tags$p(class = "accueil-hero-sous-titre", "Centre Eugene Marquis \u00b7 Rennes"),
          tags$p(
            class = "accueil-hero-description",
            "Plateforme d'analyse des donnees cliniques oncologiques.",
            tags$br(),
            "Selectionnez une section ci-dessous pour commencer."
          )
        )
      ),

      # ── Section navigation ──────────────────────────────
      div(
        class = "accueil-section-titre",
        tags$i(class = "fa-solid fa-compass me-2"),
        "Acceder aux analyses"
      ),

      # ── Grille de liens cliquables ─────────────────────
      div(
        class = "accueil-grid",
        actionLink("goto_vue", label = div(
          class = "accueil-nav-card accueil-nav-card-1",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-gauge-high accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Vue d'ensemble"),
          tags$p(class = "accueil-nav-card-desc", "Indicateurs cles, admissions, statut vital.")
        )),
        actionLink("goto_parcours", label = div(
          class = "accueil-nav-card accueil-nav-card-2",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-route accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Parcours & Durees"),
          tags$p(class = "accueil-nav-card-desc", "DMS, delais diagnostiques, prise en charge.")
        )),
        actionLink("goto_stats_cli", label = div(
          class = "accueil-nav-card accueil-nav-card-3",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-chart-bar accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Statistiques Cliniques"),
          tags$p(class = "accueil-nav-card-desc", "Cancers, comorbidites, pyramide des ages.")
        )),
        actionLink("goto_survie", label = div(
          class = "accueil-nav-card accueil-nav-card-4",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-heart-pulse accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Analyse de Survie"),
          tags$p(class = "accueil-nav-card-desc", "Kaplan-Meier, survie mediane par cancer.")
        )),
        actionLink("goto_risque", label = div(
          class = "accueil-nav-card accueil-nav-card-5",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-triangle-exclamation accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Profils de Risque"),
          tags$p(class = "accueil-nav-card-desc", "Mortalite, readmissions, heatmap antecedents.")
        )),
        actionLink("goto_stats", label = div(
          class = "accueil-nav-card accueil-nav-card-7",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-table accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Statistiques"),
          tags$p(class = "accueil-nav-card-desc", "Tableaux recapitulatifs, DMS, volumes.")
        )),
        actionLink("goto_enrichi", label = div(
          class = "accueil-nav-card accueil-nav-card-6",
          div(
            class = "accueil-nav-card-top",
            div(
              class = "accueil-nav-card-icon-wrap",
              tags$i(class = "fa-solid fa-map-location-dot accueil-nav-card-icon")
            ),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Donnees Enrichies"),
          tags$p(class = "accueil-nav-card-desc", "Geographie, stades TNM, OMS, protocoles.")
        ))
      ),

      # ── Footer ───────────────────────────────────────────
      div(
        class = "accueil-footer",
        tags$i(class = "fa-solid fa-shield-halved me-2"),
        paste0("Centre Eugene Marquis \u00b7 Rennes \u00b7 Donnees anonymisees \u00b7 ", format(Sys.Date(), "%Y"))
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-gauge-high me-2"), "Vue d'ensemble"),
      value = "vue_ensemble",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box_principal("Patients uniques", "kpi_patients", "users", col_primary),
        kpi_box_principal("Sejours totaux", "kpi_sejours", "hospital", col_accent),
        kpi_box_principal("DMS (jours)", "kpi_dms", "clock", col_warn),
        kpi_box_principal("Taux de mortalite", "kpi_mortalite", "heart-pulse", col_danger)
      ), br(),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box_secondaire("Age moyen (ans)", "kpi_age", "cake-candles", col_primary),
        kpi_box_secondaire("Taux M1", "kpi_taux_meta", "virus", col_danger),
        kpi_box_secondaire("Taux readmission", "kpi_readmis", "arrows-rotate", col_warn),
        kpi_box_secondaire("Delai diag->admit", "kpi_delai", "calendar-days", col_accent)
      ), br(),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header(card_header_with_zoom("Evolution des Admissions Mensuelles", "timeline_plot")),
          spin(plotlyOutput("timeline_plot", height = "320px"))
        ),
        card(
          card_header(card_header_with_zoom("Statut Vital des Patients", "status_plot")),
          spin(plotlyOutput("status_plot", height = "320px"))
        )
      ),
      card(card_header("Sejours Recents"), spin(DTOutput("patients_table")))
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-route me-2"), "Parcours & Durees"),
      value = "parcours",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("DMS par Type de Prise en Charge", "los_boxplot")),
          spin(plotOutput("los_boxplot", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Delai Diagnostic -> Admission (mois)", "tta_hist")),
          spin(plotOutput("tta_hist", height = "380px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Age x Duree de Sejour (par Metastase)", "age_los_scatter")),
          spin(plotOutput("age_los_scatter", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Categories de Duree de Sejour par Sexe", "los_cat_plot")),
          spin(plotOutput("los_cat_plot", height = "380px"))
        )
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-chart-bar me-2"), "Statistiques Cliniques"),
      value = "stats_cliniques",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Cancers Principaux - Top 10", "cancer_bar_plot")),
          spin(plotlyOutput("cancer_bar_plot", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Comorbidites Associees - Top 10", "assoc_bar_plot")),
          spin(plotOutput("assoc_bar_plot", height = "380px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Pyramide des Ages (Hommes / Femmes)", "age_pyramid")),
          spin(plotOutput("age_pyramid", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Repartition M0 / M1 par Cancer", "meta_by_cancer")),
          spin(plotOutput("meta_by_cancer", height = "380px"))
        )
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-heart-pulse me-2"), "Analyse de Survie"),
      value = "survie",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Kaplan-Meier Global", "km_global")),
          spin(plotOutput("km_global", height = "420px"))
        ),
        card(
          card_header(card_header_with_zoom("KM : M0 vs M1", "km_meta")),
          spin(plotOutput("km_meta", height = "420px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("KM : Homme vs Femme", "km_gender")),
          spin(plotOutput("km_gender", height = "420px"))
        ),
        card(
          card_header(card_header_with_zoom("Survie Mediane par Cancer (Top 8)", "median_survival_bar")),
          spin(plotOutput("median_survival_bar", height = "420px"))
        )
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-triangle-exclamation me-2"), "Profils de Risque"),
      value = "risque",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Heatmap : Antecedents x Cancer", "heatmap_risk")),
          spin(plotOutput("heatmap_risk", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Mortalite par Cancer & Tranche d'Age", "mortality_age_cancer")),
          spin(plotOutput("mortality_age_cancer", height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Taux de Readmission par Cancer", "readmit_by_cancer")),
          spin(plotOutput("readmit_by_cancer", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Distribution du Nombre de Sejours", "stays_hist")),
          spin(plotOutput("stays_hist", height = "400px"))
        )
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-table me-2"), "Statistiques"),
      value = "statistiques",
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Resume Statistique par Cancer"), spin(DTOutput("stats_table"))),
        card(
          card_header(card_header_with_zoom("DMS Mediane & IQR par Prise en Charge", "dms_ci_plot")),
          spin(plotOutput("dms_ci_plot", height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Volume d'Activite par PEC & Annee", "pec_year_heatmap")),
          spin(plotOutput("pec_year_heatmap", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Distribution Delai Diag -> Deces par Cancer", "tta_death_plot")),
          spin(plotOutput("tta_death_plot", height = "400px"))
        )
      )
    ),
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-map-location-dot me-2"), "Donnees Enrichies"),
      value = "enrichies",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Patients par Departement (Bretagne)", "dept_bar")),
          spin(plotlyOutput("dept_bar", height = "350px"))
        ),
        card(
          card_header(card_header_with_zoom("Distribution des Stades TNM par Cancer", "stade_bar")),
          spin(plotOutput("stade_bar", height = "350px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Score OMS par Tranche d'Age (Heatmap)", "oms_heatmap")),
          spin(plotOutput("oms_heatmap", height = "350px"))
        ),
        card(
          card_header(card_header_with_zoom("IMC Moyen par Type de Prise en Charge", "imc_plot")),
          spin(plotOutput("imc_plot", height = "350px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Protocoles Therapeutiques - Top 12", "protocole_bar")),
          spin(plotlyOutput("protocole_bar", height = "350px"))
        ),
        card(
          card_header(card_header_with_zoom("Tabagisme (paquets-annees) par Cancer", "tabac_boxplot")),
          spin(plotOutput("tabac_boxplot", height = "350px"))
        )
      )
    ),
    nav_spacer(),
    nav_item(
      div(
        style = "display:flex; align-items:center; gap:10px; padding-right:10px;",
        div(
          class = "badge-utilisateur",
          tags$i(class = "fa-solid fa-circle-user"),
          textOutput("badge_utilisateur", inline = TRUE)
        ),
        actionButton("btn_deconnexion",
          label = tags$span(tags$i(
            class = "fa-solid fa-right-from-bracket",
            style = "margin-right:5px;"
          ), "Deconnexion"),
          class = "btn-deconnexion"
        )
      )
    )
  )
}
