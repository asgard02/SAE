# ============================================================
# ui.R — Interface Utilisateur (ce que voit l'utilisateur)
# CEM Rennes – Dashboard Oncologie v3
#
# Ce fichier définit la STRUCTURE VISUELLE de l'application.
# Il ne contient pas de logique de calcul (c'est dans server.R).
#
# Deux "pages" sont définies ici :
#   1. login_ui()      → page de connexion
#   2. dashboard_ui()  → le dashboard principal (après connexion)
#
# La sélection entre les deux se fait dans server.R
# via output$page_principale.
# ============================================================


# ──────────────────────────────────────────────────────────
# THÈME BSLIB
#
# bslib permet de personnaliser Bootstrap (le framework CSS)
# directement depuis R. On définit ici les couleurs et polices
# qui s'appliquent à tous les composants Bootstrap.
# ──────────────────────────────────────────────────────────

theme_cem_bslib <- bs_theme(
  bootswatch   = "flatly", # Base : thème Flatly (sobre, professionnel)
  primary      = "#005D92", # Bleu CEM officiel
  success      = "#8CC63F", # Vert accent
  warning      = "#E6A817", # Orange
  danger       = "#C0392B", # Rouge
  base_font    = font_google("Inter"), # Police moderne (chargée depuis Google Fonts)
  heading_font = font_google("Inter")
)


# ──────────────────────────────────────────────────────────
# PAGE DE CONNEXION
#
# Cette fonction renvoie le HTML de la page de connexion.
# Elle est appelée depuis server.R quand l'utilisateur
# n'est pas encore connecté.
# ──────────────────────────────────────────────────────────

login_ui <- function() {
  div(
    class = "login-overlay",

    # ── Carte glassmorphism centrale ──────────────────────
    div(
      id = "login-card",
      class = "login-card",

      # Logo CEM (conteneur pour assurer l'affichage)
      div(
        class = "login-logo-wrap",
        tags$img(
          src   = "/logo.png",
          class = "login-logo",
          alt   = "Logo CEM Rennes"
        )
      ),

      # Titre et sous-titre
      div(class = "login-titre", "Dashboard Oncologie"),
      div(class = "login-sous-titre", "Centre Eugène Marquis · Rennes"),
      hr(),

      # ── Champ identifiant ─────────────────────────────
      div(
        class = "login-champ-groupe",
        tags$i(class = "fa-solid fa-user login-icone"),
        tags$input(
          id = "identifiant",
          type = "text",
          class = "form-control",
          placeholder = "Identifiant",
          autocomplete = "username"
        )
      ),

      # ── Champ mot de passe ────────────────────────────
      div(
        class = "login-champ-groupe",
        tags$i(class = "fa-solid fa-lock login-icone"),
        tags$input(
          id = "mot_de_passe",
          type = "password",
          class = "form-control",
          placeholder = "Mot de passe",
          autocomplete = "current-password"
        )
      ),

      # ── Bouton de connexion ───────────────────────────
      # actionButton() crée un bouton Shiny que server.R peut écouter
      actionButton(
        "btn_connexion",
        label = tags$span(
          tags$i(class = "fa-solid fa-right-to-bracket", style = "margin-right:8px;"),
          "Se connecter"
        ),
        class = "btn-login"
      ),

      # ── Message d'erreur (vide par défaut) ───────────
      # textOutput() affiche du texte dynamique géré par server.R
      div(class = "login-erreur", textOutput("login_erreur_msg")),

      # ── Pied de page ──────────────────────────────────
      div(
        class = "login-footer",
        paste0("Accès réservé au personnel médical · CEM Rennes · ", format(Sys.Date(), "%Y"))
      )
    )
  )
}


# ──────────────────────────────────────────────────────────
# SIDEBAR (panneau de filtres)
#
# Cette fonction crée le panneau latéral gauche commun
# à tous les onglets du dashboard.
# ──────────────────────────────────────────────────────────

sidebar_filtres <- function() {
  sidebar(
    width = 275,
    bg = col_light,

    # Titre de la sidebar
    div(
      class = "sidebar-titre",
      tags$i(class = "fa-solid fa-sliders"),
      "Filtres"
    ),
    hr(style = "margin:8px 0; border-color: #dee2e6;"),

    # ── Filtre : période d'admission ──────────────────
    # dateRangeInput() crée deux champs date (début + fin)
    dateRangeInput(
      "filtre_dates",
      label = "Période d'admission",
      start = min(df$date_entree, na.rm = TRUE),
      end = max(df$date_entree, na.rm = TRUE),
      format = "dd/mm/yyyy",
      language = "fr"
    ),

    # ── Filtre : âge ──────────────────────────────────
    # sliderInput() crée une barre glissante à deux poignées
    sliderInput(
      "filtre_age",
      label = "Âge à l'admission",
      min = 0, max = 100, value = c(0, 100), step = 1
    ),

    # ── Filtre : sexe ─────────────────────────────────
    selectInput(
      "filtre_sexe",
      label   = "Sexe",
      choices = c("Tous" = "All", "Homme" = "1", "Femme" = "2")
    ),

    # ── Filtre : type de prise en charge ──────────────
    selectInput(
      "filtre_pec",
      label   = "Type de prise en charge",
      choices = c("Tous", sort(unique(df$type_pec[!is.na(df$type_pec)])))
    ),

    # ── Filtre : diagnostic principal ─────────────────
    selectInput(
      "filtre_diag",
      label   = "Diagnostic principal",
      choices = c("Tous", sort(unique(df$diag_cancer[!is.na(df$diag_cancer)])))
    ),

    # ── Filtre : statut métastatique ──────────────────
    selectInput(
      "filtre_meta",
      label   = "Statut métastatique",
      choices = c("Tous", "M0", "M1")
    ),

    # ── Filtre : stade TNM (nouveau) ──────────────────
    selectInput(
      "filtre_stade",
      label   = "Stade TNM",
      choices = c("Tous", "I", "II", "III", "IV")
    ),

    # ── Filtre : département (nouveau) ────────────────
    selectInput(
      "filtre_dept",
      label   = "Département d'origine",
      choices = c("Tous", sort(unique(df$departement[!is.na(df$departement)])))
    ),
    hr(style = "margin:8px 0; border-color: #dee2e6;"),

    # ── Résumé dynamique des données filtrées ─────────
    # uiOutput() est rempli par server.R avec les chiffres actuels
    uiOutput("sidebar_resume"),

    # ── Bouton réinitialiser ───────────────────────────
    actionButton(
      "reset_filtres",
      label = tags$span(
        tags$i(class = "fa-solid fa-rotate-left", style = "margin-right:5px;"),
        "Réinitialiser"
      ),
      class = "btn btn-outline-secondary btn-sm w-100",
      id = "reset_filtres"
    )
  )
}


# ──────────────────────────────────────────────────────────
# DASHBOARD PRINCIPAL
#
# Cette fonction renvoie le HTML complet du dashboard.
# Elle est appelée depuis server.R après connexion réussie.
# ──────────────────────────────────────────────────────────

dashboard_ui <- function() {
  page_navbar(
    id = "main_navbar",
    title = tags$span(
      class = "navbar-brand-content",
      tags$img(
        src = "/logo.png",
        alt = "Centre Eugène Marquis Rennes",
        class = "navbar-logo-img",
        style = "height:38px; width:auto; margin-right:12px; vertical-align:middle; object-fit:contain;"
      ),
      "Dashboard Oncologie"
    ),
    theme = theme_cem_bslib,
    navbar_options = navbar_options(bg = col_dark, theme = "dark"),

    # ── Ressources CSS et JS ──────────────────────────
    header = tagList(
      # Font Awesome pour les icônes
      tags$link(
        rel  = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      # Notre feuille de style CEM
      tags$link(rel = "stylesheet", type = "text/css", href = "cem.css"),
      # useShinyjs() active les fonctions JavaScript de shinyjs
      useShinyjs()
    ),

    # ── Sidebar partagée entre tous les onglets ────────
    sidebar = sidebar_filtres(),

    # ══════════════════════════════════════════════════
    # ONGLET 0 : Accueil
    # Page d'atterrissage avec hero, stats et navigation
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-house me-2"), "Accueil"),
      value = "accueil",

      # ── Hero ──────────────────────────────────────────────
      div(
        class = "accueil-hero",
        div(
          class = "accueil-hero-contenu",
          tags$img(src = "/logo.png", class = "accueil-hero-logo", alt = "Logo CEM"),
          tags$h1(class = "accueil-hero-titre", "Dashboard Oncologie"),
          tags$p(class = "accueil-hero-sous-titre", "Centre Eugène Marquis · Rennes"),
          tags$p(
            class = "accueil-hero-description",
            "Plateforme d'analyse des données cliniques oncologiques.",
            tags$br(),
            "Sélectionnez une section ci-dessous pour commencer."
          )
        )
      ),

      # ── Bande de chiffres clés ─────────────────────────────
      div(
        class = "accueil-stats-bande",
        div(
          class = "accueil-stat-item",
          tags$i(class = "fa-solid fa-users accueil-stat-icone"),
          div(class = "accueil-stat-valeur", textOutput("accueil_nb_patients", inline = TRUE)),
          div(class = "accueil-stat-label", "Patients")
        ),
        div(
          class = "accueil-stat-item",
          tags$i(class = "fa-solid fa-hospital accueil-stat-icone"),
          div(class = "accueil-stat-valeur", textOutput("accueil_nb_sejours", inline = TRUE)),
          div(class = "accueil-stat-label", "Séjours")
        ),
        div(
          class = "accueil-stat-item",
          tags$i(class = "fa-solid fa-dna accueil-stat-icone"),
          div(class = "accueil-stat-valeur", textOutput("accueil_nb_cancers", inline = TRUE)),
          div(class = "accueil-stat-label", "Types de cancer")
        ),
        div(
          class = "accueil-stat-item",
          tags$i(class = "fa-solid fa-calendar-days accueil-stat-icone"),
          div(class = "accueil-stat-valeur", textOutput("accueil_periode", inline = TRUE)),
          div(class = "accueil-stat-label", "Période")
        )
      ),

      # ── Section navigation ──────────────────────────────
      div(
        class = "accueil-section-titre",
        tags$i(class = "fa-solid fa-compass me-2"),
        "Accéder aux analyses"
      ),

      # ── Grille de liens cliquables ─────────────────────
      div(
        class = "accueil-grid",
        actionLink("goto_vue", label = div(
          class = "accueil-nav-card accueil-nav-card-1",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-gauge-high accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Vue d'ensemble"),
          tags$p(class = "accueil-nav-card-desc", "Indicateurs clés, admissions, statut vital.")
        )),
        actionLink("goto_parcours", label = div(
          class = "accueil-nav-card accueil-nav-card-2",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-route accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Parcours & Durées"),
          tags$p(class = "accueil-nav-card-desc", "DMS, délais diagnostiques, prise en charge.")
        )),
        actionLink("goto_stats_cli", label = div(
          class = "accueil-nav-card accueil-nav-card-3",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-chart-bar accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Statistiques Cliniques"),
          tags$p(class = "accueil-nav-card-desc", "Cancers, comorbidités, pyramide des âges.")
        )),
        actionLink("goto_survie", label = div(
          class = "accueil-nav-card accueil-nav-card-4",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-heart-pulse accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Analyse de Survie"),
          tags$p(class = "accueil-nav-card-desc", "Kaplan-Meier, survie médiane par cancer.")
        )),
        actionLink("goto_risque", label = div(
          class = "accueil-nav-card accueil-nav-card-5",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-triangle-exclamation accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Profils de Risque"),
          tags$p(class = "accueil-nav-card-desc", "Mortalité, réadmissions, heatmap antécédents.")
        )),
        actionLink("goto_stats", label = div(
          class = "accueil-nav-card accueil-nav-card-7",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-table accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Statistiques"),
          tags$p(class = "accueil-nav-card-desc", "Tableaux récapitulatifs, DMS, volumes.")
        )),
        actionLink("goto_enrichi", label = div(
          class = "accueil-nav-card accueil-nav-card-6",
          div(
            class = "accueil-nav-card-top",
            div(class = "accueil-nav-card-icon-wrap", tags$i(class = "fa-solid fa-map-location-dot accueil-nav-card-icon")),
            tags$i(class = "fa-solid fa-arrow-right accueil-nav-card-arrow")
          ),
          tags$h3(class = "accueil-nav-card-titre", "Données Enrichies"),
          tags$p(class = "accueil-nav-card-desc", "Géographie, stades TNM, OMS, protocoles.")
        ))
      ),

      # ── Footer ───────────────────────────────────────────
      div(
        class = "accueil-footer",
        tags$i(class = "fa-solid fa-shield-halved me-2"),
        paste0("Centre Eugène Marquis · Rennes · Données anonymisées · ", format(Sys.Date(), "%Y"))
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 1 : Vue d'ensemble
    # Indicateurs clés + évolution + table récente
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-gauge-high me-2"), "Vue d'ensemble"),
      value = "vue_ensemble",

      # ── 4 KPIs principaux ────────────────────────────
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box_principal("Patients uniques", "kpi_patients", "users", col_primary),
        kpi_box_principal("Séjours totaux", "kpi_sejours", "hospital", col_accent),
        kpi_box_principal("DMS (jours)", "kpi_dms", "clock", col_warn),
        kpi_box_principal("Taux de mortalité", "kpi_mortalite", "heart-pulse", col_danger)
      ),
      br(),

      # ── 4 KPIs secondaires (moins proéminents) ───────
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box_secondaire("Âge moyen (ans)", "kpi_age", "cake-candles", col_primary),
        kpi_box_secondaire("Taux M1", "kpi_taux_meta", "virus", col_danger),
        kpi_box_secondaire("Taux réadmission", "kpi_readmis", "arrows-rotate", col_warn),
        kpi_box_secondaire("Délai diag→admit", "kpi_delai", "calendar-days", col_accent)
      ),
      br(),

      # ── Graphiques de la vue d'ensemble ───────────────
      layout_columns(
        col_widths = c(8, 4),
        # Timeline interactive (plotly) → hover sur chaque mois
        card(
          card_header(card_header_with_zoom("Évolution des Admissions Mensuelles", "timeline_plot")),
          spin(plotlyOutput("timeline_plot", height = "320px"))
        ),
        # Donut statut vital (plotly) → hover avec N et %
        card(
          card_header(card_header_with_zoom("Statut Vital des Patients", "status_plot")),
          spin(plotlyOutput("status_plot", height = "320px"))
        )
      ),

      # ── Table des patients récents ─────────────────────
      card(
        card_header("Séjours Récents"),
        spin(DTOutput("patients_table"))
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 2 : Parcours & Durées
    # Analyse des durées de séjour et des délais
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-route me-2"), "Parcours & Durées"),
      value = "parcours",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("DMS par Type de Prise en Charge", "los_boxplot")),
          spin(plotOutput("los_boxplot", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Délai Diagnostic → Admission (mois)", "tta_hist")),
          spin(plotOutput("tta_hist", height = "380px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Âge × Durée de Séjour (par Métastase)", "age_los_scatter")),
          spin(plotOutput("age_los_scatter", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Catégories de Durée de Séjour par Sexe", "los_cat_plot")),
          spin(plotOutput("los_cat_plot", height = "380px"))
        )
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 3 : Statistiques Cliniques
    # Distribution des cancers, comorbidités, pyramide des âges
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-chart-bar me-2"), "Statistiques Cliniques"),
      value = "stats_cliniques",
      layout_columns(
        col_widths = c(6, 6),
        # Cancer bar (plotly interactif → clic pour cross-filter)
        card(
          card_header(card_header_with_zoom("Cancers Principaux – Top 10", "cancer_bar_plot")),
          spin(plotlyOutput("cancer_bar_plot", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Comorbidités Associées – Top 10", "assoc_bar_plot")),
          spin(plotOutput("assoc_bar_plot", height = "380px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Pyramide des Âges (Hommes / Femmes)", "age_pyramid")),
          spin(plotOutput("age_pyramid", height = "380px"))
        ),
        card(
          card_header(card_header_with_zoom("Répartition M0 / M1 par Cancer", "meta_by_cancer")),
          spin(plotOutput("meta_by_cancer", height = "380px"))
        )
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 4 : Analyse de Survie
    # Courbes de Kaplan-Meier
    # ══════════════════════════════════════════════════
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
          card_header(card_header_with_zoom("Survie Médiane par Cancer (Top 8)", "median_survival_bar")),
          spin(plotOutput("median_survival_bar", height = "420px"))
        )
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 5 : Profils de Risque
    # Heatmaps et corrélations de facteurs de risque
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-triangle-exclamation me-2"), "Profils de Risque"),
      value = "risque",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Heatmap : Antécédents × Cancer", "heatmap_risk")),
          spin(plotOutput("heatmap_risk", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Mortalité par Cancer & Tranche d'Âge", "mortality_age_cancer")),
          spin(plotOutput("mortality_age_cancer", height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Taux de Réadmission par Cancer", "readmit_by_cancer")),
          spin(plotOutput("readmit_by_cancer", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Distribution du Nombre de Séjours", "stays_hist")),
          spin(plotOutput("stays_hist", height = "400px"))
        )
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 6 : Statistiques
    # Tableau récapitulatif + analyses opérationnelles
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-table me-2"), "Statistiques"),
      value = "statistiques",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Résumé Statistique par Cancer"),
          # DTOutput() sans scrollX → pas de barre de défilement
          spin(DTOutput("stats_table"))
        ),
        card(
          card_header(card_header_with_zoom("DMS Médiane & IQR par Prise en Charge", "dms_ci_plot")),
          spin(plotOutput("dms_ci_plot", height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Volume d'Activité par PEC & Année", "pec_year_heatmap")),
          spin(plotOutput("pec_year_heatmap", height = "400px"))
        ),
        card(
          card_header(card_header_with_zoom("Distribution Délai Diag → Décès par Cancer", "tta_death_plot")),
          spin(plotOutput("tta_death_plot", height = "400px"))
        )
      )
    ),

    # ══════════════════════════════════════════════════
    # ONGLET 7 : Données Enrichies & Géographie
    # Nouvelles colonnes : codes postaux, stade TNM, OMS, IMC
    # Ces visualisations inspirent les statisticiens sur les
    # analyses supplémentaires possibles avec ce jeu de données.
    # ══════════════════════════════════════════════════
    nav_panel(
      title = tags$span(tags$i(class = "fa-solid fa-map-location-dot me-2"), "Données Enrichies"),
      value = "enrichies",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(card_header_with_zoom("Patients par Département (Bretagne)", "dept_bar")),
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
          card_header(card_header_with_zoom("Score OMS par Tranche d'Âge (Heatmap)", "oms_heatmap")),
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
          card_header(card_header_with_zoom("Protocoles Thérapeutiques – Top 12", "protocole_bar")),
          spin(plotlyOutput("protocole_bar", height = "350px"))
        ),
        card(
          card_header(card_header_with_zoom("Tabagisme (paquets-années) par Cancer", "tabac_boxplot")),
          spin(plotOutput("tabac_boxplot", height = "350px"))
        )
      )
    ),

    # ── Espace flexible + info utilisateur + déconnexion ─
    nav_spacer(),
    nav_item(
      div(
        style = "display:flex; align-items:center; gap:10px; padding-right:10px;",
        # Badge affichant l'identifiant et le rôle de l'utilisateur connecté
        div(
          class = "badge-utilisateur",
          tags$i(class = "fa-solid fa-circle-user"),
          textOutput("badge_utilisateur", inline = TRUE)
        ),
        # Bouton de déconnexion
        actionButton(
          "btn_deconnexion",
          label = tags$span(
            tags$i(class = "fa-solid fa-right-from-bracket", style = "margin-right:5px;"),
            "Déconnexion"
          ),
          class = "btn-deconnexion"
        )
      )
    )
  )
}


# ──────────────────────────────────────────────────────────
# UI PRINCIPALE
#
# La variable "ui" est ce que Shiny cherche dans ui.R.
# Ici, c'est juste un placeholder : le vrai contenu est
# rendu dynamiquement par server.R selon l'état de connexion.
#
# uiOutput("page_principale") dit à Shiny :
# "affiche ici ce que server.R mettra dans output$page_principale"
# ──────────────────────────────────────────────────────────

ui <- fluidPage(
  # On a besoin du thème bslib pour le login aussi
  theme = theme_cem_bslib,

  # Ressources chargées dès le départ (avant connexion)
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "cem.css"),
    useShinyjs()
  ),

  # Page rendue dynamiquement (login ou dashboard)
  uiOutput("page_principale")
)
