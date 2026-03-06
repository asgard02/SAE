# ============================================================
# server.R — Logique Serveur
# CEM Rennes – Dashboard Oncologie v3
#
# Ce fichier contient TOUTE la logique de l'application :
#   - Authentification (connexion / déconnexion)
#   - Filtrage des données (sidebar)
#   - Calcul des KPIs
#   - Création de tous les graphiques
#   - Tableaux interactifs
#   - Modales de zoom
#
# En Shiny, le "server" est une fonction qui reçoit :
#   input   → ce que l'utilisateur clique/saisit
#   output  → ce qu'on veut afficher (graphiques, textes...)
#   session → informations sur la session courante
# ============================================================

server <- function(input, output, session) {
  # ──────────────────────────────────────────────────────
  # SECTION 1 : AUTHENTIFICATION
  #
  # reactiveValues() crée un "état" de l'application qui peut
  # changer au fil du temps. Ici on stocke si l'utilisateur
  # est connecté, son identifiant et son rôle.
  # ──────────────────────────────────────────────────────

  # État initial : non connecté
  auth <- reactiveValues(
    connecte = FALSE,
    identifiant = NULL,
    role = NULL
  )

  # ── Page affichée selon l'état de connexion ───────────
  # renderUI() génère du HTML dynamiquement.
  # Si l'utilisateur est connecté → dashboard, sinon → login.
  output$page_principale <- renderUI({
    if (!auth$connecte) {
      login_ui() # Défini dans ui.R
    } else {
      dashboard_ui() # Défini dans ui.R
    }
  })

  # ── Réaction au clic sur "Se connecter" ───────────────
  # observeEvent() s'exécute à chaque fois que btn_connexion
  # est cliqué par l'utilisateur.
  observeEvent(input$btn_connexion, {
    # Récupérer ce que l'utilisateur a saisi (et supprimer les espaces)
    saisie_id <- trimws(input$identifiant)
    saisie_mdp <- input$mot_de_passe

    # Vérifier si l'identifiant existe ET si le mot de passe correspond
    if (
      saisie_id %in% names(IDENTIFIANTS) &&
        IDENTIFIANTS[[saisie_id]]$mdp == saisie_mdp
    ) {
      # Connexion réussie : mettre à jour l'état
      auth$connecte <- TRUE
      auth$identifiant <- saisie_id
      auth$role <- IDENTIFIANTS[[saisie_id]]$role
    } else {
      # Connexion échouée : afficher l'erreur et secouer la carte
      output$login_erreur_msg <- renderText({
        "Identifiant ou mot de passe incorrect."
      })
      # Animation "secousse" via shinyjs (définie dans cem.css @keyframes secousse)
      shinyjs::runjs("
        var carte = document.getElementById('login-card');
        if (carte) {
          carte.classList.remove('secousse');
          void carte.offsetWidth; // force reflow (nécessaire pour réinitialiser l'animation)
          carte.classList.add('secousse');
        }
      ")
    }
  })

  # ── Réaction au clic sur "Déconnexion" ────────────────
  observeEvent(input$btn_deconnexion, {
    auth$connecte <- FALSE
    auth$identifiant <- NULL
    auth$role <- NULL
  })

  # ── Badge utilisateur dans la navbar ──────────────────
  # Affiche "identifiant — rôle" en haut à droite du dashboard
  output$badge_utilisateur <- renderText({
    req(auth$connecte)
    paste0(auth$identifiant, " — ", auth$role)
  })

  # ── Message d'erreur de connexion (vide par défaut) ───
  output$login_erreur_msg <- renderText({
    ""
  })


  # ──────────────────────────────────────────────────────
  # SECTION 1b : PAGE D'ACCUEIL — Chiffres clés
  # Ces renderText fournissent les valeurs affichées
  # sur la page d'accueil (bande de stats rapides).
  # Ils utilisent les données globales (non filtrées).
  # ──────────────────────────────────────────────────────

  output$accueil_nb_patients <- renderText({
    format(n_distinct(df$id), big.mark = " ")
  })

  output$accueil_nb_sejours <- renderText({
    format(nrow(df), big.mark = " ")
  })

  output$accueil_nb_cancers <- renderText({
    as.character(n_distinct(df$diag_cancer[!is.na(df$diag_cancer)]))
  })

  output$accueil_periode <- renderText({
    paste0(
      format(min(df$date_entree, na.rm = TRUE), "%Y"),
      " - ",
      format(max(df$date_entree, na.rm = TRUE), "%Y")
    )
  })

  # ──────────────────────────────────────────────────────
  # SECTION 1c : NAVIGATION DEPUIS L'ACCUEIL
  # Chaque actionLink de la page d'accueil navigue
  # vers l'onglet correspondant via nav_select().
  # ──────────────────────────────────────────────────────

  observeEvent(input$goto_vue, {
    nav_select("main_navbar", "vue_ensemble")
  })
  observeEvent(input$goto_parcours, {
    nav_select("main_navbar", "parcours")
  })
  observeEvent(input$goto_stats_cli, {
    nav_select("main_navbar", "stats_cliniques")
  })
  observeEvent(input$goto_survie, {
    nav_select("main_navbar", "survie")
  })
  observeEvent(input$goto_risque, {
    nav_select("main_navbar", "risque")
  })
  observeEvent(input$goto_stats, {
    nav_select("main_navbar", "statistiques")
  })
  observeEvent(input$goto_enrichi, {
    nav_select("main_navbar", "enrichies")
  })

  # Masquer la sidebar (filtres) lorsque l'utilisateur est sur l'accueil
  observe({
    req(input$main_navbar)
    if (input$main_navbar == "accueil") {
      shinyjs::addClass(selector = "body", class = "hide-sidebar")
    } else {
      shinyjs::removeClass(selector = "body", class = "hide-sidebar")
    }
  })

  # ──────────────────────────────────────────────────────
  # SECTION 2 : FILTRES (DONNÉES RÉACTIVES)
  #
  # "reactive()" crée une valeur qui se recalcule automatiquement
  # à chaque fois qu'un filtre de la sidebar change.
  #
  # Toutes les fonctions de graphiques utiliseront
  # donnees_filtrees() et patients_filtres() au lieu de df/df_patients.
  # ──────────────────────────────────────────────────────

  # ── Données séjours filtrées ──────────────────────────
  donnees_filtrees <- reactive({
    # S'assurer que l'utilisateur est connecté avant de filtrer
    req(auth$connecte)
    # S'assurer que les dates sont valides
    req(input$filtre_dates)

    tryCatch(
      {
        d <- df %>%
          filter(
            date_entree >= input$filtre_dates[1],
            date_entree <= input$filtre_dates[2],
            age_sejour >= input$filtre_age[1],
            age_sejour <= input$filtre_age[2]
          )

        # Filtres optionnels : on ne filtre que si ce n'est pas "Tous"
        if (input$filtre_sexe != "All") {
          d <- d %>% filter(sexe == as.numeric(input$filtre_sexe))
        }
        if (input$filtre_pec != "Tous") {
          d <- d %>% filter(type_pec == input$filtre_pec)
        }
        if (input$filtre_diag != "Tous") {
          d <- d %>% filter(groupe_cancer == input$filtre_diag)
        }
        if (input$filtre_meta != "Tous") {
          d <- d %>% filter(metastase == input$filtre_meta)
        }
        if (input$filtre_stade != "Tous") {
          d <- d %>% filter(stade_tnm == input$filtre_stade)
        }
        if (input$filtre_dept != "Tous") {
          d <- d %>% filter(departement == input$filtre_dept)
        }

        d
      },
      error = function(e) data.frame()
    )
  })

  # ── Patients filtrés (une ligne par patient) ──────────
  patients_filtres <- reactive({
    req(auth$connecte)
    tryCatch(
      {
        ids <- unique(donnees_filtrees()$id)
        df_patients %>% filter(id %in% ids)
      },
      error = function(e) data.frame()
    )
  })

  # ── Réinitialiser tous les filtres ────────────────────
  observeEvent(input$reset_filtres, {
    updateDateRangeInput(session, "filtre_dates",
      start = min(df$date_entree, na.rm = TRUE),
      end   = max(df$date_entree, na.rm = TRUE)
    )
    updateSliderInput(session, "filtre_age", value = c(0, 100))
    updateSelectInput(session, "filtre_sexe", selected = "All")
    updateSelectInput(session, "filtre_pec", selected = "Tous")
    updateSelectInput(session, "filtre_diag", selected = "Tous")
    updateSelectInput(session, "filtre_meta", selected = "Tous")
    updateSelectInput(session, "filtre_stade", selected = "Tous")
    updateSelectInput(session, "filtre_dept", selected = "Tous")
  })

  # ── Résumé dans la sidebar (nb patients, séjours) ─────
  output$sidebar_resume <- renderUI({
    req(auth$connecte, nrow(donnees_filtrees()) > 0)
    div(
      class = "sidebar-resume",
      tags$b(n_distinct(donnees_filtrees()$id)), " patients",
      tags$br(),
      tags$b(nrow(donnees_filtrees())), " séjours"
    )
  })

  # ──────────────────────────────────────────────────────
  # SECTION 3 : KPIs
  #
  # renderText() produit du texte qui s'affiche dans les
  # KPI cards définies dans ui.R.
  #
  # req() arrête l'exécution si les données sont vides
  # (évite les erreurs quand les filtres ne trouvent rien).
  # ──────────────────────────────────────────────────────

  # Nombre de patients uniques
  output$kpi_patients <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        format(n_distinct(d$id), big.mark = " ")
      },
      error = function(e) "Pas de données"
    )
  })

  # Nombre total de séjours
  output$kpi_sejours <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        format(nrow(d), big.mark = " ")
      },
      error = function(e) "Pas de données"
    )
  })

  # Durée Moyenne de Séjour (DMS) en jours
  output$kpi_dms <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        round(mean(d$duree_sejour, na.rm = TRUE), 1)
      },
      error = function(e) "Pas de données"
    )
  })

  # Taux de mortalité (% de patients décédés)
  output$kpi_mortalite <- renderText({
    tryCatch(
      {
        pts <- patients_filtres()
        if (is.null(pts) || nrow(pts) == 0) {
          return("Pas de données")
        }
        paste0(round(mean(pts$evenement, na.rm = TRUE) * 100, 1), "%")
      },
      error = function(e) "Pas de données"
    )
  })

  # Âge moyen
  output$kpi_age <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        round(mean(d$age_sejour, na.rm = TRUE), 1)
      },
      error = function(e) "Pas de données"
    )
  })

  # Taux de métastases (M1)
  output$kpi_taux_meta <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        d_uniq <- d %>% distinct(id, .keep_all = TRUE)
        paste0(round(mean(d_uniq$metastase == "M1", na.rm = TRUE) * 100, 1), "%")
      },
      error = function(e) "Pas de données"
    )
  })

  # Taux de réadmission
  output$kpi_readmis <- renderText({
    tryCatch(
      {
        pts <- patients_filtres()
        if (is.null(pts) || nrow(pts) == 0) {
          return("Pas de données")
        }
        paste0(round(mean(pts$est_readmis, na.rm = TRUE) * 100, 1), "%")
      },
      error = function(e) "Pas de données"
    )
  })

  # Délai médian diagnostic → admission
  output$kpi_delai <- renderText({
    tryCatch(
      {
        d <- donnees_filtrees()
        if (is.null(d) || nrow(d) == 0) {
          return("Pas de données")
        }
        paste0(round(median(d$delai_diag_admit, na.rm = TRUE), 1), " mois")
      },
      error = function(e) "Pas de données"
    )
  })


  # ──────────────────────────────────────────────────────
  # SECTION 4 : GRAPHIQUES PLOTLY (interactifs)
  #
  # Ces graphiques utilisent la bibliothèque plotly qui permet
  # le survol (hover) avec infobulle et le clic pour filtrer.
  #
  # ggplotly() convertit un graphique ggplot2 en plotly.
  # ──────────────────────────────────────────────────────

  # ── Timeline des admissions (plotly) ─────────────────
  output$timeline_plot <- renderPlotly({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(suppressWarnings(plotly::ggplotly(empty_plot("Pas de données"))))
    }

    # Compter les admissions par mois
    mensuel <- donnees_filtrees() %>%
      mutate(mois = floor_date(date_entree, "month")) %>%
      count(mois) %>%
      # Texte personnalisé pour l'infobulle au survol
      mutate(texte_hover = paste0(
        format(mois, "%B %Y"), "<br>",
        "<b>", n, " admissions</b>"
      ))

    p <- ggplot(mensuel, aes(x = mois, y = n, text = texte_hover)) +
      geom_area(fill = alpha(col_accent, 0.15)) +
      geom_line(color = col_primary, linewidth = 1.2) +
      geom_point(color = col_accent, size = 2) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      labs(x = NULL, y = "Admissions", title = NULL) +
      theme_cem() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    # tooltip = "text" → utilise notre colonne 'texte_hover' pour l'infobulle
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode  = "x unified", # infobulle unifiée sur l'axe X (comme PowerBI)
        showlegend = FALSE,
        margin     = list(l = 40, r = 10, t = 10, b = 60)
      )
  })

  # ── Donut statut vital (plotly) ───────────────────────
  output$status_plot <- renderPlotly({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(suppressWarnings(plotly::ggplotly(empty_plot("Pas de données"))))
    }

    statuts <- patients_filtres() %>%
      count(libelle_statut) %>%
      mutate(
        pct   = n / sum(n) * 100,
        label = paste0(libelle_statut, "<br><b>", n, "</b><br>", round(pct, 1), "%")
      )

    plot_ly(
      statuts,
      labels = ~libelle_statut,
      values = ~n,
      type = "pie",
      hole = 0.55, # Donut (trou central)
      marker = list(
        colors = c(col_accent, col_danger),
        line   = list(color = "white", width = 2)
      ),
      text = ~label,
      textinfo = "none", # Ne pas afficher de texte sur les tranches
      hoverinfo = "text", # Afficher notre texte personnalisé au survol
      showlegend = TRUE
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.1, y = -0.05),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })

  # ── Graphique des cancers (plotly + cross-filter) ─────
  # Cliquer sur une barre met à jour le filtre "Diagnostic principal"
  output$cancer_bar_plot <- renderPlotly({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(suppressWarnings(plotly::ggplotly(empty_plot("Pas de données"))))
    }

    d <- donnees_filtrees() %>%
      filter(!is.na(diag_cancer)) %>%
      count(diag_cancer) %>%
      slice_max(n, n = 10) %>%
      mutate(
        texte_hover = paste0(
          "<b>", diag_cancer, "</b><br>",
          n, " séjours<br>",
          round(n / nrow(donnees_filtrees()) * 100, 1), "% du total"
        )
      )

    p <- ggplot(d, aes(
      x    = reorder(str_wrap(diag_cancer, 22), n),
      y    = n,
      fill = n,
      text = texte_hover # pour plotly
    )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = col_accent, high = col_primary) +
      labs(x = NULL, y = "Séjours", title = NULL) +
      theme_cem() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text", source = "cancer_plot") %>%
      layout(
        margin     = list(l = 160, r = 20, t = 10, b = 40),
        showlegend = FALSE
      )
  })

  # ── Cross-filter : clic sur un cancer → filtre sidebar ─
  # event_data() capture les clics de l'utilisateur sur le graphique plotly
  observeEvent(event_data("plotly_click", source = "cancer_plot"), {
    clic <- event_data("plotly_click", source = "cancer_plot")
    if (!is.null(clic)) {
      # Le texte sur l'axe Y correspond au nom du cancer
      # str_wrap() avait ajouté des retours à la ligne → on les supprime
      nom_cancer <- str_replace_all(clic$y, "\n", " ")
      # Chercher la valeur exacte dans les données
      valeur_exacte <- df$diag_cancer[str_wrap(df$diag_cancer, 22) == nom_cancer][1]
      if (!is.na(valeur_exacte) && valeur_exacte %in% unique(df$diag_cancer)) {
        updateSelectInput(session, "filtre_diag", selected = valeur_exacte)
      }
    }
  })

  # ── Graphique des départements (plotly + cross-filter) ─
  output$dept_bar <- renderPlotly({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(suppressWarnings(plotly::ggplotly(empty_plot("Pas de données"))))
    }

    d <- patients_filtres() %>%
      filter(!is.na(departement)) %>%
      count(departement) %>%
      mutate(
        texte_hover = paste0(
          "<b>", departement, "</b><br>",
          n, " patients<br>",
          round(n / nrow(patients_filtres()) * 100, 1), "%"
        )
      )

    p <- ggplot(d, aes(
      x    = reorder(departement, n),
      y    = n,
      fill = n,
      text = texte_hover
    )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = alpha(col_primary, 0.4), high = col_primary) +
      labs(x = NULL, y = "Patients", title = NULL) +
      theme_cem() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text", source = "dept_plot") %>%
      layout(margin = list(l = 200, r = 20, t = 10, b = 40), showlegend = FALSE)
  })

  # Cross-filter : clic sur un département → filtre sidebar
  observeEvent(event_data("plotly_click", source = "dept_plot"), {
    clic <- event_data("plotly_click", source = "dept_plot")
    if (!is.null(clic)) {
      updateSelectInput(session, "filtre_dept", selected = clic$y)
    }
  })

  # ── Graphique des protocoles (plotly) ─────────────────
  output$protocole_bar <- renderPlotly({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(suppressWarnings(plotly::ggplotly(empty_plot("Pas de données"))))
    }

    d <- donnees_filtrees() %>%
      filter(
        !is.na(protocole), protocole != "Non applicable",
        protocole != "Non spécifié"
      ) %>%
      count(protocole) %>%
      slice_max(n, n = 12) %>%
      mutate(texte_hover = paste0("<b>", protocole, "</b><br>", n, " séjours"))

    if (nrow(d) == 0) {
      return(plotly_empty())
    }

    p <- ggplot(d, aes(
      x    = reorder(protocole, n),
      y    = n,
      fill = n,
      text = texte_hover
    )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = alpha(col_accent, 0.5), high = col_accent) +
      labs(x = NULL, y = "Séjours", title = NULL) +
      theme_cem() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 130, r = 20, t = 10, b = 40), showlegend = FALSE)
  })


  # ──────────────────────────────────────────────────────
  # SECTION 5 : GRAPHIQUES GGPLOT2 (statiques)
  #
  # Les graphiques plus complexes (survie, heatmaps, etc.)
  # restent en ggplot2 car la conversion en plotly peut
  # causer des problèmes avec certains geoms.
  # ──────────────────────────────────────────────────────

  # ── Boîte à moustaches DMS par type de prise en charge ─
  output$los_boxplot <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(type_pec), duree_sejour >= 0, duree_sejour < 200)
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }
    nb_pec <- n_distinct(d$type_pec)

    ggplot(d, aes(x = fct_reorder(type_pec, duree_sejour, median), y = duree_sejour, fill = type_pec)) +
      geom_boxplot(
        alpha = 0.85, color = col_dark,
        outlier.alpha = 0.3, outlier.color = col_warn
      ) +
      coord_flip() +
      scale_fill_manual(values = colorRampPalette(c(alpha(col_accent, 0.6), col_primary))(nb_pec)) +
      labs(
        x = NULL, y = "Durée de Séjour (jours)",
        title = "Distribution DMS par Type de Prise en Charge"
      ) +
      theme_cem() +
      theme(legend.position = "none")
  })

  # ── Histogramme délai diagnostic → admission ──────────
  output$tta_hist <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(delai_diag_admit), delai_diag_admit >= 0)
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }
    moy <- mean(d$delai_diag_admit, na.rm = TRUE)
    med <- median(d$delai_diag_admit, na.rm = TRUE)

    ggplot(d, aes(x = delai_diag_admit)) +
      geom_histogram(fill = col_primary, color = "white", bins = 35, alpha = 0.85) +
      geom_vline(xintercept = moy, color = col_accent, linetype = "dashed", linewidth = 1.3) +
      geom_vline(xintercept = med, color = col_warn, linetype = "solid", linewidth = 1.3) +
      annotate("text",
        x = moy, y = Inf, vjust = 1.5, hjust = -0.1,
        label = paste0("Moy: ", round(moy, 1), "m"),
        color = col_accent, size = 3.5, fontface = "bold"
      ) +
      annotate("text",
        x = med, y = Inf, vjust = 3.5, hjust = 1.1,
        label = paste0("Med: ", round(med, 1), "m"),
        color = col_warn, size = 3.5, fontface = "bold"
      ) +
      labs(
        x = "Mois entre Diagnostic et Admission", y = "Fréquence",
        title = "Délai Diagnostic → Première Admission"
      ) +
      theme_cem()
  })

  # ── Nuage de points âge × DMS (par statut métastatique) ─
  output$age_los_scatter <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>% filter(duree_sejour > 0, duree_sejour < 200)
    if (is.null(d) || nrow(d) < 5) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = age_sejour, y = duree_sejour, color = metastase)) +
      geom_point(alpha = 0.30, size = 1.5) +
      geom_smooth(method = "lm", aes(group = metastase), se = TRUE, linewidth = 1.2) +
      scale_color_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
      labs(
        x = "Âge à l'Admission", y = "Durée de Séjour (jours)", color = "Métastases",
        title = "Âge × Durée de Séjour"
      ) +
      theme_cem()
  })

  # ── Catégories DMS par sexe ───────────────────────────
  output$los_cat_plot <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      count(cat_duree, sexe_libelle) %>%
      mutate(cat_duree = factor(cat_duree,
        levels = c("Ambulatoire", "Court (<= 3j)", "Moyen (4-10j)", "Long (>10j)")
      ))
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = cat_duree, y = n, fill = sexe_libelle)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
      labs(
        x = "Catégorie de Durée", y = "Nombre de Séjours", fill = "Sexe",
        title = "Catégories de DMS par Sexe"
      ) +
      theme_cem()
  })

  # ── Comorbidités top 10 ───────────────────────────────
  output$assoc_bar_plot <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(diag_associe)) %>%
      count(diag_associe) %>%
      slice_max(n, n = 10)
    if (nrow(d) == 0) {
      return(empty_plot("Aucune comorbidité dans la sélection"))
    }

    ggplot(d, aes(x = reorder(str_wrap(diag_associe, 25), n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5, fontface = "bold") +
      coord_flip() +
      scale_fill_gradient(low = alpha(col_accent, 0.5), high = col_primary) +
      labs(x = NULL, y = "Fréquence", title = "Comorbidités Associées (Top 10)") +
      theme_cem() +
      theme(legend.position = "none") +
      expand_limits(y = max(d$n) * 1.15)
  })

  # ── Pyramide des âges ─────────────────────────────────
  output$age_pyramid <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(tranche_age), !is.na(sexe_libelle)) %>%
      count(tranche_age, sexe_libelle) %>%
      # Les hommes ont des valeurs négatives pour la représentation en miroir
      mutate(n_signe = ifelse(sexe_libelle == "Homme", -n, n))
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = tranche_age, y = n_signe, fill = sexe_libelle)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0, color = "white", linewidth = 1.2) +
      coord_flip() +
      scale_y_continuous(labels = function(x) abs(x)) +
      scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
      labs(
        x = "Tranche d'Âge", y = "Séjours", fill = "Sexe",
        title = "Pyramide des Âges"
      ) +
      theme_cem()
  })

  # ── Répartition M0/M1 par cancer ─────────────────────
  output$meta_by_cancer <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top8 <- donnees_filtrees() %>%
      count(diag_cancer) %>%
      slice_max(n, n = 8) %>%
      pull(diag_cancer)
    d <- donnees_filtrees() %>%
      filter(!is.na(diag_cancer), !is.na(metastase), diag_cancer %in% top8) %>%
      count(diag_cancer, metastase) %>%
      group_by(diag_cancer) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup()
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 20), pct), y = pct, fill = metastase)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
      labs(
        x = NULL, y = "% des Séjours", fill = "Métastases",
        title = "Répartition M0/M1 par Cancer (Top 8)"
      ) +
      theme_cem()
  })

  # ──────────────────────────────────────────────────────
  # SECTION 6 : COURBES DE SURVIE (Kaplan-Meier)
  #
  # Les courbes de Kaplan-Meier permettent d'estimer la
  # probabilité de survie au fil du temps.
  #
  # survfit() calcule la courbe de survie.
  # broom::tidy() convertit le résultat en tableau lisible.
  # tryCatch() gère les erreurs si les données sont insuffisantes.
  # ──────────────────────────────────────────────────────

  # ── KM Global ─────────────────────────────────────────
  output$km_global <- renderPlot({
    d <- patients_filtres() %>%
      filter(!is.na(mois_survie), mois_survie >= 0)
    if (is.null(d) || nrow(d) <= 5) {
      return(empty_plot("Pas de données"))
    }
    tryCatch(
      {
        modele <- survfit(Surv(mois_survie, evenement) ~ 1, data = d)
        donnees_p <- broom::tidy(modele)
        # Survie médiane (mois à 50% de survie)
        mediane <- as.numeric(summary(modele)$table["median"])

        ggplot(donnees_p, aes(x = time, y = estimate)) +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = alpha(col_accent, 0.2)) +
          geom_step(color = col_primary, linewidth = 1.4) +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = col_danger, linewidth = 0.8) +
          annotate("text",
            x = max(donnees_p$time) * 0.55, y = 0.57,
            label = paste0("Médiane: ", round(mediane, 1), " mois"),
            color = col_danger, size = 4, fontface = "bold"
          ) +
          scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
          labs(
            x = "Mois depuis Diagnostic", y = "Probabilité de Survie",
            title = "Courbe de Kaplan-Meier (Population Globale)"
          ) +
          theme_cem()
      },
      error = function(e) empty_plot()
    )
  })

  # ── KM par statut métastatique ────────────────────────
  output$km_meta <- renderPlot({
    d <- patients_filtres() %>%
      filter(!is.na(mois_survie), mois_survie >= 0, !is.na(metastase))
    if (is.null(d) || nrow(d) <= 5) {
      return(empty_plot("Pas de données"))
    }
    tryCatch(
      {
        modele <- survfit(Surv(mois_survie, evenement) ~ metastase, data = d)
        donnees_p <- broom::tidy(modele) %>%
          mutate(groupe = str_remove(strata, "metastase="))

        ggplot(donnees_p, aes(x = time, y = estimate, color = groupe, fill = groupe)) +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
          geom_step(linewidth = 1.3) +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
          scale_color_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
          scale_fill_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
          scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
          labs(
            x = "Mois depuis Diagnostic", y = "Probabilité de Survie",
            color = "Statut", fill = "Statut",
            title = "Kaplan-Meier : M0 vs M1"
          ) +
          theme_cem()
      },
      error = function(e) empty_plot()
    )
  })

  # ── KM par sexe ──────────────────────────────────────
  output$km_gender <- renderPlot({
    d <- patients_filtres() %>%
      filter(!is.na(mois_survie), mois_survie >= 0, !is.na(sexe_libelle))
    if (is.null(d) || nrow(d) <= 5) {
      return(empty_plot("Pas de données"))
    }
    tryCatch(
      {
        modele <- survfit(Surv(mois_survie, evenement) ~ sexe_libelle, data = d)
        donnees_p <- broom::tidy(modele) %>%
          mutate(groupe = str_remove(strata, "sexe_libelle="))

        ggplot(donnees_p, aes(x = time, y = estimate, color = groupe, fill = groupe)) +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
          geom_step(linewidth = 1.3) +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
          scale_color_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
          scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
          scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
          labs(
            x = "Mois depuis Diagnostic", y = "Probabilité de Survie",
            color = "Sexe", fill = "Sexe",
            title = "Kaplan-Meier : Homme vs Femme"
          ) +
          theme_cem()
      },
      error = function(e) empty_plot()
    )
  })

  # ── Survie médiane par cancer ─────────────────────────
  output$median_survival_bar <- renderPlot({
    d <- patients_filtres() %>%
      filter(!is.na(mois_survie), mois_survie >= 0, !is.na(diag_cancer))
    if (is.null(d) || nrow(d) <= 5) {
      return(empty_plot("Pas de données"))
    }

    # Calculer la survie médiane pour chaque cancer (minimum 3 patients)
    resultats <- d %>%
      group_by(diag_cancer) %>%
      filter(n() >= 3) %>%
      group_modify(~ {
        tryCatch(
          {
            modele <- survfit(Surv(.x$mois_survie, .x$evenement) ~ 1)
            data.frame(survie_med = as.numeric(summary(modele)$table["median"]), n = nrow(.x))
          },
          error = function(e) data.frame(survie_med = NA_real_, n = nrow(.x))
        )
      }) %>%
      filter(!is.na(survie_med)) %>%
      ungroup() %>%
      slice_max(n, n = 8)

    if (nrow(resultats) == 0) {
      return(empty_plot())
    }

    ggplot(resultats, aes(
      x = reorder(str_wrap(diag_cancer, 20), survie_med),
      y = survie_med, fill = survie_med
    )) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = paste0(round(survie_med, 0), "m")),
        hjust = -0.2, fontface = "bold", size = 3.5
      ) +
      coord_flip() +
      scale_fill_gradient(low = col_danger, high = col_accent) +
      labs(
        x = NULL, y = "Survie Médiane (mois)",
        title = "Survie Médiane par Type de Cancer"
      ) +
      theme_cem() +
      theme(legend.position = "none") +
      expand_limits(y = max(resultats$survie_med) * 1.15)
  })


  # ──────────────────────────────────────────────────────
  # SECTION 7 : PROFILS DE RISQUE
  # ──────────────────────────────────────────────────────

  # ── Heatmap antécédents × cancer ─────────────────────
  output$heatmap_risk <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top8 <- donnees_filtrees() %>%
      count(diag_cancer) %>%
      slice_max(n, n = 8) %>%
      pull(diag_cancer)
    d <- donnees_filtrees() %>%
      filter(!is.na(diag_antecedents), !is.na(diag_cancer), diag_cancer %in% top8) %>%
      count(diag_antecedents, diag_cancer) %>%
      group_by(diag_cancer) %>%
      filter(sum(n) >= 3) %>%
      ungroup()
    if (nrow(d) == 0) {
      return(empty_plot("Données insuffisantes pour la heatmap"))
    }

    ggplot(d, aes(x = str_wrap(diag_cancer, 12), y = diag_antecedents, fill = n)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = n), color = "white", fontface = "bold", size = 4) +
      scale_fill_gradient(low = "#D6EAF8", high = col_primary) +
      labs(
        x = "Cancer Principal", y = "Antécédent", fill = "Séjours",
        title = "Heatmap : Antécédents × Cancer Principal"
      ) +
      theme_cem() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # ── Mortalité par cancer & tranche d'âge ─────────────
  output$mortality_age_cancer <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top6 <- patients_filtres() %>%
      count(diag_cancer) %>%
      slice_max(n, n = 6) %>%
      pull(diag_cancer)
    d <- patients_filtres() %>%
      filter(!is.na(tranche_age), !is.na(diag_cancer), diag_cancer %in% top6) %>%
      group_by(diag_cancer, tranche_age) %>%
      summarise(taux_mortalite = mean(evenement) * 100, n = n(), .groups = "drop") %>%
      filter(n >= 2)
    if (nrow(d) == 0) {
      return(empty_plot())
    }

    ggplot(d, aes(
      x = tranche_age, y = taux_mortalite,
      color = str_wrap(diag_cancer, 18), group = diag_cancer
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = "Tranche d'Âge", y = "Taux de Mortalité (%)", color = "Cancer",
        title = "Mortalité par Cancer & Tranche d'Âge"
      ) +
      theme_cem()
  })

  # ── Taux de réadmission par cancer ───────────────────
  output$readmit_by_cancer <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- patients_filtres() %>%
      filter(!is.na(diag_cancer)) %>%
      group_by(diag_cancer) %>%
      summarise(taux_readmis = mean(est_readmis) * 100, n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      slice_max(n, n = 10)
    if (nrow(d) == 0) {
      return(empty_plot())
    }

    ggplot(d, aes(
      x = reorder(str_wrap(diag_cancer, 20), taux_readmis),
      y = taux_readmis, fill = taux_readmis
    )) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = paste0(round(taux_readmis, 0), "%")),
        hjust = -0.2, fontface = "bold", size = 3.5
      ) +
      coord_flip() +
      scale_fill_gradient(low = "#FEF9E7", high = col_warn) +
      labs(
        x = NULL, y = "Taux de Réadmission (%)",
        title = "Réadmission par Cancer Principal"
      ) +
      theme_cem() +
      theme(legend.position = "none") +
      expand_limits(y = 100)
  })

  # ── Histogramme nombre de séjours par patient ─────────
  output$stays_hist <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(patients_filtres(), aes(x = nb_sejours, fill = libelle_statut)) +
      geom_histogram(binwidth = 1, color = "white", position = "stack") +
      scale_fill_manual(values = c("En vie" = col_accent, "Décédé" = col_danger)) +
      scale_x_continuous(breaks = 1:10) +
      labs(
        x = "Nombre de Séjours par Patient", y = "Nombre de Patients", fill = "Statut",
        title = "Distribution du Nombre de Séjours"
      ) +
      theme_cem()
  })


  # ──────────────────────────────────────────────────────
  # SECTION 8 : ONGLET STATISTIQUES
  # ──────────────────────────────────────────────────────

  # ── Tableau récapitulatif par cancer ─────────────────
  output$stats_table <- renderDT({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }

    tableau <- patients_filtres() %>%
      filter(!is.na(diag_cancer)) %>%
      group_by(`Cancer` = diag_cancer) %>%
      summarise(
        N = n(),
        `% Décédés` = paste0(round(mean(evenement) * 100, 1), "%"),
        `% M1` = paste0(round(mean(metastase == "M1") * 100, 1), "%"),
        `Survie méd. (mois)` = tryCatch(
          {
            grp <- pick(mois_survie, evenement)
            round(as.numeric(
              summary(survfit(Surv(grp$mois_survie, grp$evenement) ~ 1))$table["median"]
            ), 1)
          },
          error = function(e) NA
        ),
        `DMS méd. (j)` = round(median(duree_totale, na.rm = TRUE), 1),
        `% Réadmis` = paste0(round(mean(est_readmis) * 100, 1), "%"),
        .groups = "drop"
      ) %>%
      arrange(desc(N))

    datatable(
      tableau,
      # "Responsive" adapte les colonnes à la taille de l'écran
      extensions = "Responsive",
      options = list(
        responsive  = TRUE, # Colonnes cachées sur petit écran
        pageLength  = 10,
        dom         = "tip", # t=table, i=info, p=pagination (pas de search)
        autoWidth   = FALSE # Pas de largeur auto → pas de scrollbar
      ),
      rownames = FALSE,
      class = "stripe hover compact"
    ) %>%
      formatStyle("% Décédés", color = col_danger, fontWeight = "bold") %>%
      formatStyle("% M1", color = col_warn, fontWeight = "bold")
  })

  # ── DMS médiane & IQR par prise en charge ────────────
  output$dms_ci_plot <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(type_pec), duree_sejour >= 0, duree_sejour < 200) %>%
      group_by(type_pec) %>%
      summarise(
        med = median(duree_sejour, na.rm = TRUE),
        q25 = quantile(duree_sejour, 0.25, na.rm = TRUE),
        q75 = quantile(duree_sejour, 0.75, na.rm = TRUE),
        n = n(), .groups = "drop"
      )
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = reorder(type_pec, med), y = med)) +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.3, color = col_primary, linewidth = 1) +
      geom_point(aes(size = n), color = col_accent, alpha = 0.9) +
      coord_flip() +
      labs(
        x = NULL, y = "DMS Médiane (jours) [IQR]", size = "N séjours",
        title = "Médiane & IQR de la DMS par Prise en Charge"
      ) +
      theme_cem()
  })

  # ── Heatmap volume activité par PEC & année ───────────
  output$pec_year_heatmap <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      mutate(annee = year(date_entree)) %>%
      filter(!is.na(type_pec)) %>%
      count(type_pec, annee)
    if (is.null(d) || nrow(d) <= 0) {
      return(empty_plot("Pas de données"))
    }

    ggplot(d, aes(x = factor(annee), y = type_pec, fill = n)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = n), size = 3, fontface = "bold", color = "white") +
      scale_fill_gradient(low = "#D5E8D4", high = col_primary) +
      labs(
        x = "Année", y = NULL, fill = "Séjours",
        title = "Volume par Type de Prise en Charge & Année"
      ) +
      theme_cem() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ── Distribution délai diag → décès (violon) ─────────
  output$tta_death_plot <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top6 <- patients_filtres() %>%
      filter(evenement == 1) %>%
      count(diag_cancer) %>%
      slice_max(n, n = 6) %>%
      pull(diag_cancer)
    d <- patients_filtres() %>%
      filter(
        evenement == 1, !is.na(mois_survie), !is.na(diag_cancer),
        diag_cancer %in% top6
      )
    if (nrow(d) == 0) {
      return(empty_plot("Aucun décès dans la sélection"))
    }

    ggplot(d, aes(
      x = reorder(str_wrap(diag_cancer, 18), mois_survie, median),
      y = mois_survie, fill = diag_cancer
    )) +
      geom_violin(alpha = 0.7, color = NA) +
      geom_boxplot(
        width = 0.15, color = "white", outlier.shape = NA,
        fill = col_primary, alpha = 0.8
      ) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = NULL, y = "Mois (Diagnostic → Décès)",
        title = "Distribution Diagnostic → Décès par Cancer"
      ) +
      theme_cem() +
      theme(legend.position = "none")
  })


  # ──────────────────────────────────────────────────────
  # SECTION 9 : ONGLET DONNÉES ENRICHIES
  # Nouvelles colonnes : géographie, stade TNM, OMS, IMC, tabac
  # ──────────────────────────────────────────────────────

  # ── Stade TNM par cancer (barres empilées) ────────────
  output$stade_bar <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top8 <- patients_filtres() %>%
      count(diag_cancer) %>%
      slice_max(n, n = 8) %>%
      pull(diag_cancer)
    d <- patients_filtres() %>%
      filter(!is.na(stade_tnm), diag_cancer %in% top8) %>%
      count(diag_cancer, stade_tnm) %>%
      group_by(diag_cancer) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(stade_tnm = factor(stade_tnm, levels = c("I", "II", "III", "IV")))
    if (nrow(d) == 0) {
      return(empty_plot())
    }

    ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 20), -pct), y = pct, fill = stade_tnm)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c(
        "I"   = col_accent,
        "II"  = col_warn,
        "III" = "#E67E22",
        "IV"  = col_danger
      )) +
      labs(
        x = NULL, y = "% des Patients", fill = "Stade TNM",
        title = "Distribution des Stades TNM par Cancer"
      ) +
      theme_cem()
  })

  # ── Heatmap score OMS × tranche d'âge ────────────────
  output$oms_heatmap <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- patients_filtres() %>%
      filter(!is.na(score_oms_libelle), !is.na(tranche_age)) %>%
      count(tranche_age, score_oms_libelle) %>%
      group_by(tranche_age) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup()
    if (nrow(d) == 0) {
      return(empty_plot())
    }

    ggplot(d, aes(x = tranche_age, y = score_oms_libelle, fill = pct)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(round(pct, 0), "%")),
        color = "white", fontface = "bold", size = 3.5
      ) +
      scale_fill_gradient(low = "#D6EAF8", high = col_danger) +
      labs(
        x = "Tranche d'Âge", y = "Score OMS", fill = "%",
        title = "Score OMS par Tranche d'Âge (% des Patients)"
      ) +
      theme_cem()
  })

  # ── IMC moyen par prise en charge ────────────────────
  output$imc_plot <- renderPlot({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }
    d <- donnees_filtrees() %>%
      filter(!is.na(imc), !is.na(type_pec)) %>%
      group_by(type_pec) %>%
      summarise(
        imc_moy = mean(imc, na.rm = TRUE),
        imc_sd = sd(imc, na.rm = TRUE),
        n = n(), .groups = "drop"
      ) %>%
      filter(n >= 5)
    if (nrow(d) == 0) {
      return(empty_plot())
    }

    ggplot(d, aes(x = reorder(type_pec, imc_moy), y = imc_moy, fill = imc_moy)) +
      geom_bar(stat = "identity", color = "white") +
      # Barre d'erreur = écart-type
      geom_errorbar(aes(ymin = imc_moy - imc_sd, ymax = imc_moy + imc_sd),
        width = 0.3, color = col_dark
      ) +
      geom_hline(yintercept = 25, linetype = "dashed", color = col_warn, linewidth = 0.8) +
      coord_flip() +
      scale_fill_gradient(low = alpha(col_primary, 0.4), high = col_primary) +
      labs(
        x = NULL, y = "IMC Moyen (kg/m²)",
        title = "IMC Moyen ± Écart-Type par Prise en Charge",
        subtitle = "Ligne pointillée = seuil surpoids (IMC 25)"
      ) +
      theme_cem() +
      theme(legend.position = "none")
  })

  # ── Boîte à moustaches tabac × cancer ────────────────
  output$tabac_boxplot <- renderPlot({
    if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
      return(empty_plot("Pas de données"))
    }
    top6 <- patients_filtres() %>%
      filter(tabac_paquet_an > 0) %>%
      count(diag_cancer) %>%
      slice_max(n, n = 6) %>%
      pull(diag_cancer)
    d <- patients_filtres() %>%
      filter(tabac_paquet_an > 0, !is.na(diag_cancer), diag_cancer %in% top6)
    if (nrow(d) < 3) {
      return(empty_plot("Données de tabagisme insuffisantes"))
    }

    ggplot(d, aes(
      x = reorder(str_wrap(diag_cancer, 18), tabac_paquet_an, median),
      y = tabac_paquet_an, fill = diag_cancer
    )) +
      geom_boxplot(
        alpha = 0.8, color = col_dark,
        outlier.alpha = 0.3, outlier.size = 1.5
      ) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = NULL, y = "Paquets-Années",
        title = "Consommation Tabagique (fumeurs uniquement)",
        subtitle = "Cancers ayant le plus de patients fumeurs (Top 6)"
      ) +
      theme_cem() +
      theme(legend.position = "none")
  })


  # ──────────────────────────────────────────────────────
  # SECTION 10 : TABLEAUX DATATABLES
  # ──────────────────────────────────────────────────────

  # ── Table des séjours récents ─────────────────────────
  output$patients_table <- renderDT({
    if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
      return(empty_plot("Pas de données"))
    }

    donnees_filtrees() %>%
      transmute(
        ID = id,
        Date = format(date_entree, "%d/%m/%Y"),
        Age = floor(age_sejour),
        Sexe = sexe_libelle,
        Diagnostic = diag_cancer,
        `Prise en charge` = type_pec,
        `DMS (j)` = duree_sejour,
        Métastases = metastase,
        Stade = stade_tnm,
        Protocole = protocole
      ) %>%
      arrange(desc(Date)) %>%
      head(200) %>%
      datatable(
        extensions = "Responsive",
        options = list(
          responsive  = TRUE,
          pageLength  = 8,
          dom         = "tip",
          autoWidth   = FALSE # Supprime la scrollbar horizontale
        ),
        rownames = FALSE,
        class = "stripe hover compact"
      ) %>%
      formatStyle("Métastases",
        color      = styleEqual(c("M0", "M1"), c(col_accent, col_danger)),
        fontWeight = "bold"
      ) %>%
      formatStyle("Stade",
        color = styleEqual(
          c("I", "II", "III", "IV"),
          c(col_accent, col_warn, "#E67E22", col_danger)
        ),
        fontWeight = "bold"
      )
  })


  # ──────────────────────────────────────────────────────
  # SECTION 11 : MODALES DE ZOOM
  #
  # Chaque graphique a un bouton "expand" (agrandir).
  # Quand on clique dessus, une fenêtre modale (popup) s'ouvre
  # avec le graphique en plus grand.
  #
  # zoom_map associe chaque bouton à sa fonction de graphique.
  # La boucle for() crée un écouteur pour chaque bouton.
  # ──────────────────────────────────────────────────────

  zoom_map <- list(
    list("zoom_timeline_plot", "Évolution des Admissions", function() {
      # La timeline en zoom est recréée en ggplot2 (plus lisible en grand)
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      mensuel <- donnees_filtrees() %>%
        mutate(mois = floor_date(date_entree, "month")) %>%
        count(mois)
      ggplot(mensuel, aes(x = mois, y = n)) +
        geom_area(fill = alpha(col_accent, 0.15)) +
        geom_line(color = col_primary, linewidth = 1.4) +
        geom_point(color = col_accent, size = 3) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
        labs(x = NULL, y = "Admissions", title = "Évolution Mensuelle des Admissions") +
        theme_cem() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }),
    list("zoom_los_boxplot", "DMS par Prise en Charge", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>% filter(!is.na(type_pec), duree_sejour < 200)
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      nb_pec <- n_distinct(d$type_pec)
      ggplot(d, aes(x = fct_reorder(type_pec, duree_sejour, median), y = duree_sejour, fill = type_pec)) +
        geom_boxplot(alpha = 0.85, color = col_dark) +
        coord_flip() +
        scale_fill_manual(values = colorRampPalette(c(alpha(col_accent, 0.6), col_primary))(nb_pec)) +
        labs(x = NULL, y = "Jours", title = "DMS par Type de Prise en Charge") +
        theme_cem() +
        theme(legend.position = "none")
    }),
    list("zoom_tta_hist", "Délai Diag→Admission", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>% filter(!is.na(delai_diag_admit), delai_diag_admit >= 0)
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      moy <- mean(d$delai_diag_admit, na.rm = TRUE)
      med <- median(d$delai_diag_admit, na.rm = TRUE)
      ggplot(d, aes(x = delai_diag_admit)) +
        geom_histogram(fill = col_primary, color = "white", bins = 35, alpha = 0.85) +
        geom_vline(xintercept = moy, color = col_accent, linetype = "dashed", linewidth = 1.3) +
        geom_vline(xintercept = med, color = col_warn, linetype = "solid", linewidth = 1.3) +
        labs(x = "Mois", y = "Fréquence", title = "Délai Diagnostic → Admission") +
        theme_cem()
    }),
    list("zoom_age_los_scatter", "Âge × DMS", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>% filter(duree_sejour > 0, duree_sejour < 200)
      if (is.null(d) || nrow(d) < 5) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = age_sejour, y = duree_sejour, color = metastase)) +
        geom_point(alpha = 0.3, size = 1.5) +
        geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
        scale_color_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
        labs(x = "Âge", y = "DMS (j)", color = "Métastases", title = "Âge × Durée de Séjour") +
        theme_cem()
    }),
    list("zoom_los_cat_plot", "Catégories DMS", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        count(cat_duree, sexe_libelle) %>%
        mutate(cat_duree = factor(cat_duree, levels = c("Ambulatoire", "Court (<= 3j)", "Moyen (4-10j)", "Long (>10j)")))
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = cat_duree, y = n, fill = sexe_libelle)) +
        geom_bar(stat = "identity", position = "dodge", color = "white") +
        scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
        labs(x = NULL, y = "Séjours", fill = "Sexe", title = "Catégories de DMS par Sexe") +
        theme_cem()
    }),
    list("zoom_assoc_bar_plot", "Comorbidités", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        filter(!is.na(diag_associe)) %>%
        count(diag_associe) %>%
        slice_max(n, n = 10)
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_associe, 25), n), y = n, fill = n)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_gradient(low = alpha(col_accent, 0.5), high = col_primary) +
        labs(x = NULL, y = "Fréquence", title = "Comorbidités (Top 10)") +
        theme_cem() +
        theme(legend.position = "none") +
        expand_limits(y = max(d$n) * 1.15)
    }),
    list("zoom_age_pyramid", "Pyramide des Âges", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        filter(!is.na(tranche_age), !is.na(sexe_libelle)) %>%
        count(tranche_age, sexe_libelle) %>%
        mutate(n_signe = ifelse(sexe_libelle == "Homme", -n, n))
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = tranche_age, y = n_signe, fill = sexe_libelle)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_y_continuous(labels = function(x) abs(x)) +
        scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
        labs(x = "Tranche d'Âge", y = "Séjours", fill = "Sexe", title = "Pyramide des Âges") +
        theme_cem()
    }),
    list("zoom_meta_by_cancer", "M0/M1 par Cancer", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top8 <- donnees_filtrees() %>%
        count(diag_cancer) %>%
        slice_max(n, n = 8) %>%
        pull(diag_cancer)
      d <- donnees_filtrees() %>%
        filter(!is.na(diag_cancer), !is.na(metastase), diag_cancer %in% top8) %>%
        count(diag_cancer, metastase) %>%
        group_by(diag_cancer) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ungroup()
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 20), pct), y = pct, fill = metastase)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
        labs(x = NULL, y = "% Séjours", fill = "Métastases", title = "Répartition M0/M1 par Cancer") +
        theme_cem()
    }),
    list("zoom_km_global", "KM Globale", function() {
      d <- patients_filtres() %>% filter(!is.na(mois_survie), mois_survie >= 0)
      if (is.null(d) || nrow(d) <= 5) {
        return(empty_plot("Pas de données"))
      }
      tryCatch(
        {
          m <- survfit(Surv(mois_survie, evenement) ~ 1, data = d)
          p <- broom::tidy(m)
          med <- as.numeric(summary(m)$table["median"])
          ggplot(p, aes(x = time, y = estimate)) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = alpha(col_accent, 0.2)) +
            geom_step(color = col_primary, linewidth = 1.4) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = col_danger) +
            annotate("text",
              x = max(p$time) * 0.55, y = 0.57,
              label = paste0("Médiane: ", round(med, 1), " mois"),
              color = col_danger, size = 4.5, fontface = "bold"
            ) +
            scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
            labs(x = "Mois", y = "Probabilité de Survie", title = "Kaplan-Meier Global") +
            theme_cem()
        },
        error = function(e) empty_plot()
      )
    }),
    list("zoom_km_meta", "KM M0 vs M1", function() {
      d <- patients_filtres() %>% filter(!is.na(mois_survie), mois_survie >= 0)
      if (is.null(d) || nrow(d) <= 5) {
        return(empty_plot("Pas de données"))
      }
      tryCatch(
        {
          m <- survfit(Surv(mois_survie, evenement) ~ metastase, data = d)
          p <- broom::tidy(m) %>% mutate(groupe = str_remove(strata, "metastase="))
          ggplot(p, aes(x = time, y = estimate, color = groupe, fill = groupe)) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
            geom_step(linewidth = 1.3) +
            scale_color_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
            scale_fill_manual(values = c("M0" = col_accent, "M1" = col_danger)) +
            scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
            labs(x = "Mois", y = "Probabilité", color = "Statut", fill = "Statut", title = "KM : M0 vs M1") +
            theme_cem()
        },
        error = function(e) empty_plot()
      )
    }),
    list("zoom_km_gender", "KM par Sexe", function() {
      d <- patients_filtres() %>% filter(!is.na(mois_survie), mois_survie >= 0)
      if (is.null(d) || nrow(d) <= 5) {
        return(empty_plot("Pas de données"))
      }
      tryCatch(
        {
          m <- survfit(Surv(mois_survie, evenement) ~ sexe_libelle, data = d)
          p <- broom::tidy(m) %>% mutate(groupe = str_remove(strata, "sexe_libelle="))
          ggplot(p, aes(x = time, y = estimate, color = groupe, fill = groupe)) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
            geom_step(linewidth = 1.3) +
            scale_color_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
            scale_fill_manual(values = c("Homme" = col_primary, "Femme" = col_accent)) +
            scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
            labs(x = "Mois", y = "Probabilité", color = "Sexe", fill = "Sexe", title = "KM : Homme vs Femme") +
            theme_cem()
        },
        error = function(e) empty_plot()
      )
    }),
    list("zoom_median_survival_bar", "Survie Médiane", function() {
      d <- patients_filtres() %>% filter(!is.na(mois_survie), mois_survie >= 0)
      if (is.null(d) || nrow(d) <= 5) {
        return(empty_plot("Pas de données"))
      }
      res <- d %>%
        group_by(diag_cancer) %>%
        filter(n() >= 3) %>%
        group_modify(~ tryCatch(
          {
            m <- survfit(Surv(.x$mois_survie, .x$evenement) ~ 1)
            data.frame(survie_med = as.numeric(summary(m)$table["median"]), n = nrow(.x))
          },
          error = function(e) data.frame(survie_med = NA_real_, n = nrow(.x))
        )) %>%
        filter(!is.na(survie_med)) %>%
        ungroup() %>%
        slice_max(n, n = 8)
      if (nrow(res) == 0) {
        return(empty_plot())
      }
      ggplot(res, aes(x = reorder(str_wrap(diag_cancer, 20), survie_med), y = survie_med, fill = survie_med)) +
        geom_bar(stat = "identity", color = "white") +
        geom_text(aes(label = paste0(round(survie_med, 0), "m")), hjust = -0.2, size = 4) +
        coord_flip() +
        scale_fill_gradient(low = col_danger, high = col_accent) +
        labs(x = NULL, y = "Survie Médiane (mois)", title = "Survie Médiane par Cancer") +
        theme_cem() +
        theme(legend.position = "none") +
        expand_limits(y = max(res$survie_med) * 1.15)
    }),
    list("zoom_heatmap_risk", "Heatmap Antécédents × Cancer", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top8 <- donnees_filtrees() %>%
        count(diag_cancer) %>%
        slice_max(n, n = 8) %>%
        pull(diag_cancer)
      d <- donnees_filtrees() %>%
        filter(!is.na(diag_antecedents), diag_cancer %in% top8) %>%
        count(diag_antecedents, diag_cancer) %>%
        group_by(diag_cancer) %>%
        filter(sum(n) >= 3) %>%
        ungroup()
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = str_wrap(diag_cancer, 12), y = diag_antecedents, fill = n)) +
        geom_tile(color = "white", linewidth = 0.8) +
        geom_text(aes(label = n), color = "white", fontface = "bold", size = 5) +
        scale_fill_gradient(low = "#D6EAF8", high = col_primary) +
        labs(x = "Cancer", y = "Antécédent", fill = "Séjours", title = "Heatmap : Antécédents × Cancer") +
        theme_cem() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }),
    list("zoom_mortality_age_cancer", "Mortalité par Âge & Cancer", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top6 <- patients_filtres() %>%
        count(diag_cancer) %>%
        slice_max(n, n = 6) %>%
        pull(diag_cancer)
      d <- patients_filtres() %>%
        filter(!is.na(tranche_age), diag_cancer %in% top6) %>%
        group_by(diag_cancer, tranche_age) %>%
        summarise(taux = mean(evenement) * 100, n = n(), .groups = "drop") %>%
        filter(n >= 2)
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = tranche_age, y = taux, color = str_wrap(diag_cancer, 18), group = diag_cancer)) +
        geom_line(linewidth = 1.3) +
        geom_point(size = 3) +
        scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
        scale_color_brewer(palette = "Set2") +
        labs(x = "Tranche d'Âge", y = "Taux Mortalité", color = "Cancer", title = "Mortalité par Cancer & Âge") +
        theme_cem()
    }),
    list("zoom_readmit_by_cancer", "Réadmission", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- patients_filtres() %>%
        filter(!is.na(diag_cancer)) %>%
        group_by(diag_cancer) %>%
        summarise(taux = mean(est_readmis) * 100, n = n(), .groups = "drop") %>%
        filter(n >= 3) %>%
        slice_max(n, n = 10)
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 20), taux), y = taux, fill = taux)) +
        geom_bar(stat = "identity", color = "white") +
        geom_text(aes(label = paste0(round(taux, 0), "%")), hjust = -0.2, size = 3.5) +
        coord_flip() +
        scale_fill_gradient(low = "#FEF9E7", high = col_warn) +
        labs(x = NULL, y = "Taux de Réadmission", title = "Réadmission par Cancer") +
        theme_cem() +
        theme(legend.position = "none") +
        expand_limits(y = 100)
    }),
    list("zoom_stays_hist", "Distribution des Séjours", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(patients_filtres(), aes(x = nb_sejours, fill = libelle_statut)) +
        geom_histogram(binwidth = 1, color = "white", position = "stack") +
        scale_fill_manual(values = c("En vie" = col_accent, "Décédé" = col_danger)) +
        labs(x = "Nombre de Séjours", y = "Patients", fill = "Statut", title = "Distribution du Nombre de Séjours") +
        theme_cem()
    }),
    list("zoom_dms_ci_plot", "DMS Médiane & IQR", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        filter(!is.na(type_pec), duree_sejour < 200) %>%
        group_by(type_pec) %>%
        summarise(
          med = median(duree_sejour, na.rm = TRUE), q25 = quantile(duree_sejour, .25, na.rm = TRUE),
          q75 = quantile(duree_sejour, .75, na.rm = TRUE), n = n(), .groups = "drop"
        )
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = reorder(type_pec, med), y = med)) +
        geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.3, color = col_primary, linewidth = 1) +
        geom_point(aes(size = n), color = col_accent) +
        coord_flip() +
        labs(x = NULL, y = "DMS Médiane (j) [IQR]", size = "N", title = "DMS Médiane & IQR") +
        theme_cem()
    }),
    list("zoom_pec_year_heatmap", "Volume PEC × Année", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        mutate(annee = year(date_entree)) %>%
        filter(!is.na(type_pec)) %>%
        count(type_pec, annee)
      if (is.null(d) || nrow(d) <= 0) {
        return(empty_plot("Pas de données"))
      }
      ggplot(d, aes(x = factor(annee), y = type_pec, fill = n)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = n), size = 3.5, fontface = "bold", color = "white") +
        scale_fill_gradient(low = "#D5E8D4", high = col_primary) +
        labs(x = "Année", y = NULL, fill = "Séjours", title = "Volume PEC × Année") +
        theme_cem() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }),
    list("zoom_tta_death_plot", "Diag → Décès par Cancer", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top6 <- patients_filtres() %>%
        filter(evenement == 1) %>%
        count(diag_cancer) %>%
        slice_max(n, n = 6) %>%
        pull(diag_cancer)
      d <- patients_filtres() %>%
        filter(evenement == 1, !is.na(mois_survie), diag_cancer %in% top6)
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 18), mois_survie, median), y = mois_survie, fill = diag_cancer)) +
        geom_violin(alpha = 0.7, color = NA) +
        geom_boxplot(width = 0.15, color = "white", outlier.shape = NA, fill = col_primary, alpha = 0.8) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2") +
        labs(x = NULL, y = "Mois", title = "Distribution Diag → Décès par Cancer") +
        theme_cem() +
        theme(legend.position = "none")
    }),
    list("zoom_stade_bar", "Stades TNM par Cancer", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top8 <- patients_filtres() %>%
        count(diag_cancer) %>%
        slice_max(n, n = 8) %>%
        pull(diag_cancer)
      d <- patients_filtres() %>%
        filter(!is.na(stade_tnm), diag_cancer %in% top8) %>%
        count(diag_cancer, stade_tnm) %>%
        group_by(diag_cancer) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ungroup() %>%
        mutate(stade_tnm = factor(stade_tnm, levels = c("I", "II", "III", "IV")))
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 20), -pct), y = pct, fill = stade_tnm)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("I" = col_accent, "II" = col_warn, "III" = "#E67E22", "IV" = col_danger)) +
        labs(x = NULL, y = "% Patients", fill = "Stade", title = "Stades TNM par Cancer") +
        theme_cem()
    }),
    list("zoom_oms_heatmap", "Score OMS × Âge", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- patients_filtres() %>%
        filter(!is.na(score_oms_libelle), !is.na(tranche_age)) %>%
        count(tranche_age, score_oms_libelle) %>%
        group_by(tranche_age) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ungroup()
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = tranche_age, y = score_oms_libelle, fill = pct)) +
        geom_tile(color = "white", linewidth = 0.8) +
        geom_text(aes(label = paste0(round(pct, 0), "%")), color = "white", fontface = "bold") +
        scale_fill_gradient(low = "#D6EAF8", high = col_danger) +
        labs(x = "Tranche d'Âge", y = "Score OMS", fill = "%", title = "Score OMS × Tranche d'Âge") +
        theme_cem()
    }),
    list("zoom_imc_plot", "IMC par Prise en Charge", function() {
      if (is.null(donnees_filtrees()) || nrow(donnees_filtrees()) == 0) {
        return(empty_plot("Pas de données"))
      }
      d <- donnees_filtrees() %>%
        filter(!is.na(imc), !is.na(type_pec)) %>%
        group_by(type_pec) %>%
        summarise(imc_moy = mean(imc, na.rm = TRUE), imc_sd = sd(imc, na.rm = TRUE), n = n(), .groups = "drop") %>%
        filter(n >= 5)
      if (nrow(d) == 0) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(type_pec, imc_moy), y = imc_moy, fill = imc_moy)) +
        geom_bar(stat = "identity", color = "white") +
        geom_errorbar(aes(ymin = imc_moy - imc_sd, ymax = imc_moy + imc_sd), width = 0.3, color = col_dark) +
        geom_hline(yintercept = 25, linetype = "dashed", color = col_warn) +
        coord_flip() +
        scale_fill_gradient(low = alpha(col_primary, 0.4), high = col_primary) +
        labs(x = NULL, y = "IMC Moyen", title = "IMC par Prise en Charge") +
        theme_cem() +
        theme(legend.position = "none")
    }),
    list("zoom_tabac_boxplot", "Tabagisme par Cancer", function() {
      if (is.null(patients_filtres()) || nrow(patients_filtres()) == 0) {
        return(empty_plot("Pas de données"))
      }
      top6 <- patients_filtres() %>%
        filter(tabac_paquet_an > 0) %>%
        count(diag_cancer) %>%
        slice_max(n, n = 6) %>%
        pull(diag_cancer)
      d <- patients_filtres() %>%
        filter(tabac_paquet_an > 0, diag_cancer %in% top6)
      if (nrow(d) < 3) {
        return(empty_plot())
      }
      ggplot(d, aes(x = reorder(str_wrap(diag_cancer, 18), tabac_paquet_an, median), y = tabac_paquet_an, fill = diag_cancer)) +
        geom_boxplot(alpha = 0.8, color = col_dark) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2") +
        labs(x = NULL, y = "Paquets-Années", title = "Tabagisme par Cancer (fumeurs uniquement)") +
        theme_cem() +
        theme(legend.position = "none")
    })
  )

  # ── Création automatique des écouteurs de zoom ─────────
  # Pour chaque entrée dans zoom_map, on crée un observeEvent()
  # qui ouvre une modale quand l'utilisateur clique sur le bouton.
  for (item in zoom_map) {
    local({
      id_bouton <- item[[1]]
      titre_modal <- item[[2]]
      fn_graphe <- item[[3]]
      id_output <- paste0("modal_out_", id_bouton) # identifiant unique pour chaque modal

      observeEvent(input[[id_bouton]], {
        showModal(modalDialog(
          title = titre_modal,
          size = "xl", # Modale extra-large
          plotOutput(id_output, height = "700px"),
          easyClose = TRUE, # Fermer en cliquant en dehors
          footer = modalButton("Fermer")
        ))
        output[[id_output]] <- renderPlot({
          fn_graphe()
        })
      })
    })
  }
} # Fin de la fonction server
