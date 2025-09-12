#' @title Visualisation des clusters géochimiques
#'
#' @description
#' Crée des profils géochimiques en fonction de la profondeur, colorés selon les clusters assignés.
#' L'utilisateur peut définir une palette personnalisée en fournissant une couleur par cluster.
#'
#' @param data Le dataframe original avec la colonne "Cluster" déjà ajoutée
#' @return Un graphique ggplot2 avec les profils géochimiques colorés par cluster
#' @export
visual.clust <- function(data) {
  # Charger les packages nécessaires
  packages <- c("ggplot2", "tidypaleo", "dplyr", "tidyr", "tibble", "scales")
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }

  # Vérifier la présence de la colonne "Cluster"
  if (!"Cluster" %in% names(data)) stop("The ‘Cluster’ column is missing from the dataframe.")

  # Forcer la colonne Cluster à être numérique
  data$Cluster <- as.numeric(data$Cluster)
  n_clusters <- length(unique(data$Cluster))

  # ---- Boucle principale complète ----
  repeat {
    # ---- Étape 1 : Sélection de la colonne de profondeur ----
    repeat {
      cat("\nAvailable columns:\n")
      print(names(data))
      depth_col <- safe_readline("Enter the name of the column representing depth (e.g. depth): ")

      if (depth_col %in% colnames(data)) {
        break
      } else {
        cat("Invalid column name for depth. Please try again.\n")
      }
    }
    cat("\n")
    # ---- Étape 2 : Sélection des variables ----
    repeat {
      cat("\nAvailable columns:\n")
      print(names(data))
      var_input <- safe_readline("Enter the names of the variables to display (comma separated): ")
      variables <- unlist(strsplit(var_input, ",\\s*"))

      if (all(variables %in% names(data))) {
        break
      } else {
        cat("Some variables are not valid. Please try again.\n")
      }
    }
    cat("\n")
    # ---- Étape 3 : Palette de couleurs ----
    use_custom <- tolower(safe_readline(prompt = "Would you like to set a custom palette? (yes/no): "))
    cat("\n")
    if (use_custom %in% c("oui", "o", "yes", "y")) {
      cat("Enter", n_clusters, "colors (by name or hexadecimal, e.g. ‘red’ or '#FF0000')\n")
      user_colors <- character(n_clusters)
      for (i in seq_len(n_clusters)) {
        user_colors[i] <- safe_readline(prompt = paste("Color for the cluster", i, ": "))
      }
      custom_palette <- user_colors
    } else {
     # custom_palette <- colorRampPalette(c("#A52A2A", "#FFEFD5"))(n_clusters)
      custom_palette <- colorRampPalette(
        c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b")
      )(n_clusters)
    }

    # ---- Étape 4 : Préparer les données ----
    xrfStrat <- data %>%
      dplyr::select(all_of(c(variables, depth_col, "Cluster"))) %>%
      tidyr::pivot_longer(
        cols = -c(all_of(depth_col), "Cluster"),
        names_to = "elements",
        values_to = "peakarea"
      ) %>%
      tidyr::drop_na()

    # ---- Étape 5 : Créer le graphique principal ----
    main_plot <- xrfStrat %>%
      ggplot(aes(x = peakarea, y = .data[[depth_col]])) +
      geom_lineh(aes(color = Cluster), size = 0.75) +
      scale_y_reverse() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      tidypaleo::facet_geochem_gridh(vars(elements)) +
      labs(x = "Geochemistry", y = "Depth [mm]", color = "Cluster") +
      tidypaleo::theme_paleo() +
      theme(legend.position = "none") +
      scale_color_gradientn(colors = custom_palette)

    # ---- Étape 6 : Virtual Core ----
    show_core <- tolower(safe_readline(prompt = "\nShow Virtual Core? (yes/no): "))
    if (show_core %in% c("yes", "y", "oui", "o")) {
      if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
      library(patchwork)

      core_data <- xrfStrat %>%
        dplyr::select(all_of(depth_col), Cluster) %>%
        dplyr::distinct()

      core_plot <- ggplot(core_data, aes(x = 1, y = .data[[depth_col]], fill = Cluster)) +
        geom_tile(width = 1) +
        scale_y_reverse(position = "right") +
        scale_fill_gradientn(colors = custom_palette) +
        labs(x = NULL, y = NULL, title = "Virtual Core") +
        tidypaleo::theme_paleo() +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right"
        )

      combined_plot <- main_plot + core_plot + patchwork::plot_layout(widths = c(4, 0.5))
      print(combined_plot)
    } else {
      print(main_plot)
      cat("Virtual Core skipped.\n")
    }

    # ---- Étape 7 : Recommencer ? ----
    rerun <- tolower(safe_readline("\nWould you like to generate another visualization? (yes/no): "))
    if (rerun %in% c("no", "n")) {
      cat("End.\n")
      break
    }
  }
}
