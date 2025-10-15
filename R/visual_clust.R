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
  packages <- c("ggplot2", "tidypaleo", "dplyr", "tidyr", "tibble", "scales")
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  safe_readline <- function(prompt = "", default = NULL) {
    input <- tryCatch({ readline(prompt) }, error = function(e) { "" })
    if (tolower(trimws(input)) == "exit") stop("Execution stopped by user with 'exit'.", call. = FALSE)
    if (input == "" && !is.null(default)) return(default)
    return(input)
  }

  if (!"Cluster" %in% names(data)) stop("The ‘Cluster’ column is missing from the dataframe.")
  data$Cluster <- as.numeric(data$Cluster)
  n_clusters <- length(unique(data$Cluster))

  #repeat {
    # Sélection colonne profondeur
    repeat {
      cat("\nAvailable columns:\n")
      print(names(data))
      cat("\n")
      depth_col <- safe_readline("Enter the name of the column representing depth (e.g. depth): ")
      if (depth_col %in% colnames(data)) break
      cat("Invalid column name for depth. Please try again.\n")
    }
  #
  #   # Sélection variables
  #   repeat {
  #     cat("\nAvailable columns:\n")
  #     print(names(data))
  #     cat("\n")
  #     var_input <- safe_readline("Enter the names of the variables to display (comma separated): ")
  #     variables <- unlist(strsplit(var_input, ",\\s*"))
  #     if (all(variables %in% names(data))) break
  #     cat("Some variables are not valid. Please try again.\n")
  #   }

  repeat {
    cat("\nAvailable columns:\n")
    print(names(data))
    cat("\n")
    cat("You can:\n")
    cat(" - Enter variable names separated by commas (e.g. Fe, Ti, Ca)\n")
    cat(" - Type 'all' to use all variables\n")
    cat(" - Type 'pca' to use variables selected by PCA (from 'variables_cluster')\n\n")

    var_input <- safe_readline("Enter your choice: ")

    # Option "all"
    if (tolower(var_input) == "all") {
      variables <- setdiff(names(data), c(depth_col, "Cluster"))
      cat("All variables selected.\n")
      break
    }
    # Option "pca"
    if (tolower(var_input) == "pca") {
      if (exists("variables_cluster", envir = .GlobalEnv)) {
        vars_pca <- colnames(get("variables_cluster", envir = .GlobalEnv))
        # Vérifie qu’elles existent dans data
        variables <- intersect(vars_pca, names(data))
        cat("Variables selected by PCA loaded from 'variables_cluster'.\n")
        break
      } else {
        cat("No object named 'variables_cluster' found in the global environment.\n")
        next
      }
    }

    # Option manuelle
    variables <- unlist(strsplit(var_input, ",\\s*"))
    if (all(variables %in% names(data))) break
    cat("Some variables are not valid. Please try again.\n")
  }


    cat("\n")

    # Palette de couleurs
    use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): ", default = "no"))
    if (use_custom %in% c("oui", "o", "yes", "y")) {
      cat("Enter", n_clusters, "colors (by name or hexadecimal, e.g. 'red' or '#FF0000')\n")
      custom_palette <- sapply(seq_len(n_clusters), function(i) safe_readline(paste("Color for cluster", i, ": ")))
    } else {
      custom_palette <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(n_clusters)
    }

    # Préparer données
    xrfStrat <- data %>%
      dplyr::select(all_of(c(variables, depth_col, "Cluster"))) %>%
      tidyr::pivot_longer(
        cols = -c(all_of(depth_col), "Cluster"),
        names_to = "elements",
        values_to = "peakarea"
      ) %>%
      tidyr::drop_na()

    main_plot <- xrfStrat %>% ggplot(aes(x = peakarea, y = .data[[depth_col]])) +
      geom_lineh(aes(color = Cluster), linewidth = 0.75) +
      scale_y_reverse() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      tidypaleo::facet_geochem_gridh(vars(elements)) +
      labs(x = "Geochemistry", y = "Depth [mm]", color = "Cluster") +
      tidypaleo::theme_paleo() +
      scale_color_gradientn(colors = custom_palette)+
      theme(
        text = element_text(family = "sans", face = "bold"),
        axis.text = element_text(size = 11, color = "gray25"),
        axis.title = element_text(size = 13),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")

    # Virtual Core optionnel
    show_core <- tolower(safe_readline("\nShow Virtual Core? (yes/no): ", default = "no"))
    if (show_core %in% c("yes", "y", "oui", "o")) {
      if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
      library(patchwork)

      core_data <- xrfStrat %>%
        dplyr::select(all_of(depth_col), Cluster) %>%
        dplyr::distinct()

      core_plot <- ggplot(core_data, aes(x = 1, y = .data[[depth_col]], fill = factor(Cluster))) +
        geom_tile(width = 1) +
        scale_y_reverse(position = "right") +
        scale_fill_manual(values = custom_palette, name = "Clusters") +
        labs(x = NULL, y = NULL, title = "Virtual Core") +
        tidypaleo::theme_paleo() +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right",
          text = element_text(family = "sans", face = "bold")
        )

      combined_plot <- main_plot + core_plot + patchwork::plot_layout(widths = c(4, 0.25))
      print(combined_plot)
    } else {
      print(main_plot)
      cat("Virtual Core skipped.\n")
    }

    # Export PDF
    save_pdf <- tolower(safe_readline("\nWould you like to export in PDF? (yes/no): ", default = "no"))

    if (save_pdf %in% c("yes", "y")) {
      if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
      library(rstudioapi)

      if (!rstudioapi::isAvailable()) {
        cat("RStudio API not available. Cannot select file interactively.\n")
      } else {
        pdf_path <- rstudioapi::selectFile(
          caption = "Save cluster visualization",
          label = "Save",
          path = getwd(),
          filter = list("PDF files" = "pdf"),
          existing = FALSE
        )

        if (nzchar(pdf_path)) {
          if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE))
            pdf_path <- paste0(pdf_path, ".pdf")

          # Demande des dimensions personnalisées
          pdf_width <- as.numeric(safe_readline("Enter desired PDF width (in inches, e.g., 10): "))
          pdf_height <- as.numeric(safe_readline("Enter desired PDF height (in inches, e.g., 8): "))

          # Crée le PDF avec ces dimensions
          pdf(pdf_path, width = pdf_width, height = pdf_height)
          if (exists("combined_plot")) print(combined_plot)
          else print(main_plot)
          dev.off()

          cat("\nCluster visualization saved to:", pdf_path, "\n")
        } else {
          cat("Saving cancelled.\n")
        }
      }
    } else {
      cat("PDF export skipped.\n")
    }
    cat("End.\n")
    break
  }

