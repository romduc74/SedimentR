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
  packages <- c("ggplot2", "tidypaleo", "dplyr", "tidyr", "tibble", "scales", "patchwork", "grid", "png", "jpeg", "tiff","cowplot")
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
  n_clusters <- length(unique(na.omit(data$Cluster)))

  ## Depth column selection
  repeat {
    cat("\n\n==============================\n")
    cat("  DEPTH COLUMN SELECTION\n")
    cat("==============================\n\n")
    cat("Available columns:\n")
    print(names(data))
    cat("\n")
    depth_col <- safe_readline("Enter the name of the column representing depth (e.g., depth): ")
    if (depth_col %in% colnames(data)) break
    cat("\nInvalid column name for depth. Please try again.\n\n")
  }

  ## Variable selection
  repeat {
    cat("\n\n==============================\n")
    cat("  VARIABLE SELECTION\n")
    cat("==============================\n\n")
    cat("Available columns:\n")
    cat("\n")
    print(names(data))
    cat("\nYou can:\n\n - Enter variable names separated by commas (e.g., Fe, Ti, Ca)\n\n - Type 'all' to use all variables\n\n - Type 'pca' to use PCA-selected variables\n\n")
    var_input <- safe_readline("Enter your choice: ")

    if (tolower(var_input) == "all") {
      variables <- setdiff(names(data), c(depth_col, "Cluster"))
      cat("\nAll variables selected.\n")
      break
    }

    if (tolower(var_input) == "pca") {
      if (exists("variables_cluster", envir = .GlobalEnv)) {
        vars_pca <- colnames(get("variables_cluster", envir = .GlobalEnv))
        variables <- intersect(vars_pca, names(data))
        cat("\nVariables selected by PCA loaded.\n")
        break
      } else {
        cat("\nNo object named 'variables_cluster' found.\n")
        next
      }
    }

    variables <- unlist(strsplit(var_input, ",\\s*"))
    if (all(variables %in% names(data))) break
    cat("\nSome variables are not valid. Please try again.\n")
  }

  ## Color palette
  cat("\n\n==============================\n")
  cat("  COLOR PALETTE SELECTION\n")
  cat("==============================\n\n")
  use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): ", default = "no"))

  if (use_custom %in% c("yes", "y")) {
    cat("\nEnter", n_clusters, "colors (by name or hexadecimal, e.g., 'red' or '#FF0000')\n\n")
    custom_palette <- sapply(seq_len(n_clusters), function(i) safe_readline(paste("Color for cluster", i, ": ")))
  } else {
    custom_palette <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(n_clusters)
  }

  ## Data preparation
  cat("\n\n==============================\n")
  cat("  DATA PREPARATION\n")
  cat("==============================\n\n")

  xrfStrat <- data %>%
    dplyr::select(all_of(c(variables, depth_col, "Cluster"))) %>%
    tidyr::pivot_longer(
      cols = -c(all_of(depth_col), "Cluster"),
      names_to = "elements",
      values_to = "peakarea"
    ) %>%
    tidyr::drop_na(any_of(c(variables, depth_col)))  # Keep NA in Cluster only

  depth_unit <- safe_readline("\nEnter the depth unit (default = mm): ", default = "mm")

  ## Detect depth gaps
  xrfStrat <- xrfStrat %>%
    arrange(.data[[depth_col]]) %>%
    group_by(elements) %>%
    mutate(
      depth_diff = .data[[depth_col]] - lag(.data[[depth_col]], default = first(.data[[depth_col]])),
      depth_step = median(diff(.data[[depth_col]]), na.rm = TRUE),
      is_gap = depth_diff > 1.9999999999999996 * depth_step,
      segment = cumsum(is_gap)
    ) %>%
    ungroup()

  ## Main plot
  # cat("\n\n==============================\n")
  # cat("  MAIN PLOT CREATION\n")
  # cat("==============================\n\n")
  #

  main_plot <- xrfStrat %>%
    ggplot(aes(x = peakarea, y = .data[[depth_col]], group = interaction(elements, segment))) +
    geom_lineh(aes(color = Cluster), linewidth = 0.75, na.rm = TRUE) +
    scale_y_reverse() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    tidypaleo::facet_geochem_gridh(vars(elements)) +
    labs(
      x = "Geochemistry",
      y = paste0("Depth [", depth_unit, "]"),
      color = "Cluster"
    ) +
    tidypaleo::theme_paleo() +
    scale_color_gradientn(colors = custom_palette) +
    theme(
      text = element_text(family = "sans", face = "bold"),
      axis.text = element_text(size = 11, color = "gray25"),
      axis.title = element_text(size = 13),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "none"
    )

  ## Virtual Core
  cat("\n\n==============================\n")
  cat("  VIRTUAL CORE OPTION\n")
  cat("==============================\n\n")
  show_core <- tolower(safe_readline("Show Virtual Core? (yes/no): ", default = "no"))

  if (show_core %in% c("yes", "y")) {
    cat("\nGenerating Virtual Core...\n\n")

    core_data <- xrfStrat %>%
      dplyr::select(all_of(depth_col), Cluster) %>%
      dplyr::distinct() %>%
      arrange(.data[[depth_col]])

    depth_step <- median(diff(core_data[[depth_col]]), na.rm = TRUE)

    core_data <- core_data %>%
      mutate(
        depth_diff = .data[[depth_col]] - lag(.data[[depth_col]], default = first(.data[[depth_col]])),
        is_gap = depth_diff > 1.9999999999999996 * depth_step,
        Block = cumsum((Cluster != lag(Cluster, default = first(Cluster))) | is_gap)
      )

    blocks <- core_data %>%
      group_by(Block, Cluster) %>%
      summarize(
        Depth_start = min(.data[[depth_col]], na.rm = TRUE),
        Depth_end   = max(.data[[depth_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Depth_mid = (Depth_start + Depth_end) / 2,
        Height = Depth_end - Depth_start + depth_step
      )

    core_plot <- ggplot(blocks, aes(x = 1, y = Depth_mid, fill = factor(Cluster), height = Height)) +
      geom_tile(width = 1) +
      scale_y_reverse(position = "right") +
      scale_fill_manual(values = custom_palette, name = "Clusters", na.value = "white") +
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
    cat("\nVirtual Core skipped.\n")
  }

  ## ============================================================
  ## MODELE D'AGE (OPTIONNEL)
  ## ============================================================
  cat("\n\n==============================\n")
  cat("  AGE MODEL OPTION\n")
  cat("==============================\n\n")

  add_age_model <- tolower(safe_readline(
    "Would you like to add an age-depth model? (yes/no): ",
    default = "no"
  ))

  cat("\n")

  if (add_age_model %in% c("yes", "y")) {

    ## Sélection du fichier
    age_file <- rstudioapi::selectFile(
      caption = "Select age model table",
      label   = "Open",
      path    = getwd(),
      filter  = list(
        "Data files" = c("csv", "txt", "xlsx")
      ),
      existing = TRUE
    )

    if (!nzchar(age_file)) {
      cat("\nNo age model selected. Skipped.\n")
    } else {

      ext <- tolower(tools::file_ext(age_file))

      ## Lecture du fichier
      if (ext == "xlsx") {

        sheets <- readxl::excel_sheets(age_file)
        cat("\nAvailable sheets:\n")
        print(sheets)

        sheet_name <- safe_readline(
          "Enter the sheet name containing the age model: "
        )

        age_data <- readxl::read_excel(age_file, sheet = sheet_name)

      } else {
        age_data <- read.csv(age_file, stringsAsFactors = FALSE)
      }

      ## Sélection des colonnes
      repeat {
        cat("\nAge model columns:\n")
        cat("\n")
        print(names(age_data))
        cat("\n")

        age_depth_col <- safe_readline(
          "Enter the depth column name: "
        )
        cat("\n")
        age_col <- safe_readline(
          "Enter the age (years) column name: "
        )
        cat("\n")

        if (all(c(age_depth_col, age_col) %in% names(age_data))) break
        cat("\nInvalid column names. Please try again.\n")
      }
      cat("\n")

      ## Unités
      age_unit <- safe_readline(
        "Enter age unit (e.g., cal yr BP, ka, yr): ",
        default = "yr"
      )

      cat("\n")
      ## Graphique du modèle d'âge
      age_plot <- ggplot(
        age_data,
        aes(
          x = .data[[age_col]],
          y = .data[[age_depth_col]]
        )
      ) +
        geom_path(linewidth = 0.8, color = "black") +
        scale_y_reverse() +
        labs(
          x = paste0("Age [", age_unit, "]"),
          y = paste0("Depth [", depth_unit, "]"),
          title = "Age model"
        ) +
        tidypaleo::theme_paleo() +
        theme(
          axis.text.y =  element_text(family = "sans", face = "bold"),
          axis.text = element_text(size = 11, color = "gray25"),
          text = element_text(family = "sans", face = "bold")
        )
    }
  }


  ## ============================================================
  ## Photo de carotte (optionnelle)
  ## ============================================================
  cat("\n\n==============================\n")
  cat("  PHOTO DE CAROTTE (OPTIONNELLE)\n")
  cat("==============================\n\n")

  add_photo <- tolower(safe_readline("Would you like to add a core image? (yes/no): ", default = "no"))

  if (add_photo %in% c("yes", "y")) {

    photo_file <- rstudioapi::selectFile(
      caption = "Sélectionner une photo de carotte",
      label = "Ouvrir",
      path = getwd(),
      filter = list("Images" = c("jpg","jpeg","png","tif","tiff")),
      existing = TRUE
    )

    if (!nzchar(photo_file)) {
      cat("\nNo image selected.\nCore photo skipped.\n")
    } else {

      file_ext <- tolower(tools::file_ext(photo_file))
      temp_png <- tempfile(fileext = ".png")

      if (file_ext %in% c("jpg", "jpeg")) {
        tmp_img <- readJPEG(photo_file)     # lire JPG
        writePNG(tmp_img, target = temp_png) # convertir en PNG temporaire
        img <- readPNG(temp_png)            # lire le PNG pour draw_image
      } else if (file_ext %in% c("tif", "tiff")) {
        tmp_img <- readTIFF(photo_file)
        writePNG(tmp_img, target = temp_png)
        img <- readPNG(temp_png)
      } else if (file_ext == "png") {
        temp_png <- photo_file
        img <- readPNG(temp_png)
      } else {
        stop("Format non supporté.")
      }

      photo_plot <- ggdraw() +
        draw_image(img, scale = 0.91) +
        ggtitle("Sediment Core") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }

  } else {
    cat("\nCore photo skipped.\n")
  }

  ## ============================================================
  ## Combinaison finale
  ## ============================================================
  plots_to_combine <- list()
  if (exists("photo_plot")) plots_to_combine <- c(plots_to_combine, list(photo_plot))
  if (exists("age_plot"))   plots_to_combine <- c(plots_to_combine, list(age_plot))
  if (exists("main_plot"))  plots_to_combine <- c(plots_to_combine, list(main_plot))
  if (exists("core_plot"))  plots_to_combine <- c(plots_to_combine, list(core_plot))


  if (length(plots_to_combine) == 1) {
    combined_plot <- plots_to_combine[[1]]
  } else if (length(plots_to_combine) > 1) {
    widths <- c()
    if (exists("photo_plot")) widths <- c(widths, 0.7)
    if (exists("age_plot"))   widths <- c(widths, 1)
    if (exists("main_plot")) widths <- c(widths, 4)
    if (exists("core_plot")) widths <- c(widths, 0.25)
    combined_plot <- patchwork::wrap_plots(plots_to_combine, nrow = 1) + patchwork::plot_layout(widths = widths)
  }

  print(combined_plot)
  combined_plot <<- combined_plot

  ## PDF Export
  cat("\n\n==============================\n")
  cat("  EXPORT GRAPH TO PDF\n")
  cat("==============================\n\n")
  save_pdf <- tolower(safe_readline("Would you like to export the figure as a PDF? (yes/no): ", default = "no"))

  if (save_pdf %in% c("yes", "y")) {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
    library(rstudioapi)
    if (rstudioapi::isAvailable()) {
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
        pdf_width <- as.numeric(safe_readline("\nEnter desired PDF width (in inches, e.g., 10): "))
        cat("\n")
        pdf_height <- as.numeric(safe_readline("Enter desired PDF height (in inches, e.g., 8): "))
        pdf(pdf_path, width = pdf_width, height = pdf_height)
        if (exists("combined_plot")) print(combined_plot) else print(main_plot)
        dev.off()
        cat("\n\nCluster visualization saved to:", pdf_path, "\n")
      } else {
        cat("\nSaving cancelled.\n")
      }
    } else {
      cat("\nRStudio API not available. Cannot select file interactively.\n")
    }
  } else {
    cat("\nPDF export skipped.\n")
  }

  cat("\n\n==============================\n")
  cat("  END OF EXECUTION\n")
  cat("==============================\n\n")
}
