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

  cat("\n\n==============================\n")
  cat("  CLUSTER DISPLAY OPTION\n")
  cat("==============================\n\n")

  show_clusters <- tolower(
    safe_readline("Would you like to display clusters? (yes/no): ", default = "yes")
  )

  use_clusters <- show_clusters %in% c("yes", "y")

  if (use_clusters) {
    if (!"Cluster" %in% names(data)) {
      stop("The ‘Cluster’ column is required when cluster display is enabled.")
    }
    data$Cluster <- as.numeric(data$Cluster)
    n_clusters <- length(unique(na.omit(data$Cluster)))
  }

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
      # variables <- setdiff(names(data), c(depth_col, "Cluster"))
      variables <- setdiff(names(data),c(depth_col, if (use_clusters) "Cluster"))
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
  if (use_clusters) {
    cat("\n\n==============================\n")
    cat("  COLOR PALETTE SELECTION\n")
    cat("==============================\n\n")
    use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): ", default = "no"))

    if (use_custom %in% c("yes", "y")) {
      cat("\nEnter", n_clusters, "colors (by name or hexadecimal, e.g., 'red' or '#FF0000')\n\n")
      custom_palette <- sapply(seq_len(n_clusters), function(i) safe_readline(paste("Color for cluster", i, ": ")))
    } else {
      custom_palette <- colorRampPalette(c("#35163b", "#662483", "#f39200", "#f9b233", "#ffda77"))(n_clusters)
    }
  }

  ## Data preparation
  cat("\n\n==============================\n")
  cat("  DATA PREPARATION\n")
  cat("==============================\n\n")



  cols_to_keep <- c(variables, depth_col)
  if (use_clusters) cols_to_keep <- c(cols_to_keep, "Cluster")

  xrfStrat <- data %>%
    dplyr::select(all_of(cols_to_keep)) %>%
    tidyr::pivot_longer(
      cols = -all_of(c(depth_col, if (use_clusters) "Cluster")),
      names_to = "elements",
      values_to = "peakarea"
    ) %>%
    tidyr::drop_na(any_of(c(variables, depth_col)))%>%
    mutate(
      elements = factor(elements, levels = variables)
    )

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



  main_plot <- xrfStrat %>%
    ggplot(aes(x = peakarea, y = .data[[depth_col]],
               group = interaction(elements, segment))) +
    geom_lineh(
      aes(color = if (use_clusters) Cluster else NULL),
      linewidth = 0.25,
      na.rm = TRUE
    ) +
    scale_y_reverse() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    tidypaleo::facet_geochem_gridh(vars(elements)) +
    labs(
      x = "Geochemistry",
      y = paste0("Depth [", depth_unit, "]"),
      color = if (use_clusters) "Cluster" else NULL
    ) +
    tidypaleo::theme_paleo() +
    theme(
      text = element_text(family = "sans", face = "bold"),
      axis.text = element_text(size = 11, color = "gray25"),
      axis.title = element_text(size = 13),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "none"
    )

  cat("\n\n==============================\n")
  cat("  HORIZONTAL DEPTH LINES OPTION\n")
  cat("==============================\n\n")

  depth_vals <- NULL

  add_hlines <- tolower(safe_readline(
    "Would you like to add horizontal depth lines? (yes/no): ",
    default = "no"
  ))

  cat("\n")

  if (add_hlines %in% c("yes", "y")) {

    depth_vals_input <- safe_readline(
      "Enter depths separated by commas (e.g., 12.5, 34, 56): "
    )

    depth_vals <- suppressWarnings(
      as.numeric(strsplit(depth_vals_input, ",")[[1]])
    )

    depth_vals <- depth_vals[!is.na(depth_vals)]

    if (length(depth_vals) > 0) {
      main_plot <- main_plot +
        geom_hline(
          yintercept = depth_vals,
          linetype = "dashed",
          linewidth = 0.3,
          color = "black"
        )

      cat("\nHorizontal dashed lines added at depths:\n")
      cat("\n")
      print(depth_vals)
    } else {
      cat("\nNo valid depths provided. No lines added.\n")
    }
  }

  if (use_clusters) {
    main_plot <- main_plot +
      scale_color_gradientn(colors = custom_palette)
  }


  ## Virtual Core
  cat("\n\n==============================\n")
  cat("  VIRTUAL CORE OPTION\n")
  cat("==============================\n\n")

  if (use_clusters) {
    show_core <- tolower(safe_readline("Show Virtual Core? (yes/no): ", default = "no"))
  } else {
    show_core <- "no"
    cat("\nVirtual Core disabled (no clusters).\n")
  }


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

    if (!is.null(depth_vals) && length(depth_vals) > 0) {
      core_plot <- core_plot +
        geom_hline(
          yintercept = depth_vals,
          linetype   = "dashed",
          linewidth  = 0.3,
          color      = "black"
        )
    }

    combined_plot <- main_plot + core_plot + patchwork::plot_layout(widths = c(4, 0.25))
    print(combined_plot)
  } else {
    print(main_plot)
    cat("\nVirtual Core skipped.\n")
  }


  cat("\n\n==============================\n")
  cat("  AGE MODEL OPTION\n")
  cat("==============================\n\n")

  add_age_model <- tolower(safe_readline(
    "Would you like to add an age-depth model? (yes/no): ",
    default = "no"
  ))

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
        cat("\n")
        print(sheets)
        cat("\n")
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


      Depth_start <- min(xrfStrat[[depth_col]], na.rm = TRUE)
      Depth_end <- max(xrfStrat[[depth_col]], na.rm = TRUE)

      age_data <- age_data %>%
        dplyr::filter(
          .data[[age_depth_col]] >= Depth_start,
          .data[[age_depth_col]] <= Depth_end
        )


      ## Graphique du modèle d'âge
      age_plot <- ggplot(
        age_data,
        aes(
          x = .data[[age_col]],
          y = .data[[age_depth_col]]
        )
      ) +
        geom_path(linewidth = 0.25, color = "black") +
        scale_y_reverse(limits = c(Depth_end, Depth_start))+
        labs(
          x = paste0("Age [", age_unit, "]"),
          y = paste0("Depth [", depth_unit, "]"),
          title = "Age model"
        ) +
        tidypaleo::theme_paleo() +
        theme(
          text = element_text(family = "sans", face = "bold"),
          axis.text = element_text(size = 11, color = "gray25"),
          axis.title = element_text(size = 13),
          strip.text = element_text(face = "bold", size = 12),
          legend.position = "none"
        )

    }
  }


  cat("\n\n==============================\n")
  cat("  SEDIMENT CORE PICTURE OPTION\n")
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
  cat("  EXPORT GRAPH TO PDF OPTION\n")
  cat("==============================\n\n")
  save_pdf <- tolower(safe_readline("Would you like to export the figure as a PDF? (yes/no): ", default = "no"))


  get_numeric_input <- function(prompt, default) {
    repeat {
      val <- safe_readline(prompt, default = as.character(default))
      val_num <- suppressWarnings(as.numeric(val))
      if (!is.na(val_num) && val_num > 0) return(val_num)
      cat("\nInvalid input. Please enter a positive number or press Enter for default.\n\n")
    }
  }


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
        pdf_width  <- get_numeric_input("Enter desired PDF width (in inches, [default = 10]): ", 10)
        cat("\n")
        pdf_height <- get_numeric_input("Enter desired PDF height (in inches, [default = 8]): ", 8)
        cat("\n")

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
  cat("  AGE–DEPTH PLOT OPTION\n")
  cat("==============================\n\n")

  show_age_depth_plot <- tolower(
    safe_readline("Would you like to display the Age–Depth geochemistry plot? (yes/no): ",
                  default = "no")
  )



  if (show_age_depth_plot %in% c("yes", "y")) {

    ## Si aucun age_data, on redemande un modèle d'âge
    if (!exists("age_data")) {

      cat("\nNo age model found. Please select an age model file.\n")
      cat("\n")

      age_file <- rstudioapi::selectFile(
        caption  = "Select age model table",
        label    = "Open",
        path     = getwd(),
        filter   = list("Data files" = c("csv", "txt", "xlsx")),
        existing = TRUE
      )

      if (!nzchar(age_file)) {
        cat("\nNo age model selected. Cannot continue.\n")
        return(NULL)
      }

      ext <- tolower(tools::file_ext(age_file))

      if (ext == "xlsx") {
        sheets <- readxl::excel_sheets(age_file)
        print(sheets)
        cat("\n")
        sheet_name <- safe_readline("Enter the sheet name containing the age model: ")
        cat("\n")
        age_data <- readxl::read_excel(age_file, sheet = sheet_name)
      } else {
        age_data <- read.csv(age_file, stringsAsFactors = FALSE)
      }

      repeat {
        cat("Column in the dataframe:\n")
        cat("\n")
        print(names(age_data))
        cat("\n")
        age_depth_col <- safe_readline("Enter the depth column name: ")
        cat("\n")
        age_col       <- safe_readline("Enter the age (years) column name: ")
        cat("\n")
        if (all(c(age_depth_col, age_col) %in% names(age_data))) break
        cat("\nInvalid column names. Try again.\n")
        cat("\n")
      }

      age_unit <- safe_readline("Enter age unit (e.g., cal yr BP, ka, yr): ", default = "yr")
    }
    cat("\n\n==============================\n")
    cat("  AGE–DEPTH: CLUSTER DISPLAY OPTION\n")
    cat("==============================\n\n")

    show_clusters <- tolower(
      safe_readline("Would you like to display clusters? (yes/no): ", default = "yes")
    )
    use_clusters_age <- show_clusters %in% c("yes", "y")  # <--- nom uniforme

    if (use_clusters_age) {
      if (!"Cluster" %in% names(data)) {
        stop("The ‘Cluster’ column is required when cluster display is enabled.")
      }
      data$Cluster <- as.numeric(data$Cluster)
      n_clusters <- length(unique(na.omit(data$Cluster)))
    }

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
        # variables <- setdiff(names(data), c(depth_col, "Cluster"))
        variables <- setdiff(names(data),c(depth_col, if (use_clusters_age) "Cluster"))
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
    if (use_clusters_age) {
      cat("\n\n==============================\n")
      cat("  COLOR PALETTE SELECTION\n")
      cat("==============================\n\n")
      use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): ", default = "no"))

      if (use_custom %in% c("yes", "y")) {
        cat("\nEnter", n_clusters, "colors (by name or hexadecimal, e.g., 'red' or '#FF0000')\n\n")
        custom_palette <- sapply(seq_len(n_clusters), function(i) safe_readline(paste("Color for cluster", i, ": ")))
      } else {
        custom_palette <- colorRampPalette(c("#35163b", "#662483", "#f39200", "#f9b233", "#ffda77"))(n_clusters)
      }
    }

    ## Data preparation
    cat("\n\n==============================\n")
    cat("  DATA PREPARATION\n")
    cat("==============================\n\n")

    cols_to_keep <- c(variables, depth_col)
    if (use_clusters_age) cols_to_keep <- c(cols_to_keep, "Cluster")

    xrfStrat <- data %>%
      dplyr::select(all_of(cols_to_keep)) %>%
      tidyr::pivot_longer(
        cols = -all_of(c(depth_col, if (use_clusters_age) "Cluster")),
        names_to = "elements",
        values_to = "peakarea"
      ) %>%
      tidyr::drop_na(any_of(c(variables, depth_col)))%>%
      mutate(
        elements = factor(elements, levels = variables)
      )

    depth_unit <- safe_readline("\nEnter the depth unit (default = mm): ", default = "mm")


    ## Jointure xrf + âge
    xrf_age <- xrfStrat %>%
      left_join(
        age_data %>% dplyr::select(all_of(c(age_depth_col, age_col))),
        by = setNames(age_depth_col, depth_col)
      ) %>%
      dplyr::rename(Age = all_of(age_col))

    xrf_age <- xrf_age %>%
      arrange(.data[[depth_col]]) %>%
      group_by(elements) %>%
      mutate(
        depth_diff = .data[[depth_col]] - lag(.data[[depth_col]], default = first(.data[[depth_col]])),
        depth_step = median(diff(.data[[depth_col]]), na.rm = TRUE),
        is_gap = depth_diff > 1.9999999999999996 * depth_step,
        segment = cumsum(is_gap)
      ) %>%
      ungroup()

    # Colonnes de base
    cols_export <- c(depth_col, "elements", "peakarea", "Age")

    # Ajouter Cluster seulement si présent et si l'utilisateur veut l'utiliser
    if (use_clusters_age && "Cluster" %in% names(xrf_age)) {
      cols_export <- c(cols_export, "Cluster")
    }

    xrf_age_export <- xrf_age %>%
      dplyr::select(all_of(cols_export)) %>%
      tidyr::pivot_wider(
        names_from  = elements,
        values_from = peakarea
      )

    # Relocate uniquement si les colonnes existent
    if ("Age" %in% names(xrf_age_export)) {
      xrf_age_export <- xrf_age_export %>% dplyr::relocate(Age, .after = last_col())
    }
    if ("Cluster" %in% names(xrf_age_export)) {
      xrf_age_export <- xrf_age_export %>% dplyr::relocate(Cluster, .after = Age)
    }



    if (use_clusters_age) {
      xrf_age$Cluster <- factor(xrf_age$Cluster, levels = sort(unique(xrf_age$Cluster)))
      n_clusters <- length(levels(xrf_age$Cluster))
    }

    dual_plot <- xrf_age %>%
      ggplot(aes(
        x = peakarea,
        y = .data[[depth_col]],
        group = interaction(elements, segment)
      ))
    if (use_clusters_age && "Cluster" %in% names(xrf_age)) {
      dual_plot <- dual_plot +
        geom_lineh(aes(color = factor(Cluster)), linewidth = 0.25, na.rm = TRUE) +
        scale_color_manual(values = custom_palette, name = "Cluster")
    } else {
      dual_plot <- dual_plot +
        geom_lineh(color = "black", linewidth = 0.25, na.rm = TRUE)
    }


    dual_plot <- dual_plot +
      tidypaleo::facet_geochem_gridh(vars(elements)) +
      scale_y_reverse(
        name = paste0("Depth [", depth_unit, "]"),
        sec.axis = sec_axis(
          trans = ~ .,
          labels = function(depths) {
            sapply(depths, function(d) {
              idx <- which.min(abs(xrf_age[[depth_col]] - d))
              sprintf("%.2f", xrf_age$Age[idx])
            })
          },
          name = paste0("Age [", age_unit, "]")
        )
      ) +
      labs(x = "Geochemistry") +
      tidypaleo::theme_paleo() +
      theme(
        text = element_text(family = "sans", face = "bold"),
        axis.text = element_text(size = 11, color = "gray25"),
        axis.title = element_text(size = 13),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none"
      )

    cat("\n\n===============================\n")
    cat("  HORIZONTAL AGE LINES OPTION\n")
    cat("===================================\n\n")

    age_vals <- NULL

    add_hlines <- tolower(safe_readline(
      "Would you like to add horizontal age lines? (yes/no): ",
      default = "no"
    ))

    cat("\n")

    if (add_hlines %in% c("yes", "y")) {

      ages_input <- safe_readline(
        "Enter ages separated by commas (e.g., 2024, 1980, 1500): "
      )

      # Convertir en numérique
      age_vals <- suppressWarnings(as.numeric(strsplit(ages_input, ",")[[1]]))
      age_vals <- age_vals[!is.na(age_vals)]

      if (length(age_vals) > 0) {

        # Profondeur la plus proche pour chaque âge utilisateur
        depth_for_lines <- sapply(age_vals, function(age) {
          idx <- which.min(abs(xrf_age$Age - age))
          xrf_age[[depth_col]][idx]
        })

        # Ticks réguliers sur la profondeur
        depth_ticks_regular <- pretty(range(xrf_age[[depth_col]], na.rm = TRUE), n = 5)

        # Fusion réguliers + utilisateur
        all_depth_ticks <- sort(unique(c(depth_ticks_regular, depth_for_lines)))

        # Labels Age pour TOUS les ticks
        age_labels <- sapply(all_depth_ticks, function(d) {
          idx <- which.min(abs(xrf_age[[depth_col]] - d))
          sprintf("%.0f", xrf_age$Age[idx])
        })

        # Écraser uniquement ceux correspondant aux âges utilisateur (précision + format)
        for (i in seq_along(depth_for_lines)) {
          pos <- which.min(abs(all_depth_ticks - depth_for_lines[i]))
          age_labels[pos] <- sprintf("%.2f", age_vals[i])
        }

        # Ajout au plot
        dual_plot <- dual_plot +
          geom_hline(
            yintercept = depth_for_lines,
            linetype = "dashed",
            linewidth = 0.3,
            color = "black"
          ) +
          scale_y_reverse(
            name = paste0("Depth [", depth_unit, "]"),
            sec.axis = sec_axis(
              trans  = ~ .,
              breaks = all_depth_ticks,
              labels = age_labels,
              name   = paste0("Age [", age_unit, "]")
            )
          )

        cat("\nHorizontal dashed lines added at ages:\n")
        print(sprintf("%.2f", age_vals))

      } else {
        cat("\nNo valid ages provided. No lines added.\n")
      }
    }

    cat("\n\n=================================\n")
    cat("  VIRTUAL CORE (AGE–DEPTH)\n")
    cat("=====================================\n\n")

    if (use_clusters_age) {
      show_core_age <- tolower(
        safe_readline("Show Virtual Core with Age–Depth plot? (yes/no): ", default = "no")
      )
    } else {
      show_core_age <- "no"
      print(dual_plot)
      cat("\nVirtual Core disabled (no clusters).\n")
    }

    if (show_core_age %in% c("yes", "y")) {

      depth_vals <- sapply(age_vals, function(age) {
        idx <- which.min(abs(xrf_age$Age - age))  # index du Age le plus proche
        xrf_age[[depth_col]][idx]                  # profondeur correspondante
      })

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

      core_plot_age <- ggplot(blocks,
                              aes(x = 1, y = Depth_mid,
                                  fill = factor(Cluster),
                                  height = Height)) +
        geom_tile(width = 1) +
        scale_y_reverse(position = "right") +
        scale_fill_manual(values = custom_palette, name = "Clusters", na.value = "white") +
        labs(x = NULL, y = NULL, title = "Virtual Core") +
        tidypaleo::theme_paleo() +
        theme(
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid   = element_blank(),
          legend.position = "right",
          text = element_text(family = "sans", face = "bold")
        )

      if (!is.null(depth_vals) && length(depth_vals) > 0) {
        core_plot_age <- core_plot_age +
          geom_hline(
            yintercept = depth_vals,
            linetype   = "dashed",
            linewidth  = 0.3,
            color      = "black"
          )

        combined_age_depth_plot <- dual_plot + core_plot_age +
          patchwork::plot_layout(widths = c(4, 0.25))

        print(combined_age_depth_plot)

        plot_to_export <- combined_age_depth_plot

      } else {
        print(dual_plot)
        cat("\nVirtual Core skipped for Age–Depth plot.\n")
        plot_to_export <- dual_plot
      }


      ## Sauvegarde dans l'environnement global
      xrf_age_export <<- xrf_age_export
      cat("\n")
      cat("\nExport-ready table saved as `xrf_age_export` in Global Environment.\n")
      cat("\n")

      ## Option export CSV / XLSX
      save_table <- tolower(safe_readline(
        "Would you like to export the joined table (xrf_age_export)? (yes/no): ",
        default = "no"
      ))

      if (save_table %in% c("yes", "y")) {

        export_format <- tolower(safe_readline(
          "Choose export format (csv/xlsx): ",
          default = "csv"
        ))

        if (!export_format %in% c("csv", "xlsx")) {
          cat("\nInvalid format. Export skipped.\n")
        } else {

          table_path <- rstudioapi::selectFile(
            caption  = paste("Save joined table as", toupper(export_format)),
            label    = "Save",
            path     = getwd(),
            filter   = setNames(list(export_format), toupper(export_format)),
            existing = FALSE
          )

          if (nzchar(table_path)) {

            if (export_format == "xlsx") {
              if (!grepl("\\.xlsx$", table_path, ignore.case = TRUE))
                table_path <- paste0(table_path, ".xlsx")
              writexl::write_xlsx(xrf_age_export, table_path)

            } else if (export_format == "csv") {
              if (!grepl("\\.csv$", table_path, ignore.case = TRUE))
                table_path <- paste0(table_path, ".csv")
              write.csv(xrf_age_export, table_path, row.names = FALSE)
            }

            cat("\nTable saved to:", table_path, "\n")
          } else {
            cat("\nTable export cancelled.\n")
          }
        }
      }

      cat("\n\n==============================\n")
      cat("  EXPORT GRAPH TO PDF OPTION\n")
      cat("==============================\n\n")
      save_pdf <- tolower(safe_readline("Would you like to export the figure as a PDF? (yes/no): ", default = "no"))

      get_numeric_input <- function(prompt, default) {
        repeat {
          val <- safe_readline(prompt, default = as.character(default))
          val_num <- suppressWarnings(as.numeric(val))
          if (!is.na(val_num) && val_num > 0) return(val_num)
          cat("\nInvalid input. Please enter a positive number or press Enter for default.\n\n")
        }
      }

      if (save_pdf %in% c("yes", "y")) {
        if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
        library(rstudioapi)

        if (rstudioapi::isAvailable()) {
          pdf_path <- rstudioapi::selectFile(
            caption = "Save cluster visualization",
            label   = "Save",
            path    = getwd(),
            filter  = list("PDF files" = "pdf"),
            existing = FALSE
          )

          if (nzchar(pdf_path)) {
            if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE))
              pdf_path <- paste0(pdf_path, ".pdf")

            pdf_width  <- get_numeric_input("Enter desired PDF width (in inches, [default = 10]): ", 10)
            cat("\n")
            pdf_height <- get_numeric_input("Enter desired PDF height (in inches, [default = 8]): ", 8)
            cat("\n")

            pdf(pdf_path, width = pdf_width, height = pdf_height)

            if (exists("plot_to_export")) {
              print(plot_to_export)
            } else {
              cat("\nNo plot available to export.\n")
            }

            dev.off()
            cat("\n\nCluster-Depth-Age visualization saved to:", pdf_path, "\n")
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
  }
}
