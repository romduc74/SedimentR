sed.change <- function() {
  # Charger les packages requis
  pkgs <- c("readxl", "changepoint", "dplyr", "tidyr", "ggplot2", "tidypaleo", "rstudioapi")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  # 1. Sélection du fichier principal
  repeat {
    file <- rstudioapi::selectFile(
      caption = "Select a CSV or XLSX file",
      filter = c("CSV files" = "*.csv", "Excel files" = "*.xlsx"),
      path = getwd(),
      existing = TRUE
    )
    if (length(file) != 0 && file != "") break
    cat("No file selected. Please try again.\n")
  }
  ext <- tools::file_ext(file)

  # 2. Lecture du fichier principal
  repeat {
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(file)
      cat("Available sheets:\n"); print(sheets)
      sheet <- readline("Name of the sheet to use: ")
      if (sheet %in% sheets) {
        data <- readxl::read_excel(file, sheet = sheet)
        assign("data", data, envir = .GlobalEnv)
        break
      } else cat(" Sheet not found. Try again.\n")
    } else if (ext == "csv") {
      sep_choice <- readline("Separator CSV ? (1=',' 2=';' 3='\t' 4=' '): ")
      sep <- switch(sep_choice, "1" = ",", "2" = ";", "3" = "\t", "4" = " ")
      if (!is.null(sep)) {
        data <- read.csv(file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE)
        assign("data", data, envir = .GlobalEnv)
        break
      } else cat(" Invalid separator. Try again.\n")
    } else stop(" File type not supported.")
  }

  # 3. Choix des colonnes
  cat("Available columns:\n"); print(names(data))
  repeat {
    depth_col <- readline("Depth column name: ")
    if (depth_col %in% names(data)) break
    cat(" Column not found. Try again.\n")
  }
  repeat {
    xrf_col <- readline("Name of the variable to be analyzed (breakpoints): ")
    if (xrf_col %in% names(data)) break
    cat(" Column not found. Try again.\n")
  }
  repeat {
    vars_input <- readline("Variables columns to display (comma-separated or 'all'): ")
    if (tolower(vars_input) == "all") {
      variables <- names(data)[sapply(data, is.numeric)]
      variables <- setdiff(variables, depth_col)
      break
    } else {
      variables <- trimws(strsplit(vars_input, ",")[[1]])
      if (all(variables %in% names(data))) break
      cat(" One or more columns not found. Try again.\n")
    }
  }
  cat("\n")
  # 4. Extraction des données numériques
  x <- data[[xrf_col]]
  # x <- data[[xrf_col]]
  depth <- data[[depth_col]]
  cat("\n")

  na_idx <- is.na(x) | is.na(depth)
  if (any(na_idx)) {
    cat("⚠️ Warning: Missing values (NA) found in selected data or depth column.\n")
    cat(paste0("→ ", sum(na_idx), " rows contain NA values.\n"))
    remove_na <- tolower(readline("Do you want to remove rows with NA? (yes/no): "))
    if (remove_na %in% c("yes", "y", "oui", "o")) {
      x <- x[!na_idx]
      depth <- depth[!na_idx]
      data <- data[!na_idx, ]
      cat("NA rows removed.\n")
    } else {
      stop("Process aborted due to NA values.")
    }
  }

  cat("\n")

  if (!is.numeric(x) || !is.numeric(depth)) stop("The depth and variable columns must be numeric.")

  temp_df <- data.frame(Depth = depth, Variable = x)

  view_choice <- tolower(readline("Do you want to check the data with View()? (yes/no): "))
  if (view_choice %in% c("yes", "y", "oui", "o")) {
    View(temp_df)
  }

  # 5. Analyse de l’échelle des données et pénalité automatique
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(abs(x), na.rm = TRUE)
  pen_val <- if (min_val < 0.1) {
    0.005
  } else if (min_val < 1) {
    0.01
  } else if (min_val < 10) {
    0.1
  } else {
    1
  }




  cat("\n---- Analysis of the variable data scale ----\n")
  cat("\n")
  cat(paste0("Minimum value observed in '", xrf_col, "' : ", round(min_val, 4), "\n"))
  cat(paste0("Maximum value observed in '", xrf_col, "' : ", round(max_val, 4), "\n"))
  cat("\n")
  cat("Penalty automatically adjusted according to this scale:\n")
  cat(paste0("→ pen.value = ", pen_val, "  (smaller penalty → more sensitive detection)\n"))


  # 5. Méthode de détection
  methodes <- c("PELT", "BinSeg", "SegNeigh", "AMOC", "Manual")
  repeat {
    cat("Available methods:\n")
    for (i in seq_along(methodes)) cat(i, ":", methodes[i], "\n")
    method_num <- as.integer(readline("Number of the method to be used: "))
    if (!is.na(method_num) && method_num %in% 1:length(methodes)) break
    cat(" Invalid choice. Try again.\n")
  }
  method <- methodes[method_num]

  max_breakpoints <- NULL
  if (method %in% c("BinSeg", "SegNeigh")) {
    repeat {
      max_breakpoints_input <- readline("Maximum number of breakpoints (Q)? Leave empty for default (10): ")
      if (max_breakpoints_input == "") {
        max_breakpoints <- 10
        break
      }
      max_breakpoints <- as.integer(max_breakpoints_input)
      if (!is.na(max_breakpoints) && max_breakpoints > 0) break
      cat("Invalid input. Please enter a positive integer.\n")
    }
  }

  if (min_val < 1) {
    cat("️WARNING: Low minimum value. Manual penalty is recommended.\n")
  }

  pen <- readline("Type of penalty (MBIC, BIC, SIC, AIC, Manual) [Default MBIC]: ")
  if (pen == "") pen <- "MBIC"
  use_manual <- pen == "Manual" || (pen != "Manual" && min_val < 1)

  # 8. Détection avec contrôle automatique de la pénalité "Manual" selon max_val
  use_manual <- FALSE
  if (pen != "Manual" && min_val < 1) {
    cat("️WARNING: Minimum value low (", round(min_val, 4), "): Automatic switch to ‘Manual’ penalty with pen.valuee = ", pen_val, "\n")
    use_manual <- TRUE
  } else if (pen == "Manual") {
    use_manual <- TRUE
  }

  repeat {
    cat("\nChange type: 1-Mean | 2-Variance | 3-Mean & Variance\n")
    change_type_num <- as.integer(readline("Change type number: "))
    if (!is.na(change_type_num) && change_type_num %in% 1:3) break
    cat(" Invalid type. Try again.\n")
  }
  change_type <- c("mean", "var", "meanvar")[change_type_num]
  cpt_function <- switch(change_type,
                         mean = changepoint::cpt.mean,
                         var = changepoint::cpt.var,
                         meanvar = changepoint::cpt.meanvar)

  # 6. Application de la détection
  cat("\n--- Detection in progress ---\n")

  cpt <- if (use_manual) {
    if (method %in% c("BinSeg", "SegNeigh")) {
      cpt_function(x, method = method, penalty = "Manual", pen.value = pen_val, test.stat="Normal",Q = max_breakpoints)
    } else {
      cpt_function(x, method = method, penalty = "Manual",test.stat="Normal", pen.value = pen_val)
    }
  } else {
    if (method %in% c("BinSeg", "SegNeigh")) {
      cpt_function(x, method = method, penalty = pen,test.stat="Normal", Q = max_breakpoints)
    } else {
      cpt_function(x, method = method, test.stat="Normal", penalty = pen)
    }
  }
  bps <- changepoint::cpts(cpt)
  cat("\nDetected breakpoints at these depths:\n")
  print(depth[bps])

  # 7. Visualisation segmentée (style tidypaleo sans cluster)
  # Préparation de la segmentation multi-variables

  # Récupérer uniquement variables numériques demandées + depth
  data_vars <- data %>%
    dplyr::select(all_of(c(depth_col, variables)))

  # Convertir en format long pour les variables (Signal)
  signal_long <- data_vars %>%
    tidyr::pivot_longer(
      cols = -all_of(depth_col),
      names_to = "elements",
      values_to = "peakarea"
    ) %>%
    dplyr::rename(depth = all_of(depth_col)) %>%
    drop_na()

  # Calcul de la segmentation pour chaque élément
  segmentation_long <- signal_long %>%
    group_by(elements) %>%
    mutate(
      # créer segment selon breakpoints
      segment = cut(
        row_number(),
        breaks = c(0, bps, n()),
        include.lowest = TRUE,
        labels = FALSE
      )
    ) %>%
    group_by(elements, segment) %>%
    mutate(peakarea = mean(peakarea, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-segment) %>%
    mutate(Type = "Segmentation")

  # Ajouter Type aux données signal
  signal_long <- signal_long %>% mutate(Type = "Variable")

  # Combiner signal + segmentation
  combined_plot_data <- bind_rows(signal_long, segmentation_long)

  # Plot multi-variables (éléments) avec signal + segmentation dans le même facet
  p_combined <- combined_plot_data %>%
    ggplot(aes(x = peakarea, y = depth, color = Type)) +
    geom_lineh(size = 0.8) +
    scale_y_reverse() +
    scale_color_manual(values = c("Variable" = "black", "Segmentation" = "red")) +
    tidypaleo::facet_geochem_gridh(vars(elements)) +
    labs(
      x = "Variable data",
      y = "Depth [mm]",
      color = "Type",
      title = "Signal and segmentation per variable"
    ) +
    tidypaleo::theme_paleo() +
    theme(legend.position = "right")

  print(p_combined)



  # 8. Données de clustering (optionnel)
  cluster_response <- readline("Do you have a clustering file? (yes/no): ")
  if (tolower(cluster_response) %in% c("yes", "y", "oui", "o")) {
    repeat {
      cluster_file <- rstudioapi::selectFile(
        caption = "Select the clustering file",
        filter = c("CSV files" = "*.csv", "Excel files" = "*.xlsx"),
        path = getwd(),
        existing = TRUE
      )
      if (length(cluster_file) != 0 && cluster_file != "") break
      cat(" No file selected. Try again.\n")
    }

    cluster_ext <- tools::file_ext(cluster_file)
    if (cluster_ext == "xlsx") {
      repeat {
        cluster_sheets <- readxl::excel_sheets(cluster_file)
        print(cluster_sheets)
        cluster_sheet <- readline("Name of the sheet to use: ")
        if (cluster_sheet %in% cluster_sheets) {
          cluster_data <- readxl::read_excel(cluster_file, sheet = cluster_sheet)
          assign("cluster_data", cluster_data, envir = .GlobalEnv)
          break
        } else cat(" Sheet not found. Try again.\n")
      }
    } else {
      repeat {
        sep_opt <- readline("Clustering separator? (1=',' 2=';' 3='\t' 4=' '): ")
        cluster_sep <- switch(sep_opt, "1" = ",", "2" = ";", "3" = "\t", "4" = " ")
        if (!is.null(cluster_sep)) {
          cluster_data <- read.csv(cluster_file, sep = cluster_sep, check.names = FALSE, stringsAsFactors = FALSE)
          assign("cluster_data", cluster_data, envir = .GlobalEnv)
          break
        } else cat(" Invalid separator. Try again.\n")
      }
    }

    print(names(cluster_data))
    repeat {
      cluster_var <- readline("Name of the cluster column: ")
      if (cluster_var %in% names(cluster_data)) break
      cat(" Column not found. Try again.\n")
    }
    repeat {
      cluster_depth <- readline("Name of the depth column in clustering: ")
      if (cluster_depth %in% names(cluster_data)) break
      cat(" Column not found. Try again.\n")
    }

    merged <- dplyr::left_join(
      data.frame(depth = depth, x = x),
      cluster_data[, c(cluster_depth, cluster_var)],
      by = setNames("depth", cluster_depth)
    )
    merged$cluster <- factor(merged[[cluster_var]])

    xrf_long <- merged %>%
      dplyr::rename(Depth = depth) %>%
      dplyr::mutate(
        Cluster = cluster,
        elements = xrf_col,
        peakarea = x
      ) %>%
      dplyr::select(Depth, Cluster, elements, peakarea) %>%
      tidyr::drop_na()

    xrf_long$Cluster <- as.numeric(as.character(xrf_long$Cluster))
    n_clusters <- length(unique(xrf_long$Cluster))

    repeat {
      use_custom <- tolower(readline("Custom palette? (yes/no): "))
      if (use_custom %in% c("yes", "y", "no", "n", "oui", "o")) break
      cat(" Please answer yes or no.\n")
    }

    if (use_custom %in% c("yes", "y", "oui", "o")) {
      user_colors <- character(n_clusters)
      for (i in seq_len(n_clusters)) {
        repeat {
          color <- readline(paste("Color for cluster", i, ": "))
          if (nzchar(color)) {
            user_colors[i] <- color
            break
          } else cat(" Invalid color. Try again.\n")
        }
      }
      custom_palette <- user_colors
    } else {
      custom_palette <- grDevices::colorRampPalette(c("#A52A2A", "#FFEFD5"))(n_clusters)
    }

    p2 <- xrf_long %>%
      ggplot(aes(x=peakarea,y = Depth)) +
      geom_lineh(aes(color = Cluster), size = 0.75) +
      scale_y_reverse() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      tidypaleo::facet_geochem_gridh(vars(elements)) +
      labs(
        x = xrf_col,
        y = "Depth [mm]",
        color = "Cluster",
        title = paste("Value", xrf_col, "with clusters")
      ) +
      tidypaleo::theme_paleo() +
      theme(legend.position = "right") +
      scale_color_gradientn(colors = custom_palette)
    print(p2)
  }
  cat("\nBreakpoints detected at the following depths:\n")
  return(depth[bps])
}
