#' Fonction pour normaliser les données, déterminer le nombre optimal de clusters,
#' et ajouter les résultats au dataframe choisi par l'utilisateur
#'
#' @param data Un dataframe contenant les données numériques à analyser
#' @return Le dataframe avec la colonne "Cluster" ajoutée
#' @export


# Fonction utilitaire pour sécuriser les entrées utilisateur

xrf_clust <- function(data = NULL) {

  # === Required packages ===
  required_packages <- c("NbClust", "ggplot2", "writexl", "rstudioapi", "factoextra")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  # === Safe readline wrapper ===
  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User interrupted execution via exit.")
    return(input)
  }

  # === Select or load data ===
  repeat {
    if ("variables_cluster_cache" %in% list_cache()) {
      data <- get_cache("variables_cluster_cache")
      message("Using cached 'variables_cluster_cache' dataframe.")
      break
    }

    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

    if (length(df_list) == 0) stop("No data frames found in the global environment.")

    cat("\nSelect the dataframe with the selected elements:\n")
    print(df_list)
    cat("\n")
    df_name <- readline("Enter the name of the dataframe: ")

    if (df_name %in% df_list) {
      data <- get(df_name, envir = .GlobalEnv)
      set_cache("variables_cluster_cache", data)
      break
    }
    cat("\nDataframe not found. Try again.\n")
  }

  # === Input validation ===
  if (!is.data.frame(data) && !is.matrix(data)) stop("The input must be a dataframe or matrix.")
  if (!all(sapply(data, is.numeric))) stop("All columns must be numeric.")

  # === Main clustering loop ===
  repeat {
    set.seed(123)
    cat("\n====== Clustering Analysis (K-means method) ======\n\n")

    # --- Select max number of clusters ---
    repeat {
      max_clusters <- as.integer(safe_readline("Enter max number of clusters to test (min 2, e.g. 10): "))
      if (!is.na(max_clusters) && max_clusters >= 2) break
      cat("Invalid number. Please enter integer ≥ 2.\n\n")
    }

    # --- Determine optimal cluster number ---
    cat("\n====== Determining Optimal Clusters ======\n\n")
    nb <- NbClust(data, distance = "euclidean", min.nc = 2, max.nc = max_clusters, method = "kmeans")

    results <- nb$Best.nc[1, ]
    freq_table <- sort(table(results), decreasing = TRUE)
    clusters_with_max_freq <- as.numeric(names(freq_table[freq_table == max(freq_table)]))

    if (length(clusters_with_max_freq) == 1) {
      optimal_clusters_max <- clusters_with_max_freq
    } else {
      repeat {
        cat("Multiple optimal cluster numbers detected:", paste(clusters_with_max_freq, collapse = ", "), "\n")
        user_input <- as.integer(safe_readline("Choose one of these cluster numbers: "))
        if (user_input %in% clusters_with_max_freq) {
          optimal_clusters_max <- user_input
          break
        }
        cat("Invalid choice. Try again.\n\n")
      }
    }

    cat("\nOptimal number of clusters selected:", optimal_clusters_max, "\n\n")

    # --- Apply K-means ---
    clustering_result <- kmeans(data, centers = optimal_clusters_max, nstart = 100)

    # --- Add cluster column ---
    repeat {
      if ("df_denoised_total_cache" %in% list_cache()) {
        df_denoised_total <- get_cache("df_denoised_total_cache")
        df_denoised_total$Cluster <- clustering_result$cluster
        set_cache("df_denoised_total_cache", df_denoised_total)
        assign("Dataframe_Clustering",df_denoised_total, envir = .GlobalEnv)
        cat("\nCluster column added to cached 'df_denoised_total'.\n\n")
        break
      }

      df_list <- ls(envir = .GlobalEnv)
      df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
      if (length(df_list) == 0) stop("No data frames found in the global environment.")

      cat("\n=== Select the dataframe to which the 'Cluster' column will be added ===\n\n")
      cat("Dataframes available:\n")
      print(df_list)
      cat("\n")

      df_name <- safe_readline("Enter the name of the target dataframe: ")
      if (df_name %in% df_list) {
        df_denoised_total <- get(df_name, envir = .GlobalEnv)
        df_denoised_total$Cluster <- clustering_result$cluster
        assign(df_name, df_denoised_total, envir = .GlobalEnv)
        cat("\nCluster column added to", df_name, "\n\n")
        break
      } else {
        cat("Dataframe not found. Try again.\n\n")
      }
    }

    # --- Optional save dataframe ---
    save_choice <- tolower(safe_readline("Would you like to save this dataframe? (yes/no): "))
    if (save_choice %in% c("yes", "y")) {
      repeat {
        ext_choice <- tolower(safe_readline("Desired extension: csv or xlsx: "))
        if (!ext_choice %in% c("csv", "xlsx")) { cat("Invalid extension.\n"); next }
        if (!rstudioapi::isAvailable()) { cat("Saving requires RStudio.\n"); break }

        cat("\nSelect the destination file...\n")
        path_save <- rstudioapi::selectFile(
          caption = "Save file",
          label = "Save",
          path = getwd(),
          filter = if (ext_choice == "csv") list("CSV files" = "csv") else list("Excel files" = "xlsx"),
          existing = FALSE
        )
        if (!nzchar(path_save)) { cat("Saving cancelled.\n"); break }
        if (!grepl(paste0("\\.", ext_choice, "$"), path_save, ignore.case = TRUE))
          path_save <- paste0(path_save, ".", ext_choice)

        if (ext_choice == "csv") write.csv(df_denoised_total, path_save, row.names = FALSE)
        else writexl::write_xlsx(df_denoised_total, path = path_save)

        cat("\nSaved file:", path_save, "\n\n")
        break
      }
    }

    # --- Cluster Drivers Analysis ---
    cat("=== Cluster Drivers Analysis ===\n\n")
    cat("Optimal number of clusters for your data:", optimal_clusters_max, "\n\n")

    cluster_profiles <- aggregate(data, by = list(Cluster = clustering_result$cluster), mean)
    anova_results <- data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

    for (col in colnames(data)) {
      model <- aov(data[[col]] ~ as.factor(clustering_result$cluster))
      s <- summary(model)[[1]]
      anova_results <- rbind(anova_results, data.frame(
        Variable = col,
        F_value = s$`F value`[1],
        p_value = s$`Pr(>F)`[1]
      ))
    }

    anova_results <- anova_results[order(-anova_results$F_value), ]
    anova_results$F_value <- round(anova_results$F_value, 1)
    anova_results$p_value <- signif(anova_results$p_value, 3)
    print(anova_results)
    cat("\n")

    # --- Cluster description profiles ---
    cat("=== Cluster Description Profiles ===\n\n")
    vars <- colnames(cluster_profiles)[-1]
    for (i in 1:nrow(cluster_profiles)) {
      profile_text <- c()
      for (v in vars) {
        val <- cluster_profiles[i, v]
        q <- quantile(cluster_profiles[[v]], probs = c(0.25, 0.5, 0.75))
        if (val >= q[3]) lvl <- "very high"
        else if (val >= q[2]) lvl <- "high"
        else if (val >= q[1]) lvl <- "medium"
        else lvl <- "low"
        profile_text <- c(profile_text, paste(v, lvl, sep = ": "))
      }
      cat(paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse = ", "), "\n"))
    }

    # --- Optional PCA visualization ---
    cat("\n=== PCA and Cluster Visualization ===\n\n")
    show_acp <- tolower(safe_readline("Would you like to visualize a PCA with the clusters? (yes/no): "))
    if (show_acp %in% c("yes", "y")) {

      if ("df_denoised_total_cache" %in% list_cache()) {
        df_cluster <- get_cache("df_denoised_total_cache")
      } else {
        cat("The dataframe 'df_denoised_total_cache' was not found.\n")
      }

      repeat {
        if ("df_normalized_cache" %in% list_cache()) {
          df_vars <- get_cache("df_normalized_cache")
          message("Using cached 'df_normalized_cache' dataframe.")
          break
        }
        df_vars_name <- safe_readline("Enter the dataframe used for PCA (in prop.select): ")
        if (exists(df_vars_name, envir = .GlobalEnv)) {
          df_vars <- get(df_vars_name, envir = .GlobalEnv)
          if (all(sapply(df_vars, is.numeric))) break
          cat("Selected dataframe must contain only numeric columns.\n")
        } else cat("Dataframe not found.\n")
      }

      df_cluster$Cluster <- as.factor(df_cluster$Cluster)
      res.pca <- prcomp(df_vars, scale. = FALSE)
      n_clusters <- length(levels(df_cluster$Cluster))

      use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): "))
      if (use_custom %in% c("yes", "y")) {
        cat("Enter", n_clusters, "colors (by name or hex):\n")
        user_colors <- character(n_clusters)
        for (i in seq_len(n_clusters)) user_colors[i] <- safe_readline(paste("Color for cluster", i, ": "))
        palette_finale <- user_colors
      } else {
        palette_finale <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(n_clusters)
      }

      cat("Palette used:", paste(palette_finale, collapse = ", "), "\n\n")

      p <- fviz_pca_biplot(
        res.pca, geom.ind = "point", pointshape = 16, pointsize = 2.25,
        alpha.ind = 0.9, col.ind = df_cluster$Cluster, palette = palette_finale,
        addEllipses = TRUE, ellipse.level = 0.95, mean.point = FALSE, label = "var",
        col.var = "gray35", arrowsize = 0.5, repel = TRUE, legend.title = "Cluster"
      ) +
        ggtitle("Cluster structure across PCA projection") +
        theme_minimal(base_family = "sans", base_size = 14)

      print(p)

      save_pdf <- tolower(safe_readline("Would you like to export the PCA plot as PDF? (yes/no): "))
      if (save_pdf %in% c("yes", "y") && rstudioapi::isAvailable()) {
        pdf_path <- rstudioapi::selectFile(
          caption = "Save PCA plot", label = "Save", path = getwd(),
          filter = list("PDF files" = "pdf"), existing = FALSE
        )
        if (nzchar(pdf_path)) {
          if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE)) pdf_path <- paste0(pdf_path, ".pdf")
          pdf(pdf_path, width = 10, height = 8)
          print(p)
          dev.off()
          cat("PCA plot saved to:", pdf_path, "\n")
        } else cat("Saving cancelled.\n")
      }
    }


    # --- Optional geochemical visualization ---
    cat("\n")
    show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))

    if (show_geo %in% c("yes", "y", "oui", "o")) {

      if (exists("visual.clust")) {

        repeat {

          #check: is cached df available?
          if ("df_denoised_total_cache" %in% list_cache()) {
            df_denoised_total <- get_cache("df_denoised_total_cache")
            message("Using cached 'df_denoised_total_cache' dataframe.")

            # Check if 'Cluster' column exists
            if ("Cluster" %in% colnames(df_denoised_total)) {
              visual.clust(df_denoised_total)
              break
            } else {
              cat("Cached dataframe found but no 'Cluster' column detected.\n")
            }

          } else {
            cat("No cached dataframe found under 'df_denoised_total_cache'.\n")
          }

          # Ask user for dataframe manually if cache missing or invalid
          df_visu_name <- safe_readline("Enter the name of the dataframe containing 'Cluster': ")
          if (exists(df_visu_name, envir = .GlobalEnv)) {
            df_visu <- get(df_visu_name, envir = .GlobalEnv)
            if ("Cluster" %in% colnames(df_visu)) {
              visual.clust(df_visu)
              break
            } else {
              cat("The selected dataframe does not contain a 'Cluster' column.\n")
            }
          } else {
            cat("Dataframe not found in global environment.\n")
          }

          # Let the user retry
          cat("\nTry again.\n")
        }

      } else {
        cat("Function 'visual.clust()' not found. Please load it before running this option.\n")
      }
    }

    # --- Restart clustering? ---
    restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
    if (tolower(restart) %in% c("no", "n")) {
      cat("Clustering finished.\n")
      break
    }
  }
}
