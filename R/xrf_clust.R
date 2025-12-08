#' Fonction pour normaliser les données, déterminer le nombre optimal de clusters,
#' et ajouter les résultats au dataframe choisi par l'utilisateur
#'
#' @param data Un dataframe contenant les données numériques à analyser
#' @return Le dataframe avec la colonne "Cluster" ajoutée
#' @export


# Fonction utilitaire pour sécuriser les entrées utilisateur

xrf_clust  <- function(data = NULL) {

  # === Required packages ===
  required_packages <- c("NbClust", "ggplot2", "writexl", "rstudioapi", "factoextra","clusterCrit","fpc","ggdendro","dendextend")

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

  # --- Internal cache helpers ---
  if (!exists(".my_cache", envir = .GlobalEnv)) {
    assign(".my_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }

  set_cache <- function(name, value) {
    assign(name, value, envir = .my_cache)
    invisible(TRUE)
  }

  get_cache <- function(name) {
    if (exists(name, envir = .my_cache)) get(name, envir = .my_cache)
    else stop("No cached object named '", name, "' found.")
  }

  list_cache <- function() ls(envir = .my_cache)


  # === Select or load data ===
  if ("variables_cluster_cache" %in% list_cache()) {
    data <- get_cache("variables_cluster_cache")
    message("Using cached 'variables_cluster_cache' dataframe.")
  } else {
    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

    if (length(df_list) == 0) stop("No data frames found in the global environment.")

    cat("\nSelect the dataframe with the selected elements:\n")
    print(df_list)
    cat("\n")

    repeat {
      df_name <- readline("Enter the name of the dataframe: ")
      if (df_name %in% df_list) {
        data <- get(df_name, envir = .GlobalEnv)
        set_cache("variables_cluster_cache", data)  # <- ici tu mets bien dans le cache
        break
      }
      cat("Dataframe not found. Try again.\n")
    }
  }



  # === Input validation ===
  if (!is.data.frame(data) && !is.matrix(data)) stop("The input must be a dataframe or matrix.")
  if (!all(sapply(data, is.numeric))) stop("All columns must be numeric.")



  # ===========

  # --- Determine number of clusters (manual or automatic) ---
  cat("\nCluster selection mode:\n")
  cat("\n")
  cat("(1): Automatic detection of optimal number clusters (NbClust)\n")
  cat("\n")
  cat("(2): Manual (user choice)\n\n")

  repeat {
    choice <- safe_readline("Choose mode (1 or 2): ")
    if (choice %in% c("1","2")) break
    cat("Invalid choice. Please enter 1 or 2.\n")
  }

  # --- MANUAL mode ---
  if (choice == "2" || nrow(data) < 20) {

    if (choice == "2") {
      cat("\n=== Manual cluster selection ===\n\n")
    } else {
      cat("\nDataset too small for reliable NbClust results.\n\n")
      cat("Automatic detection of the optimal number of clusters via NbClust is not reliable for such small datasets.\n\n")
      cat("In small datasets, statistical criteria may fail to give meaningful results, because:\n\n")
      cat("- There are too few observations to form well-defined clusters.\n")
      cat("- Indices or gap statistic can be highly unstable.\n")
      cat("- Random variability may dominate over actual structure in the data.\n\n")
      cat("Please use your expert judgment to choose the number of clusters. Consider:\n\n")
      cat("- Domain knowledge about the variables and expected groups.\n")
      cat("- Visual inspection (e.g., PCA projections).\n\n")
    }

    repeat {
      n_clusters <- as.integer(safe_readline("Enter desired number of clusters (integer ≥ 2): "))
      if (!is.na(n_clusters) && n_clusters >= 2) break
      cat("Invalid input. Please enter an integer ≥ 2.\n")
    }
    optimal_clusters_max <- n_clusters

    # --- Choose clustering method ---
    cat("\n--- Clustering method ---\n\n")
    repeat {
      cat("Available clustering methods:\n")
      cat("\n")
      cat("(1): K-means \n")
      cat("\n")
      cat("(2): Hierarchical clustering \n\n")

      method_choice <- as.integer(safe_readline("Choose a method (1 or 2): "))
      if (method_choice %in% c(1,2)) break
      cat("Invalid choice. Please enter 1 or 2.\n\n")
    }

    if (method_choice == 1) {
      set.seed(123)
      clustering_result <- kmeans(data, centers=optimal_clusters_max, nstart=100)
    } else {

      # --- Hierarchical ---
      available_distances <- c("euclidean","maximum","manhattan","canberra","binary","minkowski")
      repeat {
        cat("\nDistances available for hierarchical clustering:\n\n")
        for(i in seq_along(available_distances)) {
          cat("  ", i, "-", available_distances[i], "\n\n")  # double saut de ligne entre chaque option
        }

        distance_choice <- safe_readline("Select a distance (number or name): ")
        distance_num <- as.integer(distance_choice)

        if(!is.na(distance_num) && distance_num>=1 && distance_num<=length(available_distances)){
          chosen_distance <- available_distances[distance_num]
          break
        }
        if(tolower(distance_choice) %in% available_distances){
          chosen_distance <- tolower(distance_choice)
          break
        }
        cat("\nInvalid input. Try again.\n\n")
      }


      hclust_methods <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
      repeat {
        cat("\nAvailable hierarchical methods:\n\n")
        for(i in seq_along(hclust_methods)) {
          cat("  ", i, "-", hclust_methods[i], "\n\n")  # double saut de ligne entre chaque option
        }

        hclust_choice <- as.integer(safe_readline("Choose a hierarchical method (number): "))
        if(!is.na(hclust_choice) && hclust_choice>=1 && hclust_choice<=length(hclust_methods)){
          method_name <- hclust_methods[hclust_choice]
          break
        }
        cat("\nInvalid choice. Try again.\n\n")
      }


      hclust_methods <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
      repeat {
        cat("\nAvailable hierarchical methods:\n\n")
        for(i in seq_along(hclust_methods)) cat("  ", i, "-", hclust_methods[i], "\n")
        cat("\n")  # ligne vide après la liste

        hclust_choice <- as.integer(safe_readline("Choose a hierarchical method (number): "))
        if(!is.na(hclust_choice) && hclust_choice>=1 && hclust_choice<=length(hclust_methods)){
          method_name <- hclust_methods[hclust_choice]
          break
        }
        cat("\nInvalid choice. Try again.\n\n")  # espace avant le message d'erreur
      }


      dist_matrix <- dist(data, method=chosen_distance)
      hc <- hclust(dist_matrix, method=method_name)
      clusters <- cutree(hc, k=optimal_clusters_max)
      clustering_result <- list(cluster=clusters)

      # --- Plot dendrogram ---
      # plot(hc, main = "Dendrogram", xlab = "", sub = "", cex = 0.8)
      # rect.hclust(hc, k = optimal_clusters_max, border = 2:5)

      # Convertir l'objet hclust en dendrogram pour ggplot

      # Palette personnalisée pour le nombre de clusters choisi
      palette_finale <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(optimal_clusters_max)

      # Convertir en dendrogram
      dend <- as.dendrogram(hc)

      # Colorer les branches selon le nombre de clusters et ta palette
      dend <- color_branches(dend, k = optimal_clusters_max, col = palette_finale)

      # Afficher le dendrogramme
      plot(dend, main = "Hierarchical Clustering Dendrogram")
    }

  } else {
    # --- AUTOMATIC mode (NbClust) for datasets >=20 ---
    repeat {
      cat("\n")
      max_clusters <- as.integer(safe_readline("Enter max number of clusters to test (min 2, e.g. 10): "))
      if (!is.na(max_clusters) && max_clusters >= 2) break
      cat("Invalid number. Please enter integer ≥ 2.\n\n")
    }

    # --- Choose clustering method ---
    cat("\n--- Clustering method ---\n\n")
    repeat {
      cat("Available clustering methods:\n")

      cat("\n")
      cat("(1): K-means \n")
      cat("\n")
      cat("(2): Hierarchical clustering \n\n")

      method_choice <- as.integer(safe_readline("Choose a method (1 or 2): "))
      if (method_choice %in% c(1,2)) break
      cat("Invalid choice. Please enter 1 or 2.\n\n")
    }

    if (method_choice == 1) {
      cat("\n====== Determining Optimal Clusters via NbClust (K-means) ======\n\n")
      nb <- NbClust(data, distance="euclidean", min.nc=2, max.nc=max_clusters, method="kmeans")

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

      set.seed(123)
      clustering_result <- kmeans(data, centers=optimal_clusters_max, nstart=100)

    } else {
      # --- Hierarchical with NbClust ---
      available_distances <- c("euclidean","maximum","manhattan","canberra","binary","minkowski")
      repeat {
        cat("\nDistances available for hierarchical clustering:\n\n")
        for(i in seq_along(available_distances)) {
          cat("  ", i, "-", available_distances[i], "\n\n")  # double saut de ligne entre chaque option
        }

        distance_choice <- safe_readline("Select a distance (number or name): ")
        distance_num <- as.integer(distance_choice)

        if(!is.na(distance_num) && distance_num>=1 && distance_num<=length(available_distances)){
          chosen_distance <- available_distances[distance_num]
          break
        }
        if(tolower(distance_choice) %in% available_distances){
          chosen_distance <- tolower(distance_choice)
          break
        }
        cat("\nInvalid input. Try again.\n\n")
      }

      hclust_methods <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
      repeat {
        cat("\nAvailable hierarchical methods:\n\n")
        for(i in seq_along(hclust_methods)) {
          cat("  ", i, "-", hclust_methods[i], "\n\n")  # double saut de ligne entre chaque option
        }

        hclust_choice <- as.integer(safe_readline("Choose a hierarchical method (number): "))
        if(!is.na(hclust_choice) && hclust_choice>=1 && hclust_choice<=length(hclust_methods)){
          method_name <- hclust_methods[hclust_choice]
          break
        }
        cat("\nInvalid choice. Try again.\n\n")
      }


      cat("\n====== Determining Optimal Clusters via NbClust (Hierarchical) ======\n\n")
      nb <- NbClust(data, distance=chosen_distance, min.nc=2, max.nc=max_clusters, method=method_name)

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

      dist_matrix <- dist(data, method=chosen_distance)
      hc <- hclust(dist_matrix, method=method_name)
      clusters <- cutree(hc, k=optimal_clusters_max)
      clustering_result <- list(cluster=clusters)

      # --- Plot dendrogram ---
      # plot(hc, main = "Dendrogram", xlab = "", sub = "", cex = 0.8)
      # rect.hclust(hc, k = optimal_clusters_max, border = 2:5)

      # Convertir l'objet hclust en dendrogram pour ggplot

      # Palette personnalisée pour le nombre de clusters choisi
      palette_finale <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(optimal_clusters_max)

      # Convertir en dendrogram
      dend <- as.dendrogram(hc)

      # Colorer les branches selon le nombre de clusters et ta palette
      dend <- color_branches(dend, k = optimal_clusters_max, col = palette_finale)

      # Afficher le dendrogramme
      plot(dend, main = "Hierarchical Clustering Dendrogram")
    }
  }

  cat("\nNumber of clusters selected:", optimal_clusters_max, "\n\n")





  repeat {

    # --- 1) Utiliser df_denoised_total_cache si disponible ---
    if ("df_denoised_total_cache" %in% list_cache()) {

      df_denoised_total <- get_cache("df_denoised_total_cache")
      df_denoised_total$Cluster <- clustering_result$cluster
      set_cache("df_denoised_total_cache", df_denoised_total)
      assign("Dataframe_Clustering", df_denoised_total, envir = .GlobalEnv)

      cat("\nCluster column added to cached 'df_denoised_total'.\n\n")
      break

    } else if ("df_clean_cache" %in% list_cache()) {

      # --- 2) Sinon, utiliser df_clean_cache ---
      df_denoised_total <- get_cache("df_clean_cache")
      df_denoised_total$Cluster <- clustering_result$cluster
      set_cache("df_clean_cache", df_denoised_total)
      assign("Dataframe_Clustering", df_denoised_total, envir = .GlobalEnv)

      cat("\nCluster column added to cached 'df_clean_cache'.\n\n")
      break

    } else {

      # --- 3) Sinon, menu interactif ---
      df_list <- ls(envir = .GlobalEnv)
      df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

      if (length(df_list) == 0) {
        stop("No data frames found in the global environment.")
      }

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
  }


  # --- Optional save dataframe ---
  save_choice <- tolower(safe_readline("Would you like to save this dataframe? (yes/no): "))
  cat("\n")
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
  cat("\n")
  # --- Optional save cluster drivers analysis ---
  save_drivers <- tolower(safe_readline("Would you like to save the cluster drivers analysis to a text file? (yes/no): "))
  cat("\n")
  if (save_drivers %in% c("yes", "y")) {
    if (!rstudioapi::isAvailable()) {
      cat("Saving requires RStudio.\n\n")
    } else {
      repeat {
        cat("\nSelect the destination file for the cluster analysis...\n")
        path_txt <- rstudioapi::selectFile(
          caption = "Save cluster drivers analysis",
          label = "Save",
          path = getwd(),
          filter = list("Text files" = "txt"),
          existing = FALSE
        )
        if (!nzchar(path_txt)) {
          cat("Saving cancelled.\n");
          break
        }
        if (!grepl("\\.txt$", path_txt, ignore.case = TRUE))
          path_txt <- paste0(path_txt, ".txt")

        # Prepare lines for file
        output_lines <- c(
          "=== Cluster Drivers Analysis ===",
          paste("Optimal number of clusters for your data:", optimal_clusters_max),
          "\n=== ANOVA Results ===",
          capture.output(print(anova_results)),
          "\n=== Cluster Description Profiles ==="
        )

        # Add cluster description profiles
        for (i in 1:nrow(cluster_profiles)) {
          profile_text <- c()
          for (v in vars) {
            val <- cluster_profiles[i, v]
            q <- quantile(cluster_profiles[[v]], probs = c(0.25, 0.5, 0.75))
            lvl <- if(val >= q[3]) "very high" else if(val >= q[2]) "high" else if(val >= q[1]) "medium" else "low"
            profile_text <- c(profile_text, paste(v, lvl, sep=": "))
          }
          output_lines <- c(output_lines, paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse=", ")))
        }

        # Write to file
        writeLines(output_lines, con = path_txt)
        cat("\nCluster drivers analysis saved to:", path_txt, "\n\n")
        break
      }
    }
  }


  # --- Optional PCA visualization ---
  cat("\n=== PCA and Cluster Visualization ===\n\n")
  show_acp <- tolower(safe_readline("Would you like to visualize a PCA with the clusters? (yes/no): "))
  cat("\n")
  if (show_acp %in% c("yes", "y")) {



    # Sélection du dataframe pour df_cluster
    if ("df_denoised_total_cache" %in% list_cache()) {

      df_cluster <- get_cache("df_denoised_total_cache")

    } else if ("df_clean_cache" %in% list_cache()) {

      df_cluster <- get_cache("df_clean_cache")
      cat("The dataframe 'df_denoised_total_cache' was not found. Using 'df_clean_cache' instead.\n")

    } else {

      cat("Neither 'df_denoised_total_cache' nor 'df_clean_cache' were found.\n")
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

    cat("\n")

    use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): "))
    if (use_custom %in% c("yes", "y")) {
      cat("Enter", n_clusters, "colors (by name or hex):\n")
      user_colors <- character(n_clusters)
      for (i in seq_len(n_clusters)) user_colors[i] <- safe_readline(paste("Color for cluster", i, ": "))
      palette_finale <- user_colors
    } else {
      palette_finale <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(n_clusters)
    }
    cat("\n")
    cat("Palette used:", paste(palette_finale, collapse = ", "), "\n\n")

    eig <- (res.pca$sdev)^2 / sum((res.pca$sdev)^2) * 100

    # Création du graphique avec fviz_pca_biplot
    p <- fviz_pca_biplot(
      res.pca,
      geom.ind = "point", pointshape = 16, pointsize = 2.25,
      alpha.ind = 0.9, col.ind = df_cluster$Cluster, palette = palette_finale,
      addEllipses = TRUE, ellipse.level = 0.95, mean.point = FALSE,
      label = "var", col.var = "gray35", arrowsize = 0.5, repel = TRUE,
      legend.title = "Cluster"
    ) +
      ggtitle("Cluster structure across PCA projection") +
      labs(
        x = paste0("Principal Component 1 (", round(eig[1], 1), "%)"),
        y = paste0("Principal Component 2 (", round(eig[2], 1), "%)"),
        caption = "Ellipses represent 95% confidence regions"
      ) +
      theme_minimal(base_family = "sans", base_size = 14) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#222222"),
        legend.title = element_text(size = 13, face = "bold", color = "#222222"),
        legend.text = element_text(size = 11, color = "#444444"),
        axis.title = element_text(size = 14, face = "bold", color = "#222222"),
        axis.text = element_text(size = 12, color = "#333333"),
        panel.grid.major = element_line(color = "gray85", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#fcfcfc", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        plot.caption = element_text(size = 10, hjust = 1, color = "#555555")
      ) +
      guides(
        color = guide_legend(
          override.aes = list(size = 4, alpha = 1),
          title.position = "top",
          title.hjust = 0.5
        )
      )


    print(p)

    cat("\n")

    save_pdf <- tolower(safe_readline("Would you like to export the PCA plot as PDF? (yes/no): "))

    cat("\n")

    if (save_pdf %in% c("yes", "y")) {
      if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
      library(rstudioapi)

      if (!rstudioapi::isAvailable()) {
        cat("RStudio API not available. Cannot select file interactively.\n")
      } else {
        pdf_path <- rstudioapi::selectFile(
          caption = "Save PCA visualization",
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
          cat("\n")
          pdf_height <- as.numeric(safe_readline("Enter desired PDF height (in inches, e.g., 8): "))

          # Crée le PDF avec les dimensions choisies
          pdf(pdf_path, width = pdf_width, height = pdf_height)
          print(p)  # <- exporte directement le plot PCA
          dev.off()

          cat("\n")

          cat("\nPCA visualization saved to:", pdf_path, "\n")
        } else {
          cat("Saving cancelled.\n")
        }
      }
    } else {
      cat("PDF export skipped.\n")
    }
  }

  # --- Optional geochemical visualization ---
  cat("\n")
  show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))
  cat("\n")



  if (show_geo %in% c("yes", "y", "oui", "o")) {

    if (exists("visual.clust")) {
      repeat {

        # --- 1) Utiliser df_denoised_total_cache si présent ---
        if ("df_denoised_total_cache" %in% list_cache()) {

          df_denoised_total <- get_cache("df_denoised_total_cache")
          message("Using cached 'df_denoised_total_cache' dataframe.")

          if ("Cluster" %in% colnames(df_denoised_total)) {
            visual.clust(df_denoised_total)
            break
          } else {
            cat("Cached dataframe found but no 'Cluster' column detected.\n")
          }

          # --- 2) Sinon utiliser df_clean_cache si présent ---
        } else if ("df_clean_cache" %in% list_cache()) {

          df_denoised_total <- get_cache("df_clean_cache")
          message("Using cached 'df_clean_cache' dataframe (df_denoised_total_cache not found).")

          if ("Cluster" %in% colnames(df_denoised_total)) {
            visual.clust(df_denoised_total)
            break
          } else {
            cat("'df_clean_cache' found but no 'Cluster' column detected.\n")
          }

          # --- 3) Sinon rien trouvé, passer au choix utilisateur ---
        } else {

          cat("No cached dataframe found under 'df_denoised_total_cache' or 'df_clean_cache'.\n")

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

        }

        cat("\nTry again.\n")
      }

    } else {
      cat("Function 'visual.clust()' not found. Please load it before running this option.\n")
    }
  }


  # --- Cluster quality and stability ---
  cat("\n=== Cluster Quality and Stability Evaluation ===\n\n")
  eval_choice <- tolower(safe_readline("Evaluate cluster stability and quality? (yes/no): "))

  if(eval_choice %in% c("yes","y")){

    # Silhouette
    sil <- cluster::silhouette(clustering_result$cluster, dist(data))
    sil_mean <- mean(sil[,3])

    # Davies-Bouldin
    db_index <- clusterCrit::intCriteria(as.matrix(data), clustering_result$cluster, "Davies_Bouldin")
    dbi <- db_index$davies_bouldin

    # Redirige la sortie temporairement pour ne rien afficher
    temp <- tempfile()
    sink(temp)
    boot <- fpc::clusterboot(data, B = 50, clustermethod = fpc::kmeansCBI,
                             k = optimal_clusters_max, seed = 123, showplots = FALSE)
    sink()  # revient à la console
    stab_mean <- mean(boot$bootmean)

    # --- Interpretations ---
    sil_text <- if(sil_mean > 0.75) "Excellent: clusters well separated"
    else if(sil_mean > 0.6) "Good: mostly well assigned"
    else if(sil_mean > 0.4) "Moderate: some points misassigned"
    else if(sil_mean > 0.2) "Weak: clusters poorly defined"
    else "Very weak: mostly overlapping"

    db_text <- if(dbi < 0.5) "Excellent separation"
    else if(dbi < 0.75) "Good separation"
    else if(dbi < 1.0) "Moderate separation"
    else "Weak separation, overlapping"

    boot_text <- if(stab_mean > 0.8) "Very stable clusters"
    else if(stab_mean > 0.6) "Stable clusters"
    else if(stab_mean > 0.4) "Moderately stable"
    else "Unstable clusters, use caution"

    overall_text <- if(sil_mean>0.6 & dbi<0.75 & stab_mean>0.65) {
      "Clustering quality is good, clusters are well-separated and stable."
    } else if(sil_mean>0.4 & dbi<1 & stab_mean>0.4) {
      "Clustering moderate → Some clusters may be ambiguous."
    } else {
      "Clustering is weak → Clusters poorly separated or unstable."
    }

    cat("\n")
    # --- Summary lines with explanations ---
    summary_lines <- c(
      "====== Clustering Analysis (K-means) ======",
      "",
      sprintf("Silhouette mean                : %.3f", sil_mean),
      sprintf("Davies-Bouldin index           : %.3f", dbi),
      sprintf("Bootstrap stability (mean)     : %.3f", stab_mean),
      "",
      "------------------------------------------------",
      "",
      "Interpretation per metric:",
      "",
      sprintf("- Silhouette       : %.3f → %s", sil_mean, sil_text),
      "",
      "    → The silhouette measures cohesion and separation.",
      "      Values near 1 indicate distinct, well-separated clusters.",
      "      Values around 0 suggest overlap; negative values mean misclassification.",
      "",
      sprintf("- Davies-Bouldin   : %.3f → %s", dbi, db_text),
      "",
      "    → The Davies–Bouldin index measures how similar clusters are.",
      "      Lower is better: values < 0.5 indicate strong separation.",
      "      Values > 1 imply overlapping clusters.",
      "",
      sprintf("- Bootstrap stab.  : %.3f → %s", stab_mean, boot_text),
      "",
      "    → The bootstrap stability measures how reproducible clusters are.",
      "      High values (>0.8) mean clusters remain stable across resampling.",
      "      Low values (<0.4) mean clusters are unstable and likely artefacts.",
      "",
      "Overall evaluation:",
      "",
      overall_text,
      "",
      "------------------------------------------------"
    )


    cat(paste(summary_lines, collapse="\n"), "\n")

    cat("\n")

    # Optional export
    save_eval <- tolower(safe_readline("Save evaluation summary to text file? (yes/no): "))
    if(save_eval %in% c("yes","y")){
      if(requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()){
        eval_path <- rstudioapi::selectFile(
          caption="Save clustering evaluation summary",
          label="Save",
          path=getwd(),
          filter=list("Text files"="txt"),
          existing=FALSE
        )
        if(nzchar(eval_path)){
          if(!grepl("\\.txt$", eval_path, ignore.case=TRUE)) eval_path <- paste0(eval_path,".txt")
          writeLines(summary_lines, eval_path)
          cat("\n")
          cat("Evaluation summary saved to:", eval_path, "\n")
        } else cat("Saving cancelled.\n")
      } else cat("RStudio API not available; cannot select file interactively.\n")
    }
  }
  cat("\n")
  # --- End of main loop ---
  cat("Clustering finished.\n")

  cat("\n")


}

