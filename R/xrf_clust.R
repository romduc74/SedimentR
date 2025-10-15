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



    # # --- Optional geochemical visualization ---
    # cat("\n")
    # show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))
    # if (show_geo %in% c("yes", "y", "oui", "o")) {
    #   if (exists("visual.clust")) {
    #     repeat {
    #       if ("df_denoised_total_cache" %in% list_cache()) {
    #         df_denoised_total <- get_cache("df_denoised_total_cache")
    #         if ("Cluster" %in% colnames(df_denoised_total)) {
    #           visual.clust(df_denoised_total)
    #           break
    #         } } else {
    #           df_visu_name <- safe_readline("Enter dataframe name containing 'Cluster': ")
    #           if (exists(df_visu_name, envir = .GlobalEnv)) {
    #             df_visu <- get(df_visu_name, envir = .GlobalEnv)
    #             if ("Cluster" %in% colnames(df_visu)) {
    #               visual.clust(df_visu)
    #               break
    #             } else cat("Selected dataframe does not contain 'Cluster' column.\n")
    #           } else cat("Dataframe not found.\n")
    #         }
    #       else {
    #         cat("'df_denoised_total' not found in global environment.\n")
    #         break
    #       }
    #     }
    #   } else {
    #     cat("Function 'visual.clust()' not found.\n")
    #   }
    # }


    # --- Restart clustering? ---
    restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
    if (tolower(restart) %in% c("no", "n")) {
      cat("Clustering finished.\n")
      break
    }
  }
}

#
# xrf_clust <- function(data=NULL) {
#
#   # Packages nécessaires
#   required_packages <- c("NbClust", "ggplot2", "writexl", "rstudioapi", "factoextra")
#
#   repeat {
#     # Check if df_normal is already in cache
#     if ("variables_cluster_cache" %in% list_cache()) {
#       data <- get_cache("variables_cluster_cache")
#       message("Using cached 'variables_cluster_cache' dataframe.")
#       break
#     }
#
#     # Otherwise, show data frames available in the global environment
#     df_list <- ls(envir = .GlobalEnv)
#     df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
#
#     if (length(df_list) == 0) {
#       stop("No data frames found in the global environment.")
#     }
#
#     cat("\nSelect the dataframe with the selected elements:\n")
#     print(df_list)
#     cat("\n")
#
#     df_name <- readline("Enter the name of the dataframe: ")
#
#     # If a valid dataframe name is entered
#     if (df_name %in% df_list) {
#       data <- get(df_name, envir = .GlobalEnv)
#       set_cache("variables_cluster_cache", data)
#       #message("Dataframe cached as 'df_normal' for future use.")
#       break
#     }
#
#     # Otherwise, try again
#     cat("\n Dataframe not found. Try again.\n")
#   }
#
#
#
#   for (pkg in required_packages) {
#     if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
#     library(pkg, character.only = TRUE)
#   }
#
#   # Vérifications
#   if (!is.data.frame(data) && !is.matrix(data)) stop("The entry must be a dataframe or matrix.")
#   if (!all(sapply(data, is.numeric))) stop("All columns must be numeric.")
#
#   safe_readline <- function(prompt_msg) {
#     input <- readline(prompt = prompt_msg)
#     if (tolower(input) == "exit") stop("User interrupted execution via exit.")
#     return(input)
#   }
#
#   repeat {
#     set.seed(123)
#     cat("\n====== Clustering Analysis (K-means method) ======\n\n")
#
#     # Nombre max clusters
#     repeat {
#       max_clusters <- as.integer(safe_readline("Enter max number of clusters to test (min 2, e.g. 10): "))
#       if (!is.na(max_clusters) && max_clusters>=2) break
#       cat("Invalid number. Please enter integer ≥2.\n\n")
#     }
#
#     # Déterminer nombre optimal de clusters
#     cat("\n====== Determining Optimal Clusters ======\n\n")
#     nb <- NbClust(data, distance="euclidean", min.nc=2, max.nc=max_clusters, method="kmeans")
#     results <- nb$Best.nc[1,]
#     freq_table <- sort(table(results), decreasing=TRUE)
#     max_freq <- max(freq_table)
#     clusters_with_max_freq <- as.numeric(names(freq_table[freq_table==max_freq]))
#
#     if(length(clusters_with_max_freq)==1) optimal_clusters_max <- clusters_with_max_freq
#     else {
#       repeat {
#         cat("Multiple optimal cluster numbers detected:", paste(clusters_with_max_freq, collapse=", "),"\n")
#         user_input <- as.integer(safe_readline("Choose the number of clusters from these options: "))
#         if(user_input %in% clusters_with_max_freq) {
#           optimal_clusters_max <- user_input
#           break
#         }
#         cat("Invalid choice. Try again.\n\n")
#       }
#     }
#
#     cat("\nOptimal number of clusters selected:", optimal_clusters_max,"\n\n")
#
#     # Appliquer k-means
#     clustering_result <- kmeans(data, centers=optimal_clusters_max, nstart=100)
#
#     repeat {
#       # Check if df_normal is already in cache
#       if ("df_denoised_total_cache" %in% list_cache()) {
#         df_denoised_total <- get_cache("df_denoised_total_cache")
#         df_denoised_total$Cluster <- clustering_result$cluster
#         set_cache("df_denoised_total_cache",df_denoised_total)
#         cat("\nCluster column added to df_denoised_total \n\n")
#         break
#       }
#
#       # Otherwise, show data frames available in the global environment
#
#       df_list <- ls(envir = .GlobalEnv)
#       df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
#
#       if (length(df_list) == 0) {
#         stop("No data frames found in the global environment.")
#       }
#
#       cat("\n")
#       cat("=== Select the dataframe to which the 'Cluster' column will be added  ===\n")
#       cat("\n")
#
#       repeat {
#         cat("Dataframes available:\n")
#         print(df_list)
#         cat("\n")
#         df_name <- safe_readline("Enter the name of the target dataframe: ")
#         if(df_name %in% df_list){
#           df_denoised_total <- get(df_name)
#           df_denoised_total$Cluster <- clustering_result$cluster
#           assign(df_name, df_denoised_total, envir=.GlobalEnv)
#           cat("\nCluster column added to", df_name,"\n\n")
#           break
#         }
#         cat("Dataframe not found. Try again.\n\n")
#       }
#     }
#
#
#
#     # # Sélection dataframe cible
#     # cat("\n")
#     # cat("=== Select the dataframe to which the 'Cluster' column will be added  ===\n")
#     # cat("\n")
#     #
#     # df_list <- ls(envir=.GlobalEnv)
#     # df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x)))]
#     #
#     # repeat {
#     #   cat("Dataframes available:\n")
#     #   print(df_list)
#     #   cat("\n")
#     #   df_name <- safe_readline("Enter the name of the target dataframe: ")
#     #   if(df_name %in% df_list){
#     #     selected_df <- get(df_name)
#     #     selected_df$Cluster <- clustering_result$cluster
#     #     assign(df_name, selected_df, envir=.GlobalEnv)
#     #     cat("\nCluster column added to", df_name,"\n\n")
#     #     break
#     #   }
#     #   cat("Dataframe not found. Try again.\n\n")
#     # }
#
#
#
#
#
#
#     # Export dataframe final
#     save_choice <- tolower(safe_readline("Would you like to save this dataframe? (yes/no): "))
#     if(save_choice %in% c("yes","y")){
#       repeat {
#         ext_choice <- tolower(safe_readline("Desired extension: csv or xlsx: "))
#         if(!ext_choice %in% c("csv","xlsx")) { cat("Invalid extension.\n"); next }
#         if(!rstudioapi::isAvailable()){ cat("Saving requires RStudio.\n"); break }
#         cat("\nSelect the destination file...\n")
#         chemin_save <- rstudioapi::selectFile(
#           caption="Save the file",
#           label="Save",
#           path=getwd(),
#           filter=if(ext_choice=="csv") list("CSV files"="csv") else list("Excel files"="xlsx"),
#           existing=FALSE
#         )
#         if(!nzchar(chemin_save)){ cat("Saving cancelled.\n"); break }
#         if(!grepl(paste0("\\.",ext_choice,"$"), chemin_save, ignore.case=TRUE)) chemin_save <- paste0(chemin_save,".",ext_choice)
#         if(ext_choice=="csv") write.csv(df_denoised_total, chemin_save, row.names=FALSE)
#         else writexl::write_xlsx(df_denoised_total, path=chemin_save)
#         cat("\nSaved file:", chemin_save,"\n\n")
#         break
#       }
#     }
#
#     cat("\n")
#
#
#     # ---------------- Cluster Drivers Analysis ----------------
#     cat("=== Cluster Drivers Analysis  ===\n")
#
#     cat("\n=== Optimal number of clusters for your data:", optimal_clusters_max, " ===\n")
#     cat("\n=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===\n\n")
#
#     cluster_profiles <- aggregate(data, by = list(Cluster = clustering_result$cluster), mean)
#     cluster_profiles_rounded <- cluster_profiles
#     cluster_profiles_rounded[,-1] <- round(cluster_profiles[,-1], 0)
#
#     anova_results <- data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
#
#     for (col in colnames(data)) {
#       model <- aov(data[[col]] ~ as.factor(clustering_result$cluster))
#       s <- summary(model)[[1]]
#       F_val <- s$`F value`[1]
#       p_val <- s$`Pr(>F)`[1]
#       anova_results <- rbind(anova_results, data.frame(Variable = col, F_value = F_val, p_value = p_val))
#     }
#
#     anova_results <- anova_results[order(-anova_results$F_value), ]
#     anova_results$F_value <- round(anova_results$F_value, 1)
#     anova_results$p_value <- signif(anova_results$p_value, 3)
#     print(anova_results)
#     cat("\n")
#
#     vars <- colnames(cluster_profiles)[-1]
#     cat("=== Cluster Description Profiles ===\n\n")
#
#     for (i in 1:nrow(cluster_profiles)) {
#       profile_text <- c()
#       for (v in vars) {
#         val <- cluster_profiles[i, v]
#         q <- quantile(cluster_profiles[[v]], probs = c(0.25, 0.5, 0.75))
#         if (val >= q[3]) lvl <- "very high"
#         else if (val >= q[2]) lvl <- "high"
#         else if (val >= q[1]) lvl <- "medium"
#         else lvl <- "low"
#         profile_text <- c(profile_text, paste(v, lvl, sep = ": "))
#       }
#       cat(paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse = ", "), "\n"))
#     }
#
#     cat("\n")
#
#     # --- Optional export of Cluster Drivers Analysis ---
#     save_drivers <- tolower(safe_readline("Would you like to save the cluster drivers analysis to a text file? (yes/no): "))
#     if (save_drivers %in% c("yes", "y")) {
#       if (!rstudioapi::isAvailable()) {
#         cat("Saving requires RStudio.\n\n")
#       } else {
#         file_path <- rstudioapi::selectFile(
#           caption = "Save cluster drivers analysis",
#           label = "Save",
#           path = getwd(),
#           filter = list("Text files" = "txt"),
#           existing = FALSE
#         )
#         if (nzchar(file_path)) {
#           if (!grepl("\\.txt$", file_path, ignore.case = TRUE)) file_path <- paste0(file_path, ".txt")
#           analysis_lines <- c(
#             "\n=== Cluster Drivers Analysis ===\n",
#             "Optimal number of clusters: ", optimal_clusters_max, "\n",
#             "=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===\n",
#             capture.output(print(anova_results)),
#             "\n=== Cluster Description Profiles ===\n"
#           )
#           for (i in 1:nrow(cluster_profiles)) {
#             profile_text <- c()
#             for (v in vars) {
#               val <- cluster_profiles[i, v]
#               q <- quantile(cluster_profiles[[v]], probs = c(0.25, 0.5, 0.75))
#               if (val >= q[3]) lvl <- "very high"
#               else if (val >= q[2]) lvl <- "high"
#               else if (val >= q[1]) lvl <- "medium"
#               else lvl <- "low"
#               profile_text <- c(profile_text, paste(v, lvl, sep = ": "))
#             }
#             analysis_lines <- c(
#               analysis_lines,
#               paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse = ", "))
#             )
#           }
#           writeLines(analysis_lines, con = file_path)
#           cat("\nCluster drivers analysis saved to:", file_path, "\n\n")
#         } else cat("Saving cancelled.\n\n")
#       }
#     }
#
#     cat("\n=== PCA and clusters visualization ===\n\n")
#
#     # --- PCA visualization ---
#     show_acp <- tolower(safe_readline("Would you like to visualize a PCA with the clusters? (yes/no): "))
#     cat("\n")
#
#     if (show_acp %in% c("yes", "y")) {
#
#       # --- Cluster data selection ---
#       if (exists("df_denoised_total", envir = .GlobalEnv)) {
#         df_cluster <- get("df_denoised_total", envir = .GlobalEnv)
#       } else {
#         cat("The dataframe 'df_denoised_total' was not found.\n")
#         return(invisible(NULL))
#       }
#
#       # --- PCA variable data selection (cached or prompt) ---
#       repeat {
#         if ("df_normalized_cache" %in% list_cache()) {
#           df_vars <- get_cache("df_normalized_cache")
#           message("Using cached 'df_normalized_cache' dataframe.")
#           break
#         }
#
#         df_vars_name <- safe_readline("Enter the name of the dataframe which was used for PCA (in prop.select), or 'q' to cancel: ")
#         if (tolower(df_vars_name) == "q" || df_vars_name == "") {
#           cat("Cancelled PCA visualization.\n")
#           return(invisible(NULL))
#         }
#
#         if (exists(df_vars_name, envir = .GlobalEnv)) {
#           df_vars <- get(df_vars_name, envir = .GlobalEnv)
#           if (all(sapply(df_vars, is.numeric))) break
#           cat("The selected dataframe must contain only numeric columns. Try again.\n")
#         } else cat("Dataframe not found. Try again.\n")
#       }
#
#       # --- Prepare cluster column ---
#       df_cluster$Cluster <- as.factor(df_cluster$Cluster)
#
#       # --- Compute PCA ---
#       res.pca <- prcomp(df_vars, scale. = FALSE)
#
#       # --- Choose color palette ---
#       n_clusters <- length(levels(df_cluster$Cluster))
#       use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): "))
#       cat("\n")
#
#       if (use_custom %in% c("yes", "y", "oui", "o")) {
#         cat("Enter", n_clusters, "colors (by name or hex, e.g., 'red' or '#FF0000'):\n")
#         user_colors <- character(n_clusters)
#         for (i in seq_len(n_clusters)) {
#           user_colors[i] <- safe_readline(paste("Color for cluster", i, ": "))
#         }
#         palette_finale <- user_colors
#       } else {
#         palette_finale <- colorRampPalette(c("#662483", "#f39200", "#f9b233", "#ffda77", "#35163b"))(n_clusters)
#       }
#
#       cat("Palette used:", paste(palette_finale, collapse = ", "), "\n\n")
#
#       # --- PCA plot ---
#       p <- fviz_pca_biplot(
#         res.pca,
#         geom.ind = "point",
#         pointshape = 16,
#         pointsize = 2.25,
#         alpha.ind = 0.9,
#         col.ind = df_cluster$Cluster,
#         palette = palette_finale,
#         addEllipses = TRUE,
#         ellipse.level = 0.95,
#         mean.point = FALSE,
#         label = "var",
#         col.var = "gray35",
#         arrowsize = 0.5,
#         repel = TRUE,
#         legend.title = "Cluster"
#       ) +
#         ggtitle("Cluster structure across PCA projection") +
#         labs(
#           x = "Principal Component 1",
#           y = "Principal Component 2",
#           caption = "Ellipses represent 95% confidence regions"
#         ) +
#         theme_minimal(base_family = "sans", base_size = 14) +
#         theme(
#           plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#222222"),
#           legend.title = element_text(size = 13, face = "bold", color = "#222222"),
#           legend.text = element_text(size = 11, color = "#444444"),
#           axis.title = element_text(size = 14, face = "bold", color = "#222222"),
#           axis.text = element_text(size = 12, color = "#333333"),
#           panel.grid.major = element_line(color = "gray85", linewidth = 0.4),
#           panel.grid.minor = element_blank(),
#           plot.background = element_rect(fill = "#fcfcfc", color = NA),
#           panel.background = element_rect(fill = "white", color = NA),
#           legend.background = element_rect(fill = "white", color = NA),
#           legend.key = element_rect(fill = "white", color = NA),
#           plot.caption = element_text(size = 10, hjust = 1, color = "#555555")
#         ) +
#         guides(color = guide_legend(
#           override.aes = list(size = 4, alpha = 1),
#           title.position = "top",
#           title.hjust = 0.5
#         ))
#
#       print(p)
#
#       # --- Export PDF option ---
#       cat("\n")
#       save_pdf <- tolower(safe_readline("Would you like to export the PCA plot as PDF? (yes/no): "))
#       cat("\n")
#       if (save_pdf %in% c("yes", "y")) {
#         if (!rstudioapi::isAvailable()) {
#           cat("Saving requires RStudio.\n")
#         } else {
#           pdf_path <- rstudioapi::selectFile(
#             caption = "Save PCA plot",
#             label = "Save",
#             path = getwd(),
#             filter = list("PDF files" = "pdf"),
#             existing = FALSE
#           )
#           if (nzchar(pdf_path)) {
#             if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE)) pdf_path <- paste0(pdf_path, ".pdf")
#
#             pdf_width <- as.numeric(safe_readline("Enter desired PDF width (in inches, e.g., 10): "))
#             pdf_height <- as.numeric(safe_readline("Enter desired PDF height (in inches, e.g., 8): "))
#
#             pdf(pdf_path, width = pdf_width, height = pdf_height)
#             print(p)
#             dev.off()
#             cat("PCA plot saved to:", pdf_path, "\n")
#           } else cat("Saving cancelled.\n")
#         }
#       }
#     }  # <-- closes the PCA visualization block
#
#     # === Visualisation géochimique optionnelle ===
#     cat("\n")
#     show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))
#     cat("\n")
#
#     if (show_geo %in% c("yes", "y", "oui", "o")) {
#       if (exists("visual.clust")) {
#         repeat {
#           if (exists("df_denoised_total", envir = .GlobalEnv)) {
#             if ("Cluster" %in% colnames(df_denoised_total)) {
#               visual.clust(df_denoised_total)
#               break
#             } else {
#               df_visu_name <- safe_readline("Enter the name of the dataframe containing 'Cluster': ")
#               if (exists(df_visu_name, envir = .GlobalEnv)) {
#                 df_visu <- get(df_visu_name, envir = .GlobalEnv)
#                 if ("Cluster" %in% colnames(df_visu)) {
#                   visual.clust(df_visu)
#                   break
#                 } else {
#                   cat("The selected dataframe does not contain a 'Cluster' column. Try again.\n")
#                 }
#               } else {
#                 cat("Dataframe not found. Try again.\n")
#               }
#             }
#           } else {
#             cat("'df_denoised_total' not found in global environment.\n")
#             break
#           }
#         }
#       } else {
#         cat("The function 'visual.clust()' is not defined in the environment.\n")
#         cat("Please make sure it is loaded before running this option.\n")
#       }
#     }
#
#     # === Restart clustering ===
#     cat("\n")
#     restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
#     cat("\n")
#
#     if (tolower(restart) %in% c("no", "n")) {
#       cat("Clustering finished.\n")
#       break
#     }
# }
# }
#
#
#
#     # # ---------------- Cluster Drivers Analysis ----------------
#     # cat("=== Cluster Drivers Analysis  ===\n")
#     #
#     # cat("\n=== Optimal number of clusters for your data:", optimal_clusters_max," ===\n")
#     # cat("\n=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===\n")
#     # cat("\n")
#     #
#     # cluster_profiles <- aggregate(data, by=list(Cluster=clustering_result$cluster), mean)
#     # cluster_profiles_rounded <- cluster_profiles
#     # cluster_profiles_rounded[,-1] <- round(cluster_profiles[,-1],0)
#     #
#     # anova_results <- data.frame(Variable=character(), F_value=numeric(), p_value=numeric(), stringsAsFactors=FALSE)
#     # for(col in colnames(data)){
#     #   model <- aov(data[[col]] ~ as.factor(clustering_result$cluster))
#     #   s <- summary(model)[[1]]
#     #   F_val <- s$`F value`[1]
#     #   p_val <- s$`Pr(>F)`[1]
#     #   anova_results <- rbind(anova_results,data.frame(Variable=col,F_value=F_val,p_value=p_val))
#     # }
#     # anova_results <- anova_results[order(-anova_results$F_value),]
#     # anova_results$F_value <- round(anova_results$F_value,1)
#     # anova_results$p_value <- signif(anova_results$p_value,3)
#     # print(anova_results)
#     # cat("\n")
#     #
#     # vars <- colnames(cluster_profiles)[-1]
#     # cat("=== Cluster Description Profiles ===\n\n")
#     # for(i in 1:nrow(cluster_profiles)){
#     #   profile_text <- c()
#     #   for(v in vars){
#     #     val <- cluster_profiles[i, v]
#     #     q <- quantile(cluster_profiles[[v]], probs=c(0.25,0.5,0.75))
#     #     if(val >= q[3]) lvl <- "very high"
#     #     else if(val >= q[2]) lvl <- "high"
#     #     else if(val >= q[1]) lvl <- "medium"
#     #     else lvl <- "low"
#     #     profile_text <- c(profile_text, paste(v, lvl, sep=": "))
#     #   }
#     #   cat(paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse=", "), "\n"))
#     # }
#     # cat("\n")
#     #
#     # # Export optionnel Cluster Drivers Analysis
#     # save_drivers <- tolower(safe_readline("Would you like to save the cluster drivers analysis to a text file? (yes/no): "))
#     # if(save_drivers %in% c("yes","y")){
#     #   if(!rstudioapi::isAvailable()) cat("Saving requires RStudio.\n\n")
#     #   else {
#     #     file_path <- rstudioapi::selectFile(
#     #       caption="Save cluster drivers analysis",
#     #       label="Save",
#     #       path=getwd(),
#     #       filter=list("Text files"="txt"),
#     #       existing=FALSE
#     #     )
#     #     if(nzchar(file_path)){
#     #       if(!grepl("\\.txt$", file_path, ignore.case=TRUE)) file_path <- paste0(file_path,".txt")
#     #       analysis_lines <- c(
#     #         "\n=== Cluster Drivers Analysis ===\n",
#     #         "Optimal number of clusters:", optimal_clusters_max, "\n",
#     #         "=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===\n",
#     #         capture.output(print(anova_results)),
#     #         "\n=== Cluster Description Profiles ===\n"
#     #       )
#     #       for(i in 1:nrow(cluster_profiles)){
#     #         profile_text <- c()
#     #         for(v in vars){
#     #           val <- cluster_profiles[i, v]
#     #           q <- quantile(cluster_profiles[[v]], probs=c(0.25,0.5,0.75))
#     #           if(val >= q[3]) lvl <- "very high"
#     #           else if(val >= q[2]) lvl <- "high"
#     #           else if(val >= q[1]) lvl <- "medium"
#     #           else lvl <- "low"
#     #           profile_text <- c(profile_text, paste(v, lvl, sep=": "))
#     #         }
#     #         analysis_lines <- c(analysis_lines, paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse=", ")))
#     #       }
#     #       writeLines(analysis_lines, con=file_path)
#     #       cat("\nCluster drivers analysis saved to:", file_path,"\n\n")
#     #     } else cat("Saving cancelled.\n\n")
#     #   }
#     # }
#     #
#     # cat("\n")
#     #
#     # cat("=== PCA and clusters visualization ===\n")
#     #
#     # cat("\n")
#     #
#     #
#     # # --- ACP avec choix de palette ---
#     # show_acp <- tolower(safe_readline("Would you like to visualize a PCA with the clusters? (yes/no): "))
#     # cat("\n")
#     #
#     # if(show_acp %in% c("yes","y")){
#     #   # Sélection dataframe clusters
#     #   df_cluster<-df_denoised_total
#     # }
#     #
#     #   #
#     #   # repeat {
#     #   #   df_cluster_name <- safe_readline("Enter the name of the dataframe containing the 'Cluster' column: ")
#     #   #   if(exists(df_cluster_name, envir = .GlobalEnv)){
#     #   #     df_cluster <- get(df_cluster_name, envir = .GlobalEnv)
#     #   #     if("Cluster" %in% colnames(df_cluster)) break
#     #   #     cat("The selected dataframe does not contain a 'Cluster' column. Try again.\n")
#     #   #   } else cat("Dataframe not found. Try again.\n")
#     #   # }
#     #
#     #   cat("\n")
#     #
#     #
#     #
#     #
#     #
#     #
#     #
#     #
#     #
#     #   repeat {
#     #     # Check if df_normal is already in cache
#     #     if ("df_normalized_cache" %in% list_cache()) {
#     #       df_vars <- get_cache("df_normalized_cache")
#     #       message("Using cached 'df_normalized_cache' dataframe.")
#     #       break
#     #     }
#     #
#     #     df_vars_name <- safe_readline("Enter the name of the dataframe wich was used for PCA (in prop.select): ")
#     #     if(exists(df_vars_name, envir = .GlobalEnv)){
#     #       df_vars <- get(df_vars_name, envir = .GlobalEnv)
#     #       if(all(sapply(df_vars, is.numeric))) break
#     #       cat("The selected dataframe must contain only numeric columns. Try again.\n")
#     #     } else cat("Dataframe not found. Try again.\n")
#     #
#     #   }
#     #
#     #
#     #
#     #
#     #
#     #
#     #   # # Sélection dataframe variables
#     #   # repeat {
#     #   #   df_vars_name <- safe_readline("Enter the name of the dataframe wich was used for PCA (in prop.select): ")
#     #   #   if(exists(df_vars_name, envir = .GlobalEnv)){
#     #   #     df_vars <- get(df_vars_name, envir = .GlobalEnv)
#     #   #     if(all(sapply(df_vars, is.numeric))) break
#     #   #     cat("The selected dataframe must contain only numeric columns. Try again.\n")
#     #   #   } else cat("Dataframe not found. Try again.\n")
#     #   # }
#     #
#     #   # Convertir Cluster en facteur
#     #   df_cluster$Cluster <- as.factor(df_cluster$Cluster)
#     #
#     #   # Calcul PCA
#     #   res.pca <- prcomp(df_vars, scale. = FALSE)
#     #
#     #   # Nombre de clusters pour la palette
#     #   n_clusters <- length(levels(df_cluster$Cluster))
#     #
#     #   cat("\n")
#     #
#     #   # Choix de palette
#     #   use_custom <- tolower(safe_readline("Would you like to set a custom palette? (yes/no): "))
#     #   cat("\n")
#     #   if(use_custom %in% c("oui","o","yes","y")){
#     #     cat("Enter", n_clusters, "colors (by name or hex, e.g., 'red' or '#FF0000'):\n")
#     #     user_colors <- character(n_clusters)
#     #     for(i in seq_len(n_clusters)){
#     #       user_colors[i] <- safe_readline(paste("Color for cluster", i, ": "))
#     #     }
#     #     palette_finale <- user_colors
#     #   } else {
#     #     palette_finale <- colorRampPalette(c("#662483","#f39200","#f9b233","#ffda77","#35163b"))(n_clusters)
#     #   }
#     #   cat("\n")
#     #
#     #   # Afficher la palette utilisée
#     #   cat("Palette used:", paste(palette_finale, collapse=", "), "\n\n")
#     #
#     #   p <- fviz_pca_biplot(
#     #     res.pca,
#     #     geom.ind = "point",            # Show individuals as points
#     #     pointshape = 16,               # Solid round dots
#     #     pointsize = 2.25,               # Size of dots
#     #     alpha.ind = 0.9,               # Slight transparency
#     #     col.ind = df_cluster$Cluster,  # Color by cluster
#     #     palette = palette_finale,      # User or default palette
#     #     addEllipses = TRUE,            # Add confidence ellipses
#     #     ellipse.level = 0.95,
#     #     mean.point = FALSE,            # Hide cluster centroids
#     #     label = "var",                 # Show variable arrows
#     #     col.var = "gray35",            # Muted arrow color
#     #     arrowsize = 0.5,
#     #     repel = TRUE,                  # Avoid label overlap
#     #     legend.title = "Cluster"
#     #   ) +
#     #
#     #     ggtitle("Cluster structure across PCA projection") +
#     #     labs(
#     #       x = "Principal Component 1",
#     #       y = "Principal Component 2",
#     #       caption = "Ellipses represent 95% confidence regions"
#     #     ) +
#     #     theme_minimal(base_family = "sans", base_size = 14) +
#     #     theme(
#     #       plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#222222"),
#     #       legend.title = element_text(size = 13, face = "bold", color = "#222222"),
#     #       legend.text = element_text(size = 11, color = "#444444"),
#     #       axis.title = element_text(size = 14, face = "bold", color = "#222222"),
#     #       axis.text = element_text(size = 12, color = "#333333"),
#     #       panel.grid.major = element_line(color = "gray85", linewidth = 0.4),
#     #       panel.grid.minor = element_blank(),
#     #       plot.background = element_rect(fill = "#fcfcfc", color = NA),
#     #       panel.background = element_rect(fill = "white", color = NA),
#     #       legend.background = element_rect(fill = "white", color = NA),
#     #       legend.key = element_rect(fill = "white", color = NA),
#     #       plot.caption = element_text(size = 10, hjust = 1, color = "#555555")
#     #     ) +
#     #     guides(color = guide_legend(
#     #       override.aes = list(size = 4, alpha = 1),
#     #       title.position = "top",
#     #       title.hjust = 0.5
#     #     ))
#     #
#     #   print(p)
#     #
#     #   cat("\n")
#     #
#     #   # Option export PDF
#     #   # Export PDF
#     #   save_pdf <- tolower(safe_readline("Would you like to export the PCA plot as PDF? (yes/no): "))
#     #   cat("\n")
#     #   if (save_pdf %in% c("yes","y")) {
#     #     if (!rstudioapi::isAvailable()) cat("Saving requires RStudio.\n")
#     #     else {
#     #       pdf_path <- rstudioapi::selectFile(
#     #         caption = "Save PCA plot",
#     #         label = "Save",
#     #         path = getwd(),
#     #         filter = list("PDF files" = "pdf"),
#     #         existing = FALSE
#     #       )
#     #       if (nzchar(pdf_path)) {
#     #         if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE)) pdf_path <- paste0(pdf_path,".pdf")
#     #
#     #         # Demande dimensions à l'utilisateur
#     #         pdf_width <- as.numeric(safe_readline("Enter desired PDF width (in inches, e.g., 10): "))
#     #         pdf_height <- as.numeric(safe_readline("Enter desired PDF height (in inches, e.g., 8): "))
#     #
#     #         # Crée PDF avec dimensions personnalisées
#     #         pdf(pdf_path, width = pdf_width, height = pdf_height)
#     #         print(p)
#     #         dev.off()
#     #         cat("PCA plot saved to:", pdf_path, "\n")
#     #       } else cat("Saving cancelled.\n")
#     #     }
#     #   }
#     #
#     # }
#
# #     cat("\n")
# #
# #     restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
# #
# #     cat("\n")
# #
# #     if(tolower(restart) %in% c("no","n")) {
# #       cat("Clustering finished.\n")
# #       break
# #     }
# #
# #     # === Visualisation géochimique optionnelle ===
# #     cat("\n")
# #     show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))
# #     cat("\n")
# #
# #     if (show_geo %in% c("yes", "y", "oui", "o")) {
# #       if (exists("visual.clust")) {
# #         repeat {
# #           if (exists("df_denoised_total", envir = .GlobalEnv)) {
# #             if ("Cluster" %in% colnames(df_denoised_total)) {
# #               visual.clust(df_denoised_total)
# #               break
# #             } else {
# #               df_visu_name <- safe_readline("Enter the name of the dataframe containing 'Cluster': ")
# #               if (exists(df_visu_name, envir = .GlobalEnv)) {
# #                 df_visu <- get(df_visu_name, envir = .GlobalEnv)
# #                 if ("Cluster" %in% colnames(df_visu)) {
# #                   visual.clust(df_visu)
# #                   break
# #                 } else {
# #                   cat("The selected dataframe does not contain a 'Cluster' column. Try again.\n")
# #                 }
# #               } else {
# #                 cat("Dataframe not found. Try again.\n")
# #               }
# #             }
# #           }
# #         }
# #       } else {
# #         cat("The function 'visual.clust()' is not defined in the environment.\n")
# #         cat("Please make sure it is loaded before running this option.\n")
# #       }
# #     }
# #
# #   }
# # }
#
# # # === Visualisation géochimique optionnelle ===
# # cat("\n")
# # show_geo <- tolower(safe_readline("Would you like to visualize geochemical profiles by cluster? (yes/no): "))
# # cat("\n")
# #
# # if (show_geo %in% c("yes", "y", "oui", "o")) {
# #   if (exists("visual.clust")) {
# #
# #     repeat {
# #       # Vérifie si le dataframe sélectionné existe
# #       if (exists(df_denoised_total, envir = .GlobalEnv)) {
# #         df_visu <- get(df_denoised_total, envir = .GlobalEnv)
# #
# #         # Vérifie si la colonne Cluster existe
# #         if ("Cluster" %in% colnames(df_visu)) {
# #           visual.clust(df_visu)
# #           break
# #         } else {
# #           cat("The selected dataframe does not contain a 'Cluster' column.\n")
# #           df_visu_name <- safe_readline("Enter the name of the dataframe containing 'Cluster': ")
# #
# #           if (exists(df_visu_name, envir = .GlobalEnv)) {
# #             df_visu <- get(df_visu_name, envir = .GlobalEnv)
# #
# #             if ("Cluster" %in% colnames(df_visu)) {
# #               visual.clust(df_visu)
# #               break
# #             } else {
# #               cat("The provided dataframe also does not contain a 'Cluster' column. Try again.\n")
# #             }
# #           } else {
# #             cat("Dataframe not found. Try again.\n")
# #           }
# #         }
# #
# #       } else {
# #         cat("Dataframe not found. Try again.\n")
# #       }
# #     }
# #
# #   } else {
# #     cat("The function 'visual.clust()' is not defined in the environment.\n")
# #     cat("Please make sure it is loaded before running this option.\n")
# #   }
# # }
# # }
