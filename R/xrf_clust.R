#' Fonction pour normaliser les données, déterminer le nombre optimal de clusters,
#' et ajouter les résultats au dataframe choisi par l'utilisateur
#'
#' @param data Un dataframe contenant les données numériques à analyser
#' @return Le dataframe avec la colonne "Cluster" ajoutée
#' @export


# Fonction utilitaire pour sécuriser les entrées utilisateur



xrf_clust <- function(data) {
  # Chargement des packages nécessaires
  required_packages <- c("NbClust", "ggplot2", "writexl", "rstudioapi")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  if (!is.data.frame(data) && !is.matrix(data)) stop("The entry must be either variables_cluster or variables_cluster_manual.")
  if (!all(sapply(data, is.numeric))) stop("All columns must be numeric.")

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via exit.")
    return(input)
  }

  repeat {  # 🔁 BOUCLE GLOBALE
    set.seed(123)

    # ---- Méthode de clustering ----
    method_choice <- NULL
    repeat {
      cat("\nAvailable clustering methods:\n1. K-means\n2. Hierarchical clustering\n")
      cat("\n")
      method_choice <- as.integer(safe_readline("Choose a method (1 or 2): "))
      cat("\n")
      if (method_choice %in% c(1, 2)) break
      cat("Invalid choice. Please enter 1 or 2.\n")
    }

    # ---- Nombre maximal de clusters ----
    max_clusters <- NULL
    repeat {
      max_clusters <- as.integer(safe_readline("Enter the maximum number of clusters to test (min 2, e.g. 10): "))
      if (!is.na(max_clusters) && max_clusters >= 2) break
      cat("Invalid number. Please enter an integer ≥ 2.\n")
    }

    # ---- Distance ----
    available_distances <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "NULL")
    chosen_distance <- NULL
    repeat {
      cat("\nDistances available for NbClust:\n")
      for (i in seq_along(available_distances)) cat(i, "-", available_distances[i], "\n")
      cat("\n")
      distance_choice <- safe_readline("Select a distance (number or press Enter if 'K-means method'): ")

      if (distance_choice == "") {
        chosen_distance <- "euclidean"
        break
      }

      distance_num <- as.integer(distance_choice)
      if (!is.na(distance_num) && distance_num >= 1 && distance_num <= length(available_distances)) {
        chosen_distance <- available_distances[distance_num]
        break
      }

      if (tolower(distance_choice) %in% available_distances) {
        chosen_distance <- tolower(distance_choice)
        break
      }

      cat("Invalid input. Please try again.\n")
    }

    # ---- Méthode hiérarchique (si choisie) ----
    hclust_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
    method_name <- NULL

    if (method_choice == 1) {
      method_name <- "kmeans"
    } else {
      repeat {
        cat("\nAvailable hierarchical methods:\n")
        for (i in seq_along(hclust_methods)) cat(i, "-", hclust_methods[i], "\n")
        cat("\n")
        cat("\n")
        hclust_choice <- as.integer(safe_readline("Choose a hierarchical method (number): "))
        if (!is.na(hclust_choice) && hclust_choice >= 1 && hclust_choice <= length(hclust_methods)) {
          method_name <- hclust_methods[hclust_choice]
          break
        }
        cat("Invalid choice. Try again.\n")
      }
    }

    # ---- NbClust : nombre optimal de clusters ----
    cat("\nDetermining the optimal number of clusters...\n")
    nb <- NbClust(data, distance = chosen_distance, min.nc = 2, max.nc = max_clusters, method = method_name)

    results <- nb$Best.nc[1, ]
    freq_table <- sort(table(results), decreasing = TRUE)
    max_freq <- max(freq_table)
    clusters_with_max_freq <- as.numeric(names(freq_table[freq_table == max_freq]))

    optimal_clusters_max <- NULL
    if (length(clusters_with_max_freq) > 1) {
      repeat {
        cat("\n")
        cat("==== WARNING SEVERAL OPTIMAL ====\n")
        cat("\n")
        cat("Multiple optimal options detected:", paste(clusters_with_max_freq, collapse = ", "), "\n")
        cat("\n")
        cat("\n")
        user_input <- as.integer(safe_readline("Choose the number of clusters from these options: "))
        if (user_input %in% clusters_with_max_freq) {
          optimal_clusters_max <- user_input
          break
        }
        cat("Invalid choice. Try again.\n")
      }
    } else {
      optimal_clusters_max <- clusters_with_max_freq
      cat("Optimal number of clusters selected:", optimal_clusters_max, "\n")
    }

    # ---- Affichage résumé NbClust ----
    summary_lines <- c()
    summary_lines <- c(summary_lines, strrep("*", 67))
    summary_lines <- c(summary_lines, sprintf("* %s *", format(" Summary of Clustering Indices ", width = 63, justify = "centre")))
    summary_lines <- c(summary_lines, strrep("*", 67))

    # Ajout de la méthode et du nombre max de clusters
    method_text <- ifelse(method_choice == 1,
                          "Method chosen: K-means",
                          paste("Method chosen: Hierarchical clustering (", method_name, ")", sep = ""))
    summary_lines <- c(summary_lines, sprintf("* %-63s *", method_text))
    summary_lines <- c(summary_lines, sprintf("* %-63s *", paste("Max number of clusters tested:", max_clusters)))
    summary_lines <- c(summary_lines, strrep("*", 67))

    summary_lines <- c(summary_lines, sprintf("* %-30s | %-30s *", "Nb. of Clusters", "Nb. of Indices"))
    summary_lines <- c(summary_lines, strrep("*", 67))

    for (i in seq_along(freq_table)) {
      summary_lines <- c(summary_lines, sprintf("* %-30s | %-30s *",
                                                names(freq_table)[i],
                                                freq_table[i]))
    }

    summary_lines <- c(summary_lines, strrep("*", 67))
    summary_lines <- c(summary_lines, sprintf("* %s *", format(" Conclusion ", width = 63, justify = "centre")))
    summary_lines <- c(summary_lines, sprintf("* %s *", format(paste("According to the majority rule, the best number of clusters is", optimal_clusters_max),
                                                               width = 63, justify = "centre")))
    summary_lines <- c(summary_lines, strrep("*", 67), "")

    cat(paste0(summary_lines, collapse = "\n"))

    cat("\n")
    cat("\n")

    # ---- Demande sauvegarde résumé ----
    save_summary <- tolower(safe_readline("Would you like to save this summary to a text file? (yes/no): "))
    if (save_summary %in% c("yes", "y")) {
      if (!rstudioapi::isAvailable()) {
        cat("Saving requires RStudio.\n")
      } else {
        file_path <- rstudioapi::selectFile(
          caption = "Save clustering summary",
          label = "Save",
          path = getwd(),
          filter = list("Text files" = "txt"),
          existing = FALSE
        )

        if (nzchar(file_path)) {
          if (!grepl("\\.txt$", file_path, ignore.case = TRUE)) {
            file_path <- paste0(file_path, ".txt")
          }
          writeLines(summary_lines, con = file_path)
          cat("Summary saved to:", file_path, "\n")
        } else {
          cat("Saving cancelled.\n")
        }
      }
    }

    # ---- Application du clustering ----
    clustering_result <- NULL
    if (method_name == "kmeans") {
      clustering_result <- kmeans(data, centers = optimal_clusters_max, nstart = 100)
    } else {
      dist_matrix <- dist(data)
      hc <- hclust(dist_matrix, method = method_name)
      clusters <- cutree(hc, k = optimal_clusters_max)
      clustering_result <- list(cluster = clusters)
    }

    # ---- Sélection du dataframe cible ----
    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x)))]

    selected_df <- NULL
    df_name <- NULL
    cat("\n")
    repeat {
      cat("\nSelect the dataframe where you want to add the ‘Cluster’ column:\n")
      print(df_list)

      cat("\n")

      df_name <- safe_readline("Enter the name of the target dataframe: ")

      if (df_name %in% df_list) {
        selected_df <- get(df_name)
        selected_df$Cluster <- clustering_result$cluster
        assign(df_name, selected_df, envir = .GlobalEnv)
        cat("Cluster column added to", df_name, "\n")
        break
      }

      cat("\n")
      cat("Dataframe not found. Try again.\n")
    }

    # ---- Export ----
    cat("\n")
    save_choice <- tolower(safe_readline("Would you like to save this dataframe? (yes/no): "))
    cat("\n")
    if (save_choice == "yes") {
      repeat {
        ext_choice <- tolower(safe_readline("Desired extension: csv or xlsx: "))
        if (!ext_choice %in% c("csv", "xlsx")) {
          cat("Invalid extension. Please type 'csv' or 'xlsx'.\n")
          next
        }

        if (!rstudioapi::isAvailable()) {
          cat("Backup requires RStudio.\n")
          break
        }

        cat("Select the destination file...\n")
        chemin_save <- rstudioapi::selectFile(
          caption = "Save the file",
          label = "Save",
          path = getwd(),
          filter = if (ext_choice == "csv") list("CSV files" = "csv") else list("Excel files" = "xlsx"),
          existing = FALSE
        )

        if (!nzchar(chemin_save)) {
          cat("Saving cancelled.\n")
          break
        }

        if (!grepl(paste0("\\.", ext_choice, "$"), chemin_save, ignore.case = TRUE)) {
          chemin_save <- paste0(chemin_save, ".", ext_choice)
        }

        if (ext_choice == "csv") {
          write.csv(selected_df, chemin_save, row.names = FALSE)
        } else {
          writexl::write_xlsx(selected_df, path = chemin_save)
        }

        cat("Saved file:", chemin_save, "\n")
        break
      }
    }

    # ---- Relancer ? ----
    cat("\n")
    restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
    cat("\n")
    if (tolower(restart) %in% c("no", "n")) {
      cat("Clustering finished.\n")
      break
    }
  }

  return(selected_df)
}
