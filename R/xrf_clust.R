#' Fonction pour normaliser les données, déterminer le nombre optimal de clusters,
#' et ajouter les résultats au dataframe choisi par l'utilisateur
#'
#' @param data Un dataframe contenant les données numériques à analyser
#' @return Le dataframe avec la colonne "Cluster" ajoutée
#' @export


# Fonction utilitaire pour sécuriser les entrées utilisateur



xrf_clust <- function(data) {
  # Packages nécessaires
  required_packages <- c("NbClust", "ggplot2", "writexl", "rstudioapi")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  if (!is.data.frame(data) && !is.matrix(data)) stop("The entry must be a dataframe or matrix.")
  if (!all(sapply(data, is.numeric))) stop("All columns must be numeric.")

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User interrupted execution via exit.")
    return(input)
  }

  repeat {
    set.seed(123)

    cat("\n---------------------------------\n")
    cat("       Clustering Method         \n")
    cat("---------------------------------\n\n")

    # Choix méthode
    repeat {
      cat("Available clustering methods:\n1. K-means\n2. Hierarchical clustering\n\n")
      method_choice <- as.integer(safe_readline("Choose a method (1 or 2): "))
      if (method_choice %in% c(1,2)) break
      cat("Invalid choice. Please enter 1 or 2.\n\n")
    }
    cat("\n")

    # Nombre max clusters
    repeat {
      max_clusters <- as.integer(safe_readline("Enter max number of clusters to test (min 2, e.g. 10): "))
      if (!is.na(max_clusters) && max_clusters>=2) break
      cat("Invalid number. Please enter integer ≥2.\n\n")
    }
    cat("\n")

    # Distance seulement si hiérarchique
    chosen_distance <- "euclidean"
    if(method_choice==2){
      available_distances <- c("euclidean","maximum","manhattan","canberra","binary","minkowski")
      repeat {
        cat("Distances available for NbClust:\n")
        for(i in seq_along(available_distances)) cat(i,"-",available_distances[i],"\n")
        cat("\n")
        distance_choice <- safe_readline("Select a distance (number): ")
        distance_num <- as.integer(distance_choice)
        if(!is.na(distance_num) && distance_num>=1 && distance_num<=length(available_distances)){
          chosen_distance <- available_distances[distance_num]
          break
        }
        if(tolower(distance_choice) %in% available_distances){
          chosen_distance <- tolower(distance_choice)
          break
        }
        cat("Invalid input. Try again.\n\n")
      }
    }
    cat("\n")

    # Méthode hiérarchique
    hclust_methods <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
    method_name <- ifelse(method_choice==1,"kmeans",NULL)
    if(method_choice==2){
      repeat {
        cat("Available hierarchical methods:\n")
        for(i in seq_along(hclust_methods)) cat(i,"-",hclust_methods[i],"\n")
        cat("\n")
        hclust_choice <- as.integer(safe_readline("Choose a hierarchical method (number): "))
        if(!is.na(hclust_choice) && hclust_choice>=1 && hclust_choice<=length(hclust_methods)){
          method_name <- hclust_methods[hclust_choice]
          break
        }
        cat("Invalid choice. Try again.\n\n")
      }
    }
    # Déterminer nombre optimal de clusters
    cat("\n---------------------------------\n")
    cat("   Determining Optimal Clusters  \n")
    cat("---------------------------------\n\n")
    nb <- NbClust(data, distance=chosen_distance, min.nc=2, max.nc=max_clusters, method=method_name)
    results <- nb$Best.nc[1,]
    freq_table <- sort(table(results), decreasing=TRUE)
    max_freq <- max(freq_table)
    clusters_with_max_freq <- as.numeric(names(freq_table[freq_table==max_freq]))

    if(length(clusters_with_max_freq)==1) optimal_clusters_max <- clusters_with_max_freq
    else {
      repeat {
        cat("Multiple optimal cluster numbers detected:", paste(clusters_with_max_freq, collapse=", "),"\n")
        user_input <- as.integer(safe_readline("Choose the number of clusters from these options: "))
        if(user_input %in% clusters_with_max_freq) {
          optimal_clusters_max <- user_input
          break
        }
        cat("Invalid choice. Try again.\n\n")
      }
    }

    cat("\nOptimal number of clusters selected:", optimal_clusters_max,"\n\n")

    # Appliquer clustering
    if(method_name=="kmeans"){
      clustering_result <- kmeans(data, centers=optimal_clusters_max, nstart=100)
    } else {
      dist_matrix <- dist(data)
      hc <- hclust(dist_matrix, method=method_name)
      clusters <- cutree(hc, k=optimal_clusters_max)
      clustering_result <- list(cluster=clusters)
    }

    # Sélection dataframe cible
    cat("\n---------------------------------\n")
    cat("   Select Target Dataframe       \n")
    cat("---------------------------------\n\n")
    df_list <- ls(envir=.GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x)))]
    repeat {
      cat("Dataframes available:\n")
      print(df_list)
      cat("\n")
      df_name <- safe_readline("Enter the name of the target dataframe: ")
      if(df_name %in% df_list){
        selected_df <- get(df_name)
        selected_df$Cluster <- clustering_result$cluster
        assign(df_name, selected_df, envir=.GlobalEnv)
        cat("\nCluster column added to", df_name,"\n\n")
        break
      }
      cat("Dataframe not found. Try again.\n\n")
    }

    # Export dataframe final
    save_choice <- tolower(safe_readline("Would you like to save this dataframe? (yes/no): "))
    if(save_choice %in% c("yes","y")){
      repeat {
        ext_choice <- tolower(safe_readline("Desired extension: csv or xlsx: "))
        if(!ext_choice %in% c("csv","xlsx")) {
          cat("Invalid extension. Please type 'csv' or 'xlsx'.\n\n")
          next
        }
        if(!rstudioapi::isAvailable()){
          cat("Saving requires RStudio.\n\n")
          break
        }
        cat("\nSelect the destination file...\n")
        chemin_save <- rstudioapi::selectFile(
          caption="Save the file",
          label="Save",
          path=getwd(),
          filter=if(ext_choice=="csv") list("CSV files"="csv") else list("Excel files"="xlsx"),
          existing=FALSE
        )
        if(!nzchar(chemin_save)){ cat("Saving cancelled.\n\n"); break }
        if(!grepl(paste0("\\.",ext_choice,"$"), chemin_save, ignore.case=TRUE)) chemin_save <- paste0(chemin_save,".",ext_choice)
        if(ext_choice=="csv") write.csv(selected_df, chemin_save, row.names=FALSE)
        else writexl::write_xlsx(selected_df, path=chemin_save)
        cat("\nSaved file:", chemin_save,"\n\n")
        break
      }
    }

    # ---------------- Cluster Drivers Analysis ----------------
    cat("\n==============================\n")
    cat("   Cluster Drivers Analysis   \n")
    cat("==============================\n\n")

    # Profils moyens
    cluster_profiles <- aggregate(data, by=list(Cluster=clustering_result$cluster), mean)
    cluster_profiles_rounded <- cluster_profiles
    cluster_profiles_rounded[,-1] <- round(cluster_profiles[,-1],0)
    #cat("=== Cluster Profiles (mean values per cluster) ===\n\n")
    #print(cluster_profiles_rounded)
    cat("\n")

    # ANOVA F-values
    cat("=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===\n\n")
    anova_results <- data.frame(Variable=character(), F_value=numeric(), p_value=numeric(), stringsAsFactors=FALSE)
    for(col in colnames(data)){
      model <- aov(data[[col]] ~ as.factor(clustering_result$cluster))
      s <- summary(model)[[1]]
      F_val <- s$`F value`[1]
      p_val <- s$`Pr(>F)`[1]
      anova_results <- rbind(anova_results,data.frame(Variable=col,F_value=F_val,p_value=p_val))
    }
    anova_results <- anova_results[order(-anova_results$F_value),]
    anova_results$F_value <- round(anova_results$F_value,1)
    anova_results$p_value <- signif(anova_results$p_value,3)
    print(anova_results)
    cat("\n")

    # Profil synthétique (corrigé, variable par variable)
    cat("=== Cluster Description Profiles ===\n\n")
    vars <- colnames(cluster_profiles)[-1]
    for(i in 1:nrow(cluster_profiles)){
      profile_text <- c()
      for(v in vars){
        val <- cluster_profiles[i, v]
        q <- quantile(cluster_profiles[[v]], probs=c(0.25,0.5,0.75))
        if(val >= q[3]) lvl <- "very high"
        else if(val >= q[2]) lvl <- "high"
        else if(val >= q[1]) lvl <- "medium"
        else lvl <- "low"
        profile_text <- c(profile_text, paste(v, lvl, sep=": "))
      }
      cat(paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse=", "), "\n"))
    }
    cat("\n")

    # Export optionnel Cluster Drivers Analysis
    save_drivers <- tolower(safe_readline("Would you like to save the cluster drivers analysis to a text file? (yes/no): "))
    if(save_drivers %in% c("yes","y")){
      if(!rstudioapi::isAvailable()) cat("Saving requires RStudio.\n\n")
      else {
        file_path <- rstudioapi::selectFile(
          caption="Save cluster drivers analysis",
          label="Save",
          path=getwd(),
          filter=list("Text files"="txt"),
          existing=FALSE
        )
        if(nzchar(file_path)){
          if(!grepl("\\.txt$", file_path, ignore.case=TRUE)) file_path <- paste0(file_path,".txt")
          analysis_lines <- c(
            "\n",
            "=== Cluster Drivers Analysis === ",
            "\n",
            "=== Variable Importance in Cluster Formation (Ranked by ANOVA F-values) ===",
            "\n",
            capture.output(print(anova_results)),
            "",
            "\n",
            "=== Cluster Description Profiles ===",
            "\n"
          )
          for(i in 1:nrow(cluster_profiles)){
            profile_text <- c()
            for(v in vars){
              val <- cluster_profiles[i, v]
              q <- quantile(cluster_profiles[[v]], probs=c(0.25,0.5,0.75))
              if(val >= q[3]) lvl <- "very high"
              else if(val >= q[2]) lvl <- "high"
              else if(val >= q[1]) lvl <- "medium"
              else lvl <- "low"
              profile_text <- c(profile_text, paste(v, lvl, sep=": "))
            }
            analysis_lines <- c(analysis_lines, paste0("Cluster ", cluster_profiles$Cluster[i], " → ", paste(profile_text, collapse=", ")))
          }
          writeLines(analysis_lines, con=file_path)
          cat("\nCluster drivers analysis saved to:", file_path,"\n\n")
        } else cat("Saving cancelled.\n\n")
      }
    }

    # Relancer ?
    restart <- safe_readline("\nWould you like to perform another clustering? (yes/no): ")
    cat("\n")
    if(tolower(restart) %in% c("no","n")) {
      cat("Clustering finished.\n")
      break
    }
  }

  return(selected_df)
}
