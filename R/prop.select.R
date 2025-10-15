#' @title Analyse en Composantes Principales (ACP) avec extraction des variables clés
#'
#' @description
#' Fonction pour réaliser une ACP, visualiser les résultats, extraire les variables les plus importantes,
#' et permettre à l'utilisateur de sélectionner manuellement des variables supplémentaires pertinentes.
#'
#' @param df_normalized Un dataframe contenant les données normalisées après sélection.
#' @return Un dataframe contenant les variables les plus importantes selon les composantes principales retenues.
#' @export

prop.select <- function(df_normalized=NULL) {
  if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("compositions", quietly = TRUE)) install.packages("compositions")
  library(compositions)
  library(factoextra)
  library(ggplot2)
  library(rstudioapi)

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }

  repeat {
    # Check if df_normal is already in cache
    if ("df_normalized_cache" %in% list_cache()) {
      df_normalized <- get_cache("df_normalized_cache")
      message("Using cached 'df_normalized_cache' dataframe.")
      break
    }

    # Otherwise, show data frames available in the global environment
    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

    if (length(df_list) == 0) {
      stop("No data frames found in the global environment.")
    }

    cat("\nSelect the dataframe with the selected elements:\n")
    print(df_list)
    cat("\n")

    df_name <- readline("Enter the name of the dataframe from the exit of the scale_data function: ")

    # If a valid dataframe name is entered
    if (df_name %in% df_list) {
      df_normalized <- get(df_name, envir = .GlobalEnv)
      set_cache("df_normalized_cache", df_normalized)
      #message("Dataframe cached as 'df_normal' for future use.")
      break
    }

    # Otherwise, try again
    cat("\n Dataframe not found. Try again.\n")
  }

  repeat {
    acp <- prcomp(df_normalized, center = TRUE, scale. = FALSE)

    cat("\n====== Summary of the PCA ======\n\n")
    print(summary(acp))

    print(fviz_eig(acp, addlabels = TRUE))
    print(fviz_pca_var(acp, col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE,
                       title = "PCA - Variables"))

    eig_values <- acp$sdev^2
    variance_expliquee <- eig_values / sum(eig_values)
    variance_cumulee <- cumsum(variance_expliquee)
    n_comp <- length(eig_values)

    broken_stick <- function(n) {
      sapply(1:n, function(k) sum(1 / (k:n)) / n) * sum(eig_values)
    }
    bstick_values <- broken_stick(n_comp)

    cat("\n")

    repeat {
      input <- safe_readline("Enter the cumulative variance threshold (e.g. 0.70): ")
      seuil_variance <- as.numeric(input)
      if (!is.na(seuil_variance) && seuil_variance > 0 && seuil_variance < 1) break
      cat("Please enter a valid number between 0 and 1.\n")
    }
    n_variance <- which(variance_cumulee >= seuil_variance)[1]
    n_bstick <- sum(eig_values > bstick_values)

    # === Sélection de la méthode par l'utilisateur ===
    repeat {
      cat("\nChoose the method to determine number of components:\n")
      cat("1) Cumulative variance method (", n_variance, " components)\n")
      cat("2) Broken-stick method (", n_bstick, " components)\n")
      cat("3) Manual choice\n")
      cat("\n")
      method_choice <- safe_readline("Enter (1) Cumulative variance, (2) Broken-stick  or (3) Manual: ")
      if (method_choice %in% c("1", "2", "3")) break
      cat("\n")
      cat("Please enter a valid option: 1, 2 or 3.\n")
      cat("\n")
    }

    if (method_choice == "1") {
      n_dims <- n_variance
      methode_selection <- "Cumulative Variance"
    } else if (method_choice == "2") {
      n_dims <- n_bstick
      methode_selection <- "Broken-Stick"
    } else {
      repeat {
        cat("\n")
        input <- safe_readline(paste0("Enter manually the number of components to retain (1–", n_comp, "): "))
        n_dims <- as.integer(input)
        if (!is.na(n_dims) && n_dims >= 1 && n_dims <= n_comp) break
        cat("\n")
        cat("Please enter a valid integer between 1 and ", n_comp, ".\n")
      }
      methode_selection <- "Manual Choice"
    }

    loadings <- acp$rotation[, 1:n_dims, drop = FALSE]

    cat("\n")

    repeat {
      reponse_affichage <- tolower(safe_readline("\nDisplay the loadings of selected components? (yes/no): "))
      if (reponse_affichage %in% c("yes", "no")) break
      cat("Please type 'yes' or 'no'.\n")
    }
    if (reponse_affichage == "yes") {
      for (i in 1:n_dims) {
        cat(paste0("\nComponent ", i, " (", round(variance_expliquee[i] * 100, 1), "%):\n"))
        print(loadings[, i, drop = FALSE])
      }
    }

    cat("\n")

    repeat {
      input <- safe_readline("Enter a percentile threshold for variable selection (e.g., 0.9): ")
      quantile_thresh <- as.numeric(input)
      if (!is.na(quantile_thresh) && quantile_thresh > 0 && quantile_thresh < 1) break
      cat("Please enter a valid number between 0 and 1.\n")
    }

    vars_selectionnees <- c()
    for (i in 1:n_dims) {
      poids <- abs(loadings[, i])
      seuil_poids <- quantile(poids, probs = quantile_thresh)
      vars_i <- names(poids)[poids >= seuil_poids]
      vars_selectionnees <- c(vars_selectionnees, vars_i)
      cat(paste0("\nComponent ", i, ": ", paste(vars_i, collapse = ", "), "\n"))
    }
    vars_selectionnees <- unique(vars_selectionnees)

    results_table <- data.frame(
      Component = character(),
      Variable = character(),
      Loading = numeric(),
      PercentileThreshold = numeric(),
      stringsAsFactors = FALSE
    )
    for (i in 1:n_dims) {
      poids <- abs(loadings[, i])
      seuil_poids <- quantile(poids, probs = quantile_thresh)
      vars_i <- names(poids)[poids >= seuil_poids]
      for (var in vars_i) {
        results_table <- rbind(results_table, data.frame(
          Component = paste0("PC", i),
          Variable = var,
          Loading = round(poids[var], 4),
          PercentileThreshold = quantile_thresh,
          stringsAsFactors = FALSE
        ))
      }
    }

    cat("\n======= Automatically Selected Variables =======\n\n")
    print(results_table)

    repeat {
      export_response <- tolower(safe_readline("\nExport this table? (yes/no): "))
      if (export_response %in% c("yes", "no")) break
      cat("Please type 'yes' or 'no'.\n")
    }

    if (export_response == "yes") {
      repeat {
        format_choice <- tolower(safe_readline("Choose export format: csv or xlsx: "))
        if (format_choice %in% c("csv", "xlsx")) break
        cat("Please enter 'csv' or 'xlsx'.\n")
      }
      cat("\nSelect file location to save the output.\n\n")
      default_ext <- ifelse(format_choice == "csv", ".csv", ".xlsx")
      file_path <- rstudioapi::selectFile(caption = "Save As", label = "Save", existing = FALSE)
      if (!is.null(file_path) && file_path != "") {
        if (!grepl(paste0("\\", default_ext, "$"), file_path)) file_path <- paste0(file_path, default_ext)
        if (format_choice == "csv") write.csv(results_table, file_path, row.names = FALSE)
        else {
          if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
          library(openxlsx)
          write.xlsx(results_table, file_path, rowNames = FALSE)
        }
        cat(paste0("\nTable successfully exported to: ", file_path, "\n"))
        cat("\n")
      } else cat("Export cancelled: No file selected.\n")
      cat("\n")
    } else cat("Table not exported.\n")

    cat("\n")

    resume_pca <- paste0(
      "\n",
      "==================== PCA SUMMARY ====================\n",
      sprintf("Observations               : %d", nrow(df_normalized)), "\n",
      sprintf("Original variables         : %d", ncol(df_normalized)), "\n",
      "-----------------------------------------------------\n",
      sprintf("Cumulative variance cutoff : %.2f", seuil_variance), "\n",
      sprintf("→ Components (cum. var.)   : %d", n_variance), "\n",
      sprintf("→ Components (broken-stick): %d", n_bstick), "\n",
      sprintf("Selected method            : %s", methode_selection), "\n",
      sprintf("Components retained        : %d", n_dims), "\n",
      sprintf("Cumulative variance (ret.) : %.2f%%", variance_cumulee[n_dims] * 100), "\n",
      sprintf("Percentile threshold       : %.2f", quantile_thresh), "\n",
      sprintf("Variables selected         : %d", length(vars_selectionnees)), "\n",
      "-----------------------------------------------------\n",
      "Selected variables:\n",
      paste0("→ ", paste(strwrap(paste(vars_selectionnees, collapse = ", "), width = 70),
                         collapse = "\n   ")), "\n",
      "=====================================================\n"
    )
    cat(resume_pca)

    cat("\n")

    repeat {
      save_summary <- tolower(safe_readline("Save a summary report (.txt)? (yes/no): "))
      if (save_summary %in% c("yes", "no")) break
      cat("Please enter 'yes' or 'no'.\n")
    }
    if (save_summary == "yes") {
      file_path_summary <- rstudioapi::selectFile(caption = "Save summary", label = "Save", existing = FALSE)
      if (!is.null(file_path_summary) && file_path_summary != "") {
        if (!grepl("\\.txt$", file_path_summary)) file_path_summary <- paste0(file_path_summary, ".txt")
        writeLines(resume_pca, con = file_path_summary)
        cat(paste0("\nSummary report successfully saved to: ", file_path_summary, "\n"))
      } else cat("No file selected. Summary not saved.\n")
    } else cat("Summary not saved.\n")

    variables_cluster <- df_normalized[, vars_selectionnees, drop = FALSE]
    variables_cluster <- variables_cluster[, !duplicated(t(variables_cluster))]

    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x)))]

    repeat {
      # Check if df_normal is already in cache
      if ("df_clean_cache" %in% list_cache()) {
        df_original <- get_cache("df_clean_cache")
        message("Using cached 'df_clean_cache' dataframe.")
        break
      }

      # Otherwise, show data frames available in the global environment
      df_list <- ls(envir = .GlobalEnv)
      df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

      if (length(df_list) == 0) {
        stop("No data frames found in the global environment.")
      }

      cat("\nSelect the dataframe used in the function scale_data:\n")
      print(df_list)
      cat("\n")

      df_name <- readline("Enter the name of the dataframe: ")

      # If a valid dataframe name is entered
      if (df_name %in% df_list) {
        df_original <- get(df_name, envir = .GlobalEnv)
        set_cache("df_clean_cache", df_original)
        message("Dataframe cached as 'df_normal' for future use.")
        break
      }

      # Otherwise, try again
      cat("\n Dataframe not found. Try again.\n")
    }
    # repeat {
    #   cat("\nSelect the dataframe used in the function (scale_data):\n")
    #   print(df_list)
    #   cat("\n")
    #   df_name <- safe_readline("Enter the name of the dataframe: ")
    #   if (df_name %in% df_list) {
    #     df_original <- get(df_name, envir = .GlobalEnv)
    #     break
    #   }
    #   cat("\nDataframe not found. Try again.\n")
    # }

    variables_cluster_original <- df_original[, colnames(variables_cluster), drop = FALSE]
    variables_cluster <- as.data.frame(variables_cluster_original)

    set_cache("variables_cluster_cache",variables_cluster)

    repeat {
      redo <- tolower(safe_readline("\nRestart PCA with other parameters? (yes/no): "))
      if (redo %in% c("yes", "no")) break
      cat("Please enter 'yes' or 'no'.\n")
    }
    if (redo == "no") {
      cat("\n‘variables_cluster’ dataframe was created with the most significant variables.\n")
      cat("\nAnalysis complete.\n")
      break
    } else cat("\nRestarting PCA analysis...\n\n")
  }

  return(invisible(variables_cluster))
}
