#' @title Analyse en Composantes Principales (ACP) avec extraction des variables clés
#'
#' @description
#' Fonction pour réaliser une ACP, visualiser les résultats, extraire les variables les plus importantes,
#' et permettre à l'utilisateur de sélectionner manuellement des variables supplémentaires pertinentes.
#'
#' @param df_normalized Un dataframe contenant les données normalisées après sélection.
#' @return Un dataframe contenant les variables les plus importantes selon les composantes principales retenues.
#' @export

prop.select <- function(df_normalized) {
  if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("compositions", quietly = TRUE)) install.packages("compositions")
  library(compositions)
  library(factoextra)
  library(ggplot2)
  library(rstudioapi)

  # Fonction utilitaire pour sécuriser les entrées utilisateur
  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }

  repeat {
    acp <- prcomp(df_normalized, center = TRUE, scale. = FALSE)

    cat("\n====== Summary of the PCA ======\n\n")
    print(summary(acp))

    cat("\n====== Disclaimers ======\n\n")
    cat("The objective of PCA is to identify the main variables that drive the data.\n")
    cat("You have two methods to decide how many components to retain:\n")
    cat("1) Cumulative variance\n")
    cat("2) Broken-stick method\n\n")

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
      sapply(1:n, function(k) {
        sum(1 / (k:n)) / n
      }) * sum(eig_values)
    }
    bstick_values <- broken_stick(n_comp)

    cat("\n====== Method 1: Cumulative Variance ======\n\n")
    cat("It is advisable to take at least ‘70% cumulative variance’: This is often considered sufficient for a good representation of the data.\n\n")

    repeat {
      input <- safe_readline("Enter the cumulative variance threshold (e.g. min=0.70 for 70%): ")
      seuil_variance <- as.numeric(input)
      if (!is.na(seuil_variance) && seuil_variance > 0 && seuil_variance < 1) break
      cat("Please enter a valid number between 0 and 1.\n")
    }

    n_variance <- which(variance_cumulee >= seuil_variance)[1]
    cat(paste0("→ Components to reach ", seuil_variance * 100, "% variance: ", n_variance, "\n\n"))

    cat("\n====== Method 2: Broken-stick ======\n\n")
    n_bstick <- sum(eig_values > bstick_values)
    cat(paste0("→ Components retained using broken-stick: ", n_bstick, "\n\n"))

    df_comp <- data.frame(
      Component = 1:n_comp,
      Eigenvalue = eig_values,
      BrokenStick = bstick_values
    )

    print(
      ggplot(df_comp, aes(x = Component)) +
        geom_line(aes(y = Eigenvalue, color = "Eigenvalue"), size = 1) +
        geom_point(aes(y = Eigenvalue, color = "Eigenvalue"), size = 2) +
        geom_line(aes(y = BrokenStick, color = "Broken-Stick"), linetype = "dashed", size = 1) +
        geom_point(aes(y = BrokenStick, color = "Broken-Stick"), size = 2) +
        scale_color_manual(name = "Legend",
                           values = c("Eigenvalue" = "blue", "Broken-Stick" = "red")) +
        labs(title = "Eigenvalues vs Broken-Stick",
             y = "Eigenvalues", x = "Components") +
        theme_minimal() +
        theme(legend.position = "top")
    )

    cat("\n====== WARNING: ======\n\n")
    cat("Regardless of the selection method used, it is advisable to have a cumulative variance of at least 70%.\n\n")

    repeat {
      input <- safe_readline(paste0("Choose the number of components to retain (", n_variance, " or ", n_bstick, "): "))
      n_dims <- as.integer(input)
      if (!is.na(n_dims) && n_dims >= 1 && n_dims <= n_comp) break
      cat("Please enter a valid integer between 1 and ", n_comp, ".\n")
    }

    methode_selection <- ifelse(n_dims == n_variance, "Cumulative Variance",
                                ifelse(n_dims == n_bstick, "Broken-Stick", "Manual Choice"))

    loadings <- acp$rotation[, 1:n_dims, drop = FALSE]

    repeat {
      reponse_affichage <- tolower(safe_readline("\nWould you like to display the loadings of each selected component? (yes/no): "))
      if (reponse_affichage %in% c("yes", "no")) break
      cat("Please type ‘yes’ or ‘no’.\n")
    }

    if (reponse_affichage == "yes") {
      for (i in 1:n_dims) {
        cat(paste0("\nComponent ", i, " (", round(variance_expliquee[i] * 100, 1), "%):\n"))
        print(loadings[, i, drop = FALSE])
      }
    }

    cat("\nAutomatic variable selection based on weight percentile threshold.\n\n")

    repeat {
      input <- safe_readline("Enter a percentile threshold (e.g., 0.9): ")
      quantile_thresh <- as.numeric(input)
      if (!is.na(quantile_thresh) && quantile_thresh > 0 && quantile_thresh < 1) break
      cat("Please enter a valid number between 0 and 1.\n")
    }

    vars_selectionnees <- c()
    for (i in 1:n_dims) {
      poids <- abs(loadings[, i])
      seuil_poids <- quantile(poids, probs = quantile_thresh)
      vars_i <- names(poids)[poids >= seuil_poids]

      cat(paste0("\nComponent ", i, " (", round(variance_expliquee[i] * 100, 1), "%):\n"))
      cat("→ Weight threshold: ", round(seuil_poids, 4), "\n")
      cat("→ Variables selected: ", paste(vars_i, collapse = ", "), "\n")

      vars_selectionnees <- c(vars_selectionnees, vars_i)
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
      export_response <- tolower(safe_readline("\nWould you like to export this table? (yes/no): "))
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
        if (!grepl(paste0("\\", default_ext, "$"), file_path)) {
          file_path <- paste0(file_path, default_ext)
        }

        if (format_choice == "csv") {
          write.csv(results_table, file_path, row.names = FALSE)
        } else {
          if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
          library(openxlsx)
          write.xlsx(results_table, file_path, rowNames = FALSE)
        }

        cat(paste0("\nTable successfully exported to: ", file_path, "\n"))
      } else {
        cat("Export cancelled: No file selected.\n")
      }
    } else {
      cat("Table not exported.\n")
    }



    # repeat {
    #   reponse_manuelle <- tolower(safe_readline("\nWould you like to set variables manually? (yes/no): "))
    #   if (reponse_manuelle %in% c("yes", "no")) break
    #   cat("Please type 'yes' or 'no'.\n")
    # }
    #
    # if (reponse_manuelle == "yes") {
    #   print(colnames(df_normalized))
    #   user_input <- safe_readline("Enter variable names (comma-separated): ")
    #
    #   if (nzchar(user_input)) {
    #     selected_vars <- trimws(strsplit(user_input, ",")[[1]])
    #     selected_vars <- selected_vars[selected_vars %in% colnames(df_normalized)]
    #     if (length(selected_vars) > 0) {
    #       variables_cluster_manual <<- df_normalized[, selected_vars, drop = FALSE]
    #       cat("\n‘variables_cluster_manual’ created with manual selection.\n")
    #     } else {
    #       cat("\nNo valid variable found in your input.\n")
    #     }
    #   }
    # }

    resume_pca <- paste0(
      "===== PCA Summary Report =====\n\n",
      "Analysis performed on ", nrow(df_normalized), " observations and ", ncol(df_normalized), " original variables.\n\n",
      "Cumulative variance threshold: ", seuil_variance, "\n",
      "Cumulative variance selected components: ", n_variance, "\n",
      "Broken-stick selected components: ", n_bstick, "\n",
      "Selection method used: ", methode_selection, "\n",
      "User-retained components: ", n_dims, "\n",
      "Cumulative variance (retained comps): ", round(variance_cumulee[n_dims] * 100, 2), "%\n",
      "Quantile threshold: ", quantile_thresh, "\n",
      "Number of variables selected: ", length(vars_selectionnees), "\n",
      "Selected variables: ", paste(vars_selectionnees, collapse = ", "), "\n"
    )

    cat("\n",
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
        paste0("→ ", paste(strwrap(paste(vars_selectionnees, collapse = ", "), width = 70), collapse = "\n   ")), "\n",
        "=====================================================\n"
    )

    cat("\n")

    repeat {
      save_summary <- tolower(safe_readline("Would you like to save a summary report (.txt)? (yes/no): "))
      if (save_summary %in% c("yes", "no")) break
      cat("Please enter 'yes' or 'no'.\n")
    }

    if (save_summary == "yes") {
      cat("Please select where to save the report.\n")
      file_path_summary <- rstudioapi::selectFile(caption = "Save summary", label = "Save", existing = FALSE)
      if (!is.null(file_path_summary) && file_path_summary != "") {
        if (!grepl("\\.txt$", file_path_summary)) {
          file_path_summary <- paste0(file_path_summary, ".txt")
        }
        writeLines(resume_pca, con = file_path_summary)
        cat(paste0("\nSummary report successfully saved to: ", file_path_summary, "\n"))
      } else {
        cat("No file selected. Summary not saved.\n")
      }
    } else {
      cat("Summary not saved.\n")
    }

    # Sous-ensemble des variables sélectionnées dans le dataframe normalisé
    variables_cluster <- df_normalized[, vars_selectionnees, drop = FALSE]
    variables_cluster <- variables_cluster[, !duplicated(t(variables_cluster))]

    # Lister les dataframes présents dans l'environnement global
    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x)))]

    # Demander à l'utilisateur de choisir le dataframe original
    repeat {
      cat("\nSelect the dataframe to use as the ORIGINAL data (before CLR):\n")
      print(df_list)

      df_name <- safe_readline("Enter the name of the dataframe: ")

      if (df_name %in% df_list) {
        df_original <- get(df_name, envir = .GlobalEnv)
        break
      }

      cat("\nDataframe not found. Try again.\n")
    }

    # Sous-ensemble des variables sélectionnées dans le tableau original
    variables_cluster_original <- df_original[, colnames(variables_cluster), drop = FALSE]

    # Application CLR sur ce sous-ensemble
    variables_cluster <- as.data.frame(variables_cluster_original)

    # À la fin de la fonction, on retourne le CLR transformé

    repeat {
      redo <- tolower(safe_readline("\nWould you like to restart the PCA with other parameters? (yes/no): "))
      if (redo %in% c("yes", "no")) break
      cat("Please enter 'yes' or 'no'.\n")
    }
    if (redo == "no") {
      cat("\n")
      cat("\n‘variables_cluster’ dataframe was created with the most significant variables.\n")
      cat("\n")
      cat("Analysis complete.\n")
      cat("\n")
      break
    } else {
      cat("\nRestarting PCA analysis...\n\n")
    }
  }

  #return(variables_cluster_clr)
  return(variables_cluster)
}
