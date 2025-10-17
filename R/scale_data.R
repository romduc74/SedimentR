#' @title Sélection, normalisation et exportation des choix utilisateurs
#' @param donnees Un dataframe contenant les données à normaliser.
#' @return Un dataframe avec les données normalisées après suppression des colonnes spécifiées.
#' @export
scale_data <- function(donnees=NULL) {

  if (!requireNamespace("compositions", quietly = TRUE)) install.packages("compositions")
  library(compositions)

  # Fonction utilitaire
  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }



  repeat {
    # Check if df_normal is already in cache
    if ("df_clean_cache" %in% list_cache()) {
      donnees <- get_cache("df_clean_cache")
      message("Using cached 'df_clean_cache' dataframe.")
      break
    }

    # Otherwise, show data frames available in the global environment
    df_list <- ls(envir = .GlobalEnv)
    df_list <- df_list[sapply(df_list, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

    if (length(df_list) == 0) {
      stop("No data frames found in the global environment.")
    }

    cat("\nSelect the denoised dataframe :\n")
    print(df_list)
    cat("\n")

    df_name <- readline("Enter the name of the dataframe: ")

    # If a valid dataframe name is entered
    if (df_name %in% df_list) {
      donnees <- get(df_name, envir = .GlobalEnv)
      set_cache("df_clean_cache", donnees)
      #message("Dataframe cached as 'df_normal' for future use.")
      break
    }

    # Otherwise, try again
    cat("\n Dataframe not found. Try again.\n")
  }


  cat("\n====== WARNING ======\n")
  cat("\033[4mIt is advisable to remove depth elements measured at different energies, Incoherent & Coherent and retain the least noisy elements.\033[0m\n\n")

  cat("Available columns:\n")
  print(names(donnees))
  cat("\n")

  # Sauvegarde des colonnes initiales
  colonnes_initiales <- names(donnees)
  colonnes_supprimees <- character(0)

  # Suppression éventuelle de colonnes
  reponse <- safe_readline(prompt = "Would you like to delete certain columns before the analysis? (yes/no): ")
  cat("\n")

  if (tolower(reponse) == "yes") {
    colonnes_a_supprimer <- safe_readline(prompt = "Enter the names of the columns to be deleted, separated by commas: ")
    colonnes_a_supprimer <- strsplit(colonnes_a_supprimer, ",")[[1]]
    colonnes_a_supprimer <- trimws(colonnes_a_supprimer)

    colonnes_existantes <- intersect(colonnes_a_supprimer, names(donnees))
    if (length(colonnes_existantes) > 0) {
      donnees <- donnees[, !(names(donnees) %in% colonnes_existantes)]
      colonnes_supprimees <- colonnes_existantes
      cat("\nThe subsequent columns have been removed: ", paste(colonnes_existantes, collapse = ", "), "\n")
    } else {
      cat("\nNone of the specified columns exist in the dataframe.\n")
    }
  } else {
    cat("No columns will be deleted.\n")
  }

  if (ncol(donnees) == 0) {
    stop("The dataframe is empty after deleting the columns. Make sure you don't delete all the columns.")
  }

  # Normalisation
  cat("\n====== INFORMATION ======\n")
  cat("The following steps will be performed:\n")
  cat("1. Columns containing 'log' Ratios will be left unchanged.\n")
  cat("2. All other columns will be transformed using the CLR (centered log-ratio) transformation.\n")
  cat("3. The resulting dataframe will combine clr-transformed columns and 'log' columns.\n\n")

  colonnes_log <- donnees[, grepl("log", names(donnees)), drop = FALSE]
  colonnes_a_clr <- donnees[, !grepl("log", names(donnees)), drop = FALSE]

  df_clr <- clr(colonnes_a_clr)
  df_clr <- as.data.frame(df_clr)

  df_normalized <- cbind(df_clr, colonnes_log)
  set_cache("df_normalized_cache",df_normalized)
  # -------------------------------
  # Résumé des choix utilisateur
  # -------------------------------
  total_initial <- length(colonnes_initiales)
  total_selected <- ncol(df_normalized)

  resume_txt <- paste0(
    "====== SUMMARY OF CHOICES ======\n",
    "\nInitial Columns (", total_initial, "):\n", paste(colonnes_initiales, collapse = ", "),
    "\n\nDeleted Columns:\n", ifelse(length(colonnes_supprimees) > 0, paste(colonnes_supprimees, collapse = ", "), "None"),
    "\n\nCLR Transformed Columns:\n", ifelse(ncol(df_clr) > 0, paste(names(df_clr), collapse = ", "), "None"),
    "\n\nLog Columns Retained:\n", ifelse(ncol(colonnes_log) > 0, paste(names(colonnes_log), collapse = ", "), "None"),
    "\n\n--------------------------------\n",
    "Total initial columns: ", total_initial,
    "\nTotal selected columns for PCA: ", total_selected,
    "\n===============================\n"
  )

  # Affichage dans la console
  cat("\n", resume_txt, "\n")

  # -------------------------------
  # Export optionnel en .txt
  # -------------------------------
  repeat {
    save_summary <- tolower(safe_readline("Would you like to save a summary report (.txt)? (yes/no): "))
    cat("\n")
    if (save_summary %in% c("yes", "no")) break
    cat("Please enter 'yes' or 'no'.\n")
  }

  if (save_summary == "yes") {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
    library(rstudioapi)

    cat("Please select where to save the report.\n")
    file_path_summary <- rstudioapi::selectFile(caption = "Save summary", label = "Save", existing = FALSE)

    if (!is.null(file_path_summary) && file_path_summary != "") {
      if (!grepl("\\.txt$", file_path_summary)) {
        file_path_summary <- paste0(file_path_summary, ".txt")
      }
      writeLines(resume_txt, con = file_path_summary)
      cat(paste0("\nSummary report successfully saved to: ", file_path_summary, "\n"))
    } else {
      cat("No file selected. Summary not saved.\n")
    }
  } else {
    cat("Summary not saved.\n")
  }
  assign("df_clean_normalized", df_normalized, envir = .GlobalEnv)
  return(invisible(df_normalized))
}
