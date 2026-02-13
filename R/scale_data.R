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
  cat("\n")
  cat("\033[4mIt is advisable to remove depth elements measured at different energies, Incoherent & Coherent and retain the least noisy elements.\033[0m\n\n")

  cat("Available columns:\n")
  cat("\n")
  print(names(donnees))
  cat("\n")

  # Sauvegarde des colonnes initiales
  colonnes_initiales <- names(donnees)
  colonnes_supprimees <- character(0)


  # -------------------------------
  # Suppression obligatoire de la colonne profondeur
  # -------------------------------
  repeat {
    col_profondeur <- safe_readline("Enter the name of the depth column to remove: ")

    if (!col_profondeur %in% names(donnees)) {
      cat("\nError: column not found. Please choose among:\n")
      # print(names(donnees))
    } else {
      depth_col <- donnees[[col_profondeur]]
      donnees <- donnees[, !(names(donnees) %in% col_profondeur)]
      colonnes_supprimees <- c(colonnes_supprimees, col_profondeur)
      cat("\nDepth column removed: ", col_profondeur, "\n")
      break
    }
  }

  cat("\n")


  cat("\n====== POTENTIAL XRF DUPLICATES (BY NAME) ======\n")
  cat("\n")
  col_names <- names(donnees)
  elements  <- sub("([A-Za-z]+).*", "\\1", col_names)
  dup_elements <- elements[duplicated(elements) | duplicated(elements, fromLast = TRUE)]

  if (length(dup_elements) > 0) {
    unique_elems <- unique(dup_elements)
    for (el in unique_elems) {
      cols <- col_names[elements == el]
      cat("\n- Element", el, "measured multiple times:", paste(cols, collapse = ", "), "\n")
    }
    cat("\n")

    # Demander suppression immédiate
    repeat {
      rep_dup <- tolower(safe_readline(
        "Do you want to remove some of these duplicate XRF columns now? (yes/no): "
      ))
      cat("\n")
      if (!rep_dup %in% c("yes", "no")) next

      if (rep_dup == "yes") {
        cols_dup <- safe_readline("Enter duplicate column names to remove (comma-separated): ")
        cols_dup <- trimws(strsplit(cols_dup, ",")[[1]])

        valid_cols   <- intersect(cols_dup, names(donnees))
        invalid_cols <- setdiff(cols_dup, names(donnees))

        if (length(valid_cols) > 0) {
          donnees <- donnees[, !(names(donnees) %in% valid_cols)]
          colonnes_supprimees <- c(colonnes_supprimees, valid_cols)

          cat("\nRemoved duplicate XRF columns:\n",
              paste(valid_cols, collapse = ", "), "\n")
        }

        if (length(invalid_cols) > 0) {
          cat("\nIgnored (not found):", paste(invalid_cols, collapse = ", "), "\n")
        }
      }

      break
    }

  } else {
    cat("\n")
    cat("No duplicated XRF element names detected.\n")
  }


  cat("\n")




  ## ===== MAINTENANT : suppression libre d’autres colonnes =====
  repeat {
    rep2 <- tolower(safe_readline("Do you want to remove other columns before the analysis? (yes/no): "))
    if (!rep2 %in% c("yes", "no")) next

    if (rep2 == "no") break

    cat("\n====== AVAILABLE COLUMNS ======\n")
    cat("\n")
    print(names(donnees))
    cat("\n")

    colonnes_a_supprimer <- safe_readline(
      "Enter the names of the columns to be deleted, separated by commas: "
    )
    colonnes_a_supprimer <- trimws(strsplit(colonnes_a_supprimer, ",")[[1]])

    colonnes_existantes <- intersect(colonnes_a_supprimer, names(donnees))
    colonnes_invalides  <- setdiff(colonnes_a_supprimer, names(donnees))

    if (length(colonnes_existantes) == 0) {
      cat("\nError: none of the specified columns exist. Try again.\n")
      next
    }

    if (length(colonnes_invalides) > 0) {
      cat("\nWarning: ignored:", paste(colonnes_invalides, collapse = ", "), "\n")
    }

    donnees <- donnees[, !(names(donnees) %in% colonnes_existantes)]
    colonnes_supprimees <- c(colonnes_supprimees, colonnes_existantes)

    cat("\nThe following columns have been removed:\n",
        paste(colonnes_existantes, collapse = ", "), "\n")

    break
  }




  if (ncol(donnees) == 0) {
    stop("The dataframe is empty after deleting the columns. Make sure you don't delete all the columns.")
  }

  # Normalisation
  cat("\n====== INFORMATION ======\n")
  cat("\n")
  cat("The following steps will be performed:\n")
  cat("1. Columns containing 'log' Ratios will be left unchanged.\n")
  cat("2. All other columns will be transformed using the CLR (centered log-ratio) transformation.\n")
  cat("3. The resulting dataframe will combine clr-transformed columns and 'log' columns.\n\n")

  colonnes_log <- donnees[, grepl("log", names(donnees)), drop = FALSE]
  colonnes_a_clr <- donnees[, !grepl("log", names(donnees)), drop = FALSE]

  geom_mean <- apply(colonnes_a_clr, 1, function(x) exp(mean(log(x))))

  df_clr <- clr(colonnes_a_clr)
  df_clr <- as.data.frame(df_clr)

  df_normalized <- cbind(
    #depth = depth_col,
    df_clr,
    colonnes_log
  )
#
#   df_normalized3 <- cbind(
#     depth = depth_col,
#     df_clr,
#     colonnes_log
#   )

  set_cache("df_normalized_cache", df_normalized)

  #set_cache("df_clean_cache", df_normalized3)

  df_clr2<-df_clr

  names(df_clr2) <- paste0("clr_", names(df_clr2))

  df_normalized2 <- cbind(df_clr2, colonnes_log)

  df_normalized_depth <- cbind(
    depth = depth_col,
    "Geometric Mean" = geom_mean,
    colonnes_a_clr,
    df_normalized2
  )



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
    "\nTotal selected columnsa: ", total_selected,
    "\n\n===============================\n"
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
    cat("\n")
  }

  # -------------------------------
  # Sauvegarde optionnelle des données normalisées
  # -------------------------------
  repeat {
    save_data <- tolower(safe_readline("Would you like to save the normalized data (CLR-transformed)? (yes/no): "))
    cat("\n")
    if (save_data %in% c("yes", "no")) break
    cat("Please enter 'yes' or 'no'.\n\n")
  }

  if (save_data == "yes") {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
    library(rstudioapi)

    repeat {
      ext_choice <- tolower(safe_readline("Desired file extension: csv or xlsx: "))
      cat("\n")
      if (!ext_choice %in% c("csv", "xlsx")) {
        cat("Invalid extension. Please choose 'csv' or 'xlsx'.\n\n")
        next
      }

      if (!rstudioapi::isAvailable()) {
        cat("Saving requires RStudio API. Cannot select file interactively.\n\n")
        break
      }

      cat("Please select the destination file...\n")
      path_save <- rstudioapi::selectFile(
        caption = "Save normalized data",
        label   = "Save",
        path    = getwd(),
        filter  = if (ext_choice == "csv") list("CSV files" = "csv") else list("Excel files" = "xlsx"),
        existing = FALSE
      )

      if (!nzchar(path_save)) {
        cat("Saving cancelled.\n\n")
        break
      }

      # Ajout automatique de l'extension si nécessaire
      if (!grepl(paste0("\\.", ext_choice, "$"), path_save, ignore.case = TRUE)) {
        path_save <- paste0(path_save, ".", ext_choice)
      }

      # Écriture selon le format choisi
      if (ext_choice == "csv") {
        write.csv(df_normalized, path_save, row.names = FALSE)
      } else {
        if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
        writexl::write_xlsx(df_normalized_depth, path = path_save)
      }

      cat("\nNormalized data successfully saved to:", path_save, "\n\n")
      break
    }
  } else {
    cat("Normalized data not saved.\n\n")
  }

  assign("df_clr_check", df_normalized_depth, envir = .GlobalEnv)


  return(invisible(df_normalized))
}
