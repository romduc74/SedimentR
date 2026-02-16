#' Load a CSV or Excel (.xlsx) file into a DataFrame.
#'
#' @param path Full path to the CSV or Excel file.
#' @param sheet (optional) Name or index of the Excel sheet. If not specified, the first sheet is used.
#' @export
charger <- function(path = NULL, sheet = NULL, sep, fileEncoding) {

  # --- Required packages ---
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  library(rstudioapi)
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)


  extension <- tools::file_ext(path)

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }

  if (exists(".my_cache", envir = .GlobalEnv)) {
    clear_cache()
  }



  # --- Select file interactively if not provided ---
  if (is.null(path)) {
    if (!rstudioapi::isAvailable()) stop("RStudio API not available for interactive selection.")
    cat("\nSelect your data file (CSV or XLSX)...\n")
    path <- rstudioapi::selectFile(
      caption = "Select your data file",
      label = "Open",
      path = getwd(),
      filter = list(
        "Data files" = c("csv","xlsx"),
        "CSV files" = "csv",
        "Excel files" = "xlsx"
      ),
      existing = TRUE
    )
    if (!nzchar(path)) {
      cat("\nFile selection cancelled.\n")
      return(invisible(NULL))
    }
  }

  cat("\nSelected file:", basename(path), "\n")
  cat("\n")
  extension <- tolower(tools::file_ext(path))




  if (extension == "csv") {

    if (missing(sep) || is.null(sep)) {
      sep <- safe_readline("Enter separator used in CSV (default ','): ")
      if (sep == "") sep <- ","
    }

    cat("\n")

    if (missing(fileEncoding) || is.null(fileEncoding)) {
      fileEncoding <- safe_readline("Enter file encoding (default 'UTF-8'): ")
      if (fileEncoding == "") fileEncoding <- "UTF-8"
    }

    # --- Lecture du CSV ---
    df <- tryCatch({
      read.csv(path, sep = sep, fileEncoding = fileEncoding,
               stringsAsFactors = FALSE, check.names = FALSE)
    }, error = function(e) {
      stop("Failed to read CSV file: ", e$message)
    })

    # --- Conversion des colonnes numériques (comma → dot) ---
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        col <- gsub(",", ".", col, fixed = TRUE)
        suppressWarnings(as.numeric(col))
      } else col
    })




  } else if (extension == "xlsx") {

    if (is.null(sheet)) {
      sheets <- excel_sheets(path)
      cat("\nAvailable sheets:\n\n")
      print(sheets)
      cat("\n")

      repeat {
        sheet_choice <- safe_readline("Enter sheet name or index to load: ")
        if (sheet_choice %in% sheets) {
          sheet <- sheet_choice
          break
        } else if (suppressWarnings(!is.na(as.numeric(sheet_choice))) &&
                   as.numeric(sheet_choice) %in% seq_along(sheets)) {
          sheet <- as.numeric(sheet_choice)
          break
        } else {
          cat("Invalid sheet. Try again.\n")
        }
      }
    }

    df <- read_excel(path, sheet = sheet)
    df <- as.data.frame(df)



    # Conversion en numérique
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) return(col)
      if (is.factor(col)) col <- as.character(col)
      if (is.character(col)) col <- gsub(",", ".", col, fixed = TRUE)
      suppressWarnings(as.numeric(col))
    })

  } else stop("Unsupported file format. Use .csv or .xlsx.")

  assign("User Dataframe", df, envir=.GlobalEnv)

  # --- Gestion des valeurs manquantes ---
  if (anyNA(df)) {
    cat("\n")
    cat("\nWARNING: The dataframe contains missing values (NA).\n")
    cat("\n")
    cat("Available columns:\n")
    cat("\n")
    print(colnames(df))
    cat("\n")

    exclude_response <- tolower(safe_readline("Do you want to exclude any columns from the NA filtering? (yes/no): "))
    cat("\n")
    excluded_columns <- NULL
    if (exclude_response == "yes") {
      input <- safe_readline("Enter column names to exclude (separated by commas): ")
      excluded_columns <- trimws(unlist(strsplit(input, ",")))
      invalid_columns <- setdiff(excluded_columns, colnames(df))
      if (length(invalid_columns) > 0) stop(paste("Invalid column names:", paste(invalid_columns, collapse = ", ")))
    }
    cat("\n")
    na_response <- tolower(safe_readline("Do you want to delete rows with NA values? (yes/no): "))
    cat("\n")
    if (na_response == "yes") {
      df_tmp <- df
      if (!is.null(excluded_columns)) df_tmp <- df[, !(names(df) %in% excluded_columns), drop = FALSE]
      na_rows <- which(apply(df_tmp, 1, function(x) any(is.na(x))))
      if (length(na_rows) > 0) {
        cat("Rows removed:", paste(na_rows, collapse = ", "), "\n")
        df <- df[complete.cases(df_tmp), ]
      } else {
        cat("\n")
        cat("No rows to remove: no NA values in selected columns.\n")
      }
      cat("\n")
      cat("The dataframe is now cleaned.\n")
    } else {
      cat("\n")
      cat("Rows containing NA values were retained.\n")
    }
  } else {
    cat("\n")
    cat("No missing values found in the dataframe.\n")
  }

  cat("\n")
  cat("\nData loaded successfully.\n")
  cat("\n")



  # --- Vérification et lancement du module XRF noise ---
  run_noise <- safe_readline("Would you like to run the XRF noise detection & denoising module (EEMD-based)? (yes/no): ")
  cat("\n")

  if (tolower(run_noise) == "yes") {

    # Vérifier que la fonction existe
    if (!exists("xrf_noise")) {
      stop("The function 'xrf_noise()' is not available in the current environment. Cannot perform XRF denoising.")
    }

    cat("\nLaunching XRF noise detection...\n\n")

    noise_output <- xrf_noise(df)

    # Mettre à jour les variables locales et globales pour le cache
    df <- noise_output$df_clean
    df_clean <- noise_output$df_clean
    df_denoised_total <- noise_output$df_denoised_total

    cat("\nXRF noise filtering completed.\n")
    cat("\nThe dataframe has been updated with denoised & filtered variables.\n")

    # --- Mettre à jour le cache
    set_cache("df_clean_cache", df_clean)
    set_cache("df_denoised_total_cache", df_denoised_total)
    cat("\n")

  } else {
    cat("XRF noise detection skipped.\n\n")
  }

  cat("\n")

  # --- Bloc création ratios log-transformés ---
  answer <- safe_readline("Would you like to create log-transformed ratios? (yes/no) : ")
  cat("\n")
  if (tolower(answer) == "yes") {

    repeat {

      created_ratios <- list()

      selected <- safe_readline("\nWhat ratios would you like to create? (separate with commas: e.g. Fe/Ti, Ca/K) : ")

      cat("\n")
      cat("\n Ratios examples: Micro-XRF Studies of Sediment Cores (Ian W. Croudace & R. Guy Rothwell 2015, doi:https://doi.org/10.1007/978-94-017-9849-5):\n")
      cat("\n")


      element_candidates <- colnames(df)
      element_names <- element_candidates[!grepl("/", element_candidates) & !grepl("log_", element_candidates)]
      cat("\n Elements available in your dataframe:\n")
      cat("\n")
      print(element_names)

      cat("\n")

      selected_ratios <- trimws(strsplit(selected, ",")[[1]])

      cat("\n")

      for (ratio in selected_ratios) {

        cat(paste0("\nCreation of the log ratio for ", ratio, "\n\n"))
        #
        # numerator   <- safe_readline(paste("Numerator of", ratio, ": "))
        # cat("\n")
        # denominator <- safe_readline(paste("Denominator of", ratio, ": "))

        repeat {
          numerator <- safe_readline(paste("Numerator of", ratio, ": "))
          if (numerator %in% element_candidates) {
            break
          } else {
            cat("\n")
            cat("Invalid entry. Please enter a valid column name from your dataframe.\n")
            cat("\n")
            cat("Available elements:\n")
            cat("\n")
            print(element_candidates)
            cat("\n")
            cat("\nPlease retry.\n\n")
          }
        }

        cat("\n")

        # --- Choix du dénominateur ---
        repeat {
          denominator <- safe_readline(paste("Denominator of", ratio, ": "))
          if (denominator %in% element_candidates) {
            break
          } else {
            cat("\n")
            cat("Invalid entry. Please enter a valid column name from your dataframe.\n")
            cat("\n")
            cat("Available elements:\n")
            cat("\n")
            print(element_candidates)
            cat("\n")
            cat("\nPlease retry.\n\n")
          }
        }

        var_name <- paste0("log_", numerator, "_", denominator)



        # Création dans df
        df[[var_name]] <- ifelse(
          !is.na(df[[numerator]]) &
            !is.na(df[[denominator]]) &
            df[[denominator]] != 0,
          log(df[[numerator]] / df[[denominator]]),
          NA
        )

        # Création dans df_denoised_total si dispo
        if (exists("df_denoised_total", inherits = TRUE)) {
          df_denoised_total[[var_name]] <- ifelse(
            !is.na(df_denoised_total[[numerator]]) &
              !is.na(df_denoised_total[[denominator]]) &
              df_denoised_total[[denominator]] != 0,
            log(df_denoised_total[[numerator]] / df_denoised_total[[denominator]]),
            NA
          )
        }

        if (exists("df_clean", inherits = TRUE)) {
          df_clean[[var_name]] <- ifelse(
            !is.na(df_clean[[numerator]]) &
              !is.na(df_clean[[denominator]]) &
              df_clean[[denominator]] != 0,
            log(df_clean[[numerator]] / df_clean[[denominator]]),
            NA
          )
        }



        created_ratios[[length(created_ratios) + 1]] <- c(
          ratio = ratio,
          numerator = numerator,
          denominator = denominator,
          variable = var_name
        )
      }


      # -------- SUMMARY --------
      cat("\n==================== SUMMARY OF CREATED RATIOS ====================\n\n")
      for (i in seq_along(created_ratios)) {
        r <- created_ratios[[i]]
        cat(sprintf("[%d] %s  ->  log(%s / %s)  =>  %s\n\n",
                    i, r["ratio"], r["numerator"], r["denominator"], r["variable"]))
      }
      cat("==================================================================\n\n")


      confirm <- tolower(safe_readline("Are these ratios correct? (yes/no): "))
      cat("\n")

      if (confirm == "yes") {
        cat("Ratios confirmed and kept.\n")
        break
      } else {
        cat("Ratios will be discarded. Let's try again.\n")

        # Nettoyage des variables créées
        for (r in created_ratios) {
          df[[r["variable"]]] <- NULL
          if (exists("df_denoised_total", inherits = TRUE)) {
            df_denoised_total[[r["variable"]]] <- NULL
          }
        }
      }
    }

  }else {
    cat("No log-transformed ratios were created.\n")
  }
  cat("\n")


  # --- Message avant vérification des valeurs négatives ---
  cat("\n=============================================================================================================\n")
  cat("Next, we will check all numeric columns (excluding log-transformed ratios) for negative values.\n")
  cat("You will have the option to either remove the entire column or only the rows containing negative values.\n")
  cat("This is important before computing CLR or log-transformed ratios, as negative values can cause issues.\n")
  cat("\n=============================================================================================================\n")


  # --- Toujours créer df_clean si pas encore ---
  if (!exists("df_clean", inherits = TRUE)) df_clean <- df

  # --- Colonnes numériques normales (exclure log_) ---
  numeric_cols <- colnames(df_clean)[sapply(df_clean, is.numeric)]
  normal_cols <- setdiff(numeric_cols, grep("^log_", numeric_cols, value = TRUE))

  # --- Pour garder trace des suppressions ---
  removed_columns <- c()
  removed_rows <- list()

  # --- Vérification des négatifs par colonne (pré-stockage) ---
  negative_info <- list()
  for (col_name in normal_cols) {
    neg_rows <- which(df_clean[[col_name]] < 0)
    if (length(neg_rows) > 0) {
      negative_info[[col_name]] <- neg_rows
    }
  }

  # --- Afficher les colonnes contenant des négatifs ---
  if (length(negative_info) > 0) {
    cat("Columns containing negative values:\n\n")
    cat(paste("-", names(negative_info)), sep = "\n")
  } else {
    cat("No negative values detected in numeric columns.\n")
  }

  # --- Suivi des suppressions ---
  removed_columns <- c()
  removed_rows <- list()

  all_removed_rows <- c()

  # --- Demander la colonne depth ---
  depth_col <- NULL
  numeric_candidates <- colnames(df_clean)[sapply(df_clean, is.numeric)]
  if (length(numeric_candidates) > 0) {
    cat("\nAvailable numeric columns (potential depth columns):\n\n")
    print(numeric_candidates)
    cat("\n")
    repeat {
      depth_input <- readline(prompt = "Enter the name of the depth column: ")
      if (depth_input == "" || depth_input %in% colnames(df_clean)) {
        depth_col <- ifelse(depth_input == "", NULL, depth_input)
        break
      }
      cat("Invalid column name. Try again.\n")
    }
  }



  # --- Traitement des colonnes une par une ---
  for (col_name in names(negative_info)) {
    neg_rows <- negative_info[[col_name]]

    repeat {
      cat("\n--------------------------------------------------\n")
      cat(sprintf("Column: %s\n", col_name))
      cat(sprintf("Number of negative values: %d\n", length(neg_rows)))
      cat("Options:\n  [1] Remove the entire column\n  [2] Remove only the rows containing negative values\n")
      cat("--------------------------------------------------\n")

      choice <- readline(prompt="Enter 1 or 2: ")
      if (choice %in% c("1","2")) break
      cat("Invalid input. Please enter 1 or 2.\n")
    }

    if (choice == "1") {
      df[[col_name]] <- NULL
      df_clean[[col_name]] <- NULL
      if (exists("df_denoised_total")) df_denoised_total[[col_name]] <- NULL
      removed_columns <- c(removed_columns, col_name)
      cat(sprintf("Column '%s' removed.\n", col_name))

    } else {
      # --- Stocker toutes les lignes avant suppression ---
      new_rows <- setdiff(neg_rows, all_removed_rows)
      already_removed <- intersect(neg_rows, all_removed_rows)

      if (!is.null(depth_col)) {
        removed_rows[[col_name]] <- data.frame(
          row_index = neg_rows,
          depth = df[[depth_col]][neg_rows]
        )
      } else {
        removed_rows[[col_name]] <- data.frame(
          row_index = neg_rows
        )
      }

      # Supprimer les nouvelles lignes
      if (length(new_rows) > 0) {
        df <- df[-new_rows, , drop = FALSE]
        df_clean <- df_clean[-new_rows, , drop = FALSE]
        if (exists("df_denoised_total")) df_denoised_total <- df_denoised_total[-new_rows, , drop = FALSE]
      }

      all_removed_rows <- union(all_removed_rows, neg_rows)

      cat(sprintf("%d rows removed from '%s'\n", length(new_rows), col_name))
      if (length(already_removed) > 0) {
        cat(sprintf("  Note: %d rows also have negative values in other columns.\n", length(already_removed)))
      }
    }
  }





  # --- Colonnes log_ uniquement ---
  log_cols <- grep("^log_", colnames(df_clean), value = TRUE)
  na_removed <- list()

  if (length(log_cols) > 0) {
    for (col_name in log_cols) {
      na_rows <- which(is.na(df_clean[[col_name]]))
      if (length(na_rows) > 0) {
        cat(sprintf("\nWARNING: Column '%s' contains %d NA values (log of 0/negative)\n",
                    col_name, length(na_rows)))

        # Stocker les infos avant suppression
        if (!is.null(depth_col)) {
          na_removed[[col_name]] <- data.frame(
            row_index = na_rows,
            depth = df[[depth_col]][na_rows]
          )
        } else {
          na_removed[[col_name]] <- data.frame(
            row_index = na_rows
          )
        }

        # Supprimer les lignes
        df <- df[-na_rows, , drop = FALSE]
        df_clean <- df_clean[-na_rows, , drop = FALSE]
        if (exists("df_denoised_total")) df_denoised_total <- df_denoised_total[-na_rows, , drop = FALSE]

        cat(sprintf("%d rows removed due to NA in '%s'\n", length(na_rows), col_name))
      }
    }
    if (length(na_removed) == 0) {
      cat("\nNo NA values detected in log_ columns. Nothing to remove.\n")
    }
  } else {
    cat("\nNo log_ columns detected.\n")
  }

  # --- Résumé final ---
  # cat("\n==================== CLEANING SUMMARY ====================\n\n")
  #
  # # Colonnes supprimées
  # if (length(removed_columns) > 0) {
  #   cat("Columns removed due to negative values:\n\n")
  #   cat(paste("-", removed_columns), sep = "\n")
  #   cat("\n")
  # } else {
  #   cat("No columns removed.\n\n")
  # }
  #
  # # Lignes supprimées pour valeurs négatives
  # if (length(removed_rows) > 0) {
  #   cat("Rows removed due to negative values (per column, with depth if available):\n\n")
  #   for (col in names(removed_rows)) {
  #     info <- removed_rows[[col]]
  #     if (!is.null(depth_col) && "depth" %in% colnames(info)) {
  #       depth_info <- paste(info$depth, collapse = ", ")
  #       cat(sprintf("  Column '%s': %d rows removed → depths: %s\n",
  #                   col, nrow(info), depth_info))
  #     } else {
  #       cat(sprintf("  Column '%s': %d rows removed\n", col, nrow(info)))
  #     }
  #   }
  #   cat("\n")
  # } else {
  #   cat("No rows removed due to negative values.\n\n")
  # }
  cat("\n==================== CLEANING SUMMARY ====================\n\n")

  if (length(removed_columns) > 0) {
    cat("Columns removed due to negative values:\n\n")
    cat(paste("-", removed_columns), sep = "\n")
    cat("\n")
    # --- Total des colonnes supprimées ---
    cat(sprintf("Total columns removed: %d\n\n", length(removed_columns)))
  } else {
    cat("No columns removed.\n\n")
  }

  cat("-----------------------------------------------------------------\n\n")

  if (length(removed_rows) > 0) {

    cat("Depths of rows removed due to negative values:\n\n")

    # Fusionner toutes les profondeurs supprimées
    all_depths <- c()

    for (col in names(removed_rows)) {
      info <- removed_rows[[col]]

      if (!is.null(depth_col) && "depth" %in% colnames(info)) {
        valid_depths <- info$depth[!is.na(info$depth)]
        all_depths <- c(all_depths, valid_depths)
      }
    }

    # Supprimer doublons
    all_depths <- unique(all_depths)

    if (length(all_depths) > 0) {
      depth_info <- paste(all_depths, collapse = ", ")
      cat(sprintf("Total rows removed: %d\n", length(all_depths)))
      cat(sprintf("Depths removed → %s\n", depth_info))
    } else {
      cat("No valid depths recorded.\n")
    }

    cat("\n=========================================================\n\n")

  } else {
    cat("No rows removed due to negative values.\n\n")
  }



  # --- Save globally & optional caching ---
  #assign("df_clean", df, envir=.GlobalEnv)


  if (exists("df_clean", inherits = TRUE)) {
    # df_denoised_total existe → prendre les colonnes low-noise dans la version débruitée
    set_cache("df_clean_cache", df_clean)
  } else {
    # Pas de débruitage → utiliser df_clean tel quel
    set_cache("df_clean_cache", df)
  }



  # --- Vérification que df_denoised_total existe ---
  if (exists("df_denoised_total") && "depth" %in% colnames(df)) {

    # Créer un dataframe avec la colonne depth conservée
    df_denoised_full <- df_denoised_total  # commence avec les colonnes déjà débruitées
    df_denoised_full <- df_denoised_full[, colnames(df_denoised_total), drop = FALSE]

    # Ajouter les colonnes de df (original ou df après xrf_noise) qui ne sont pas dans df_denoised_total
    cols_to_add <- setdiff(colnames(df), c("depth", colnames(df_denoised_total)))

    if (length(cols_to_add) > 0) {
      for (col in cols_to_add) {
        df_denoised_full[[paste0(col, "_denoised")]] <- df[[col]]
      }
    }

    # Ajouter la colonne depth en premier
    df_denoised_full <- df_denoised_full[, c("depth", setdiff(colnames(df_denoised_full), "depth"))]



    # Assigner dans l'environnement global
    assign("Denoised Dataframe", df_denoised_full, envir = .GlobalEnv)
    set_cache("df_denoised_total_cache", df_denoised_full)

  }

  # --- Post-traitement du Denoised Dataframe ---
  if (exists("Denoised Dataframe", envir = .GlobalEnv)) {

    df_denoised_export <- get("Denoised Dataframe", envir = .GlobalEnv)

    # --- Supprimer la colonne depth choisie par l'utilisateur ---
    if (!is.null(depth_col) && depth_col %in% colnames(df_denoised_export)) {
      df_denoised_export[[depth_col]] <- NULL
    }

    # --- Ajouter suffixe _denoised à toutes les colonnes restantes ---
    colnames(df_denoised_export) <- paste0(colnames(df_denoised_export), "_denoised")

    # --- Mise en cache ---
    set_cache("Dataframe_Denoised_cache", df_denoised_export)
  }


}

