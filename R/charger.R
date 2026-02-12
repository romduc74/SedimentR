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



  cat("\n")
  run_noise <- safe_readline("Would you like to run the XRF noise detection & denoising module (EEMD-based)? (yes/no): ")
  cat("\n")


  if (tolower(run_noise) == "yes") {

    if (!exists("xrf_noise")) {
      stop("The function 'xrf_noise()' is not available in the current environment.")
    }
    cat("\nLaunching XRF noise detection...\n\n")

    noise_output <- xrf_noise(df)

    df <- noise_output$df_clean

    cat("\nXRF noise filtering completed.\n")
    cat("\n")
    cat("The dataframe has been updated with denoised & filtered variables.\n\n")

  } else {
    cat("XRF noise detection skipped.\n\n")
  }

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
        cat("Ratios confirmed and kept.\n\n")
        break
      } else {
        cat("Ratios will be discarded. Let's try again.\n\n")

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
  cat("\n")




  # --- CHECK FOR NEGATIVE VALUES AFTER DENOISING / RATIOS ---
  cat("=== CHECK FOR NEGATIVE VALUES IN THE DATAFRAME ===\n\n")

  # Choose which dataframe to check: denoised if exists, otherwise final df
  df_to_check <- if (exists("df_denoised_total")) df_denoised_total else df

  # Identify negative values
  neg_counts <- sapply(df_to_check, function(col) sum(col < 0, na.rm = TRUE))
  cols_with_neg <- names(neg_counts)[neg_counts > 0]

  if (length(cols_with_neg) == 0) {
    cat("No negative values detected in the dataframe.\n\n")
  } else {

    cat("The following columns contain negative values, with the number of affected rows:\n\n")
    for (col in cols_with_neg) {
      cat(sprintf("- %s : %d rows\n", col, neg_counts[col]))
    }

    cat("\n")
    cat("Negative values typically indicate elements that were poorly measured during XRF analysis.\n")
    cat("It is recommended to remove these values before CLR (Centered Log-Ratio) normalization.\n")
    cat("If negative or zero values remain, the geometric mean cannot be calculated properly.\n\n")

    # ------------------------
    # Cas par cas
    # ------------------------
    for (col in cols_with_neg) {
      repeat {
        cat("\n")  # espace avant chaque colonne
        cat(sprintf("Column '%s' has %d negative values.\n", col, neg_counts[col]))
        cat("\n")

        action <- tolower(readline(
          paste0(
            "Choose action for this column:\n\n",
            "  1) Remove rows containing negative values\n",
            "  2) Remove the entire column\n\n",
            "Your choice (1/2): "
          )
        ))
        cat("\n")

        if (action %in% c("1", "remove rows")) {
          # Remove rows where this column is negative
          rows_to_remove <- which(df_to_check[[col]] < 0)
          cat(sprintf("\nRemoving %d rows in column '%s'...\n\n", length(rows_to_remove), col))
          if (length(rows_to_remove) > 0) {
            df_to_check <- df_to_check[-rows_to_remove, , drop = FALSE]
          }
          break
        } else if (action %in% c("2", "remove column")) {
          # Remove entire column
          cat(sprintf("\nRemoving column '%s'...\n\n", col))
          df_to_check <- df_to_check[, !(names(df_to_check) %in% col), drop = FALSE]
          break
        } else {
          cat("\nInvalid choice. Please enter 1 or 2.\n\n")
        }
      }
      cat("\n")  # espace après chaque colonne
    }


    # Assign cleaned dataframe back to global environment
    if (exists("df_denoised_total")) {
      df_denoised_total <- df_to_check
      assign("df_denoised_total", df_denoised_total, envir = .GlobalEnv)
      set_cache("df_denoised_total_cache", df_denoised_total)
    } else {
      df <- df_to_check
      assign("df_clean", df, envir = .GlobalEnv)
      set_cache("df_clean_cache", df)
    }

    cat("Negative value handling completed.\n\n")
  }

  cat("\n")

  # ---- SUMMARY TABLE FOR NEGATIVE VALUE HANDLING ----
  if (length(cols_with_neg) > 0) {

    summary_neg <- data.frame(
      Column = cols_with_neg,
      Negative_Rows = neg_counts[cols_with_neg],
      Action_Taken = NA_character_,
      stringsAsFactors = FALSE
    )

    # Fill in the actions taken
    for (i in seq_along(cols_with_neg)) {
      col <- cols_with_neg[i]
      if (!col %in% names(df_to_check)) {
        summary_neg$Action_Taken[i] <- "Column removed"
        summary_neg$Negative_Rows[i] <- neg_counts[col]  # keep original count
      } else {
        # Count how many rows were removed for this column
        removed_rows <- neg_counts[col] - sum(df_to_check[[col]] < 0, na.rm = TRUE)
        summary_neg$Action_Taken[i] <- paste0("Removed ", removed_rows, " rows")
        summary_neg$Negative_Rows[i] <- neg_counts[col]
      }
    }

    # Print a nicely formatted table
    cat("\n====== SUMMARY OF NEGATIVE VALUE HANDLING ======\n\n")
    print(summary_neg, row.names = FALSE)
    cat("\n================================================\n\n")
  }






  # --- Save globally & optional caching ---
  assign("df_clean", df, envir=.GlobalEnv)
  set_cache("df_clean_cache", df_clean)

  if (exists("df_denoised_total")){
    assign("df_denoised_total", df_denoised_total, envir=.GlobalEnv)
    set_cache("df_denoised_total_cache", df_denoised_total)
  }



}
