#' @title Automated Processing of Multi-Energy XRF Data (10 kV / 30 kV)
#'
#' @description This function reads an `.xlsx` file containing the sheets "10 kV" and "30 kV",
#' extracts the relevant columns, calculates relative depths, merges the data,
#' computes predefined or custom log-ratios, and exports the results.
#'
#' Useful function to prepare a dataset before exploratory analysis or clustering.
#'
#' @param chemin Path to the .xlsx file to be processed.
#' @param export_choix Indicates whether the user wants to export the results ("yes" or "no"). Default: "no".
#' @param format_choisi Desired export format: "csv" or "xlsx".
#' @param dossier Destination folder for the exported file.
#' @param nom_fichier Name of the exported file (without extension).
#'
#' @return No object is directly returned, but the final table is assigned to the global environment under the name `XRF`.
#' @export

xrf_traitement <- function(chemin, export_choix = "no", format_choisi = "csv", dossier = NULL, nom_fichier = NULL) {
  # Required packages
  required_packages <- c("dplyr", "readxl", "stringr", "tibble")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste0("Installing missing package: ", pkg, "\n"))
      install.packages(pkg)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }

  cat("All required packages are ready.\n\n")

  # Check if the file exists
  if (!file.exists(chemin)) stop("The file does not exist.")
  if (!grepl("\\.xlsx$", chemin)) stop("The file must be in .xlsx format.")

  # Check for required sheets
  sheets <- readxl::excel_sheets(chemin)
  required_sheets <- c("10 kV", "30 kV")
  if (!all(required_sheets %in% sheets)) {
    stop("The file must contain the sheets: '10 kV' and '30 kV'.")
  }

  # Read, filter, and process one sheet
  read_filter_process <- function(sheet_name, suffix) {
    cat(paste0("Reading sheet: ", sheet_name, "\n"))
    df <- readxl::read_excel(chemin, sheet = sheet_name)

    # Select columns, including Rh-Ka-Inc Area and Rh-Ka-Coh Area
    selected_cols <- names(df)[
      names(df) == "CoreDepth" |
        grepl("-Ka Area", names(df)) |
        names(df) %in% c("Rh-Ka-Inc Area", "Rh-Ka-Coh Area")
    ]
    df_filtered <- df[, selected_cols]

    # Rename "-Ka Area" or " Area" columns with suffix
    col_names <- names(df_filtered)
    col_names <- ifelse(
      grepl("-Ka Area$", col_names),
      stringr::str_replace(col_names, "-Ka Area$", paste0("_", suffix)),
      ifelse(
        grepl(" Area$", col_names),
        stringr::str_replace(col_names, " Area$", paste0("_", suffix)),
        col_names
      )
    )
    names(df_filtered) <- col_names

    # Convert and create depth column
    if (!is.numeric(df_filtered$CoreDepth)) {
      df_filtered$CoreDepth <- as.numeric(df_filtered$CoreDepth)
    }

    first_value <- df_filtered$CoreDepth[1]
    df_filtered <- df_filtered %>%
      dplyr::mutate(depth = CoreDepth - first_value)

    # Remove rows with only NAs
    df_filtered <- df_filtered[complete.cases(df_filtered), ]

    return(df_filtered)
  }

  # Process both sheets
  data_10kv <- read_filter_process("10 kV", "10")
  data_30kv <- read_filter_process("30 kV", "30")

  # Make dataframes available in the global environment
  assign("data_10kv", data_10kv, envir = .GlobalEnv)
  assign("data_30kv", data_30kv, envir = .GlobalEnv)
  cat("Sheets '10 kV' and '30 kV' are now available in the Global Environment.\n\n")

  # Check depth alignment
  cat("Checking alignment of 'depth' columns...\n")

  if (length(data_10kv$depth) != length(data_30kv$depth)) {
    cat("The lengths of the 'depth' columns are different.\n")
    cat(paste0("10 kV: ", length(data_10kv$depth), " rows\n"))
    cat(paste0("30 kV: ", length(data_30kv$depth), " rows\n"))
  } else {
    differences <- which(data_10kv$depth != data_30kv$depth | is.na(data_10kv$depth) | is.na(data_30kv$depth))
    if (length(differences) == 0) {
      cat("The 'depth' columns are perfectly aligned.\n\n")
    } else {
      cat("Differences were found in the 'depth' columns at the following rows:\n")
      print(differences)
      comparison <- tibble::tibble(
        row = differences,
        depth_10kv = data_10kv$depth[differences],
        depth_30kv = data_30kv$depth[differences]
      )
      print(comparison)
    }
  }

  # Merge
  cat("Merging the two sheets by 'depth'...\n")
  data_final <- dplyr::full_join(data_10kv, data_30kv, by = "depth", suffix = c("_10kv", "_30kv"))
  cat("Merge completed.\n\n")

  # Average by depth (remove triplicates)
  cat("Averaging triplicates by 'depth'...\n")
  data_final <- data_final %>%
    dplyr::group_by(depth) %>%
    dplyr::summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

  cat("Averaging completed.\n\n")

  # Compute main ratios
  cat("Creating new columns with log ratios:\n")
  cat("Automatically creating Ca/Ti, Inco/Co, Zr/Rb (organic matter and grain size)...\n\n")
  data_final <- data_final %>%
    dplyr::mutate(
      log_Rh_Inc_Coh_30 = ifelse(!is.na(`Rh-Ka-Inc_30`) & !is.na(`Rh-Ka-Coh_30`) & `Rh-Ka-Coh_30` != 0,
                                 log(`Rh-Ka-Inc_30` / `Rh-Ka-Coh_30`), NA),
      log_Zr_Rb_30     = ifelse(!is.na(Zr_30) & !is.na(Rb_30) & Rb_30 != 0,
                                log(Zr_30 / Rb_30), NA),
      log_Ca_Ti_10     = ifelse(!is.na(Ca_10) & !is.na(Ti_10) & Ti_10 != 0,
                                log(Ca_10 / Ti_10), NA)
    )

  # Custom ratios
  more_ratios <- readline("Do you want to add other custom log-ratios? (yes/no): ")

  if (tolower(more_ratios) %in% c("yes", "y", "oui", "o")) {
    cat("Enter your custom ratios (e.g., Fe_30/Mn_30), type 'stop' to finish.\n\n")
    repeat {
      ratio <- readline("New ratio (or 'stop'): ")
      if (tolower(ratio) == "stop") break
      if (grepl("/", ratio)) {
        parts <- strsplit(ratio, "/")[[1]]
        num <- trimws(parts[1])
        denom <- trimws(parts[2])
        col_name <- paste0("log_", num, "_", denom)
        if (!(num %in% names(data_final))) {
          cat(paste0("Column not found: ", num, "\n")); next
        }
        if (!(denom %in% names(data_final))) {
          cat(paste0("Column not found: ", denom, "\n")); next
        }
        data_final[[col_name]] <- ifelse(
          !is.na(data_final[[num]]) & !is.na(data_final[[denom]]) & data_final[[denom]] != 0,
          log(data_final[[num]] / data_final[[denom]]),
          NA
        )
        cat(paste0("Ratio added: ", col_name, "\n"))
      } else {
        cat("Incorrect format. Expected example: Fe_30/Mn_30\n")
      }
    }
  } else {
    cat("Only the 3 main log-ratios were added.\n\n")
  }

  # Remove unwanted columns
  cat("Removing columns containing 'CoreDepth' and 'Std'...\n")
  data_final <- data_final %>%
    dplyr::select(-contains("CoreDepth"), -contains("Std"))

  cat("Columns 'CoreDepth' and 'Std' removed.\n\n")

  # Export
  if (tolower(export_choix) %in% c("yes", "y", "oui", "o")) {
    if (!(format_choisi %in% c("csv", "xlsx"))) stop("Unrecognized format.")
    if (is.null(dossier)) dossier <- readline("Destination folder: ")
    if (!dir.exists(dossier)) stop("Folder does not exist.")
    if (is.null(nom_fichier)) nom_fichier <- readline("File name (without extension): ")
    export_path <- file.path(dossier, paste0(nom_fichier, ".", format_choisi))
    if (format_choisi == "csv") {
      write.csv(data_final, export_path, row.names = FALSE)
    } else {
      if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
      writexl::write_xlsx(list(XRF = data_final), path = export_path)
    }
    cat("File successfully exported to: ", export_path, "\n\n")
  } else {
    cat("No export selected.\n")
  }

  # Assign to global
  assign("XRF", data_final, envir = .GlobalEnv)
  cat("In addition to saving, the dataframe is ready for clustering.\n")
  cat("The dataframe 'XRF' is now available in the global environment.\n")

  # Clustering
  cluster <- readline("Do you want to proceed with clustering now? (yes/no): ")
  if (tolower(cluster) %in% c("yes", "y", "oui", "o")) {
    cat("\nPlease use the {sedimentR} package with the dataframe XRF.\n")
    cat("Or run the `charger()` function later by providing the file path to start.\n")
  }
}
