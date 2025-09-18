#' Load a CSV or Excel (.xlsx) file into a DataFrame with SNR filtering
#'
#' This function reads a .csv or .xlsx file. For Excel files, you can specify a sheet
#' by name or index. It calculates the SNR for numeric columns assuming Poisson counting
#' statistics, and allows filtering by a user-defined SNR threshold, with optional manual
#' removal of elements.
#'
#' @param path Full path to the CSV or Excel file.
#' @param sheet (optional) Name or index of the Excel sheet. If not specified, the first sheet is used.
#' @export
charger <- function(path, sheet = NULL, sep, fileEncoding) {
  extension <- tools::file_ext(path)

  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User has interrupted execution via 'exit'.")
    return(input)
  }

  # --- Read CSV or Excel ---
  if (extension == "csv") {
    df <- tryCatch({
      read.csv(path, sep = sep, fileEncoding = fileEncoding, stringsAsFactors = FALSE, check.names = FALSE)
    }, error = function(e) {
      read.csv(path, sep = sep, fileEncoding = fileEncoding, stringsAsFactors = FALSE, check.names = FALSE)
    })

    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        col <- gsub(",", ".", col, fixed = TRUE)
        suppressWarnings(as.numeric(col))
      } else col
    })

  } else if (extension == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' required for Excel files.")
    if (is.null(sheet)) sheet <- readxl::excel_sheets(path)[1]
    df <- readxl::read_excel(path, sheet = sheet)
    df <- as.data.frame(df)
  } else stop("Unsupported file format. Use .csv or .xlsx.")
  cat("\n")
  cat("\nData loaded successfully.\n")
  cat("\n")

  # --- Bloc création ratios log-transformés ---
  answer <- safe_readline("Would you like to create log-transformed ratios? (yes/no) : ")
  cat("\n")
  cat("\n")
  if (tolower(answer) == "yes") {
    cat("\n",
        "================== ELEMENT INTERPRETATION ==================\n",
        "Element      | Use/Interpretation\n",
        "------------------------------------------------------------\n",
        "Ca/Fe        | Biogenic/detrital clay ratio, carbonate stratigraphy\n",
        "Ca/Ti        | Biogenic vs lithogenic sedimentation and carbonate content\n",
        "Ca/Al        | Changes in terrigenous sediment contribution\n",
        "Ca/K         | K-rich clay variation from varying bottom currents strength\n",
        "Fe/K         | Climat proxy: on set of arid conditions (decrease)\n",
        "Fe/Zr        | Climat proxy: Aridity/humidity indicator: High value more humid\n",
        "Fe/Rb        | Grain size proxy: grain size in turbidites (High value = coarser grains)\n",
        "Fe/Ca        | Climat proxy: Measuring terrigenous sediment fluxes, particularly as a rainfall and run- off proxy\n",
        "Al/Si        | Chemical weathering, clay content\n",
        "Al/Ca        | Proxy for precipitation and runoff\n",
        "Si/Al        | Wind strength, weathering intensity, biogenic production and aluminosilicate composition\n",
        "Si/Ca        | Aeolian dust flux, wind strength\n",
        "Ti/Ca        | Variation in terrigenous sediment delivery and marine carbonates\n",
        "Ti/Al        | Aeolian dust, wind strength, aridity, coarse sediment\n",
        "Ti/K         | Variations in sediment source\n",
        "Ti/Fe        | Wind strength, sediment provenance\n",
        "Ti/Rb        | Heavy mineral detection (turbidites)\n",
        "Ti/Sr        | Variation in terrigenous sediment delivery and climate variability\n",
        "Zr/Rb        | Grain size proxy, flood events\n",
        "Mn/Fe        | Redox proxy, diagenesis\n",
        "Mn/Ti        | Mn enrichment, oxidation levels\n",
        "Mn/Al        | Oxygenation changes\n",
        "K/Ti         | Sediment provenance, weathering intensity\n",
        "K/Ca         | Terrigenous increase (anthropogenic)\n",
        "K/Rb         | Illite content, porosity sensitivity\n",
        "K/Al         | Precipitation and runoff, weathering intensity\n",
        "Cu/Ti        | Post-depositional oxidation\n",
        "S/Cl         | Pyrite or high organic carbon\n",
        "Br/Cl        | Marin organic matter, porosity\n",
        "Br/Ti        | Organic productivity\n",
        "============================================================\n"
    )

    cat("\n")
    cat("\nList from: Micro-XRF Studies of Sediment Cores (Ian W. Croudace & R. Guy Rothwell 2015, doi:https://doi.org/10.1007/978-94-017-9849-5):\n")
    cat("\n")

    # Affichage des éléments simples disponibles dans df
    element_candidates <- colnames(df)
    element_names <- element_candidates[!grepl("/", element_candidates) & !grepl("log_", element_candidates)]
    cat("\n Elements available in your dataframe:\n")
    print(element_names)
    cat("\n")
    cat("\n")
    selected <- safe_readline(prompt = "\nWhat ratios would you like to create? (separate with commas: e.g. Fe/Ti, Ca/K) : ")
    selected_ratios <- strsplit(selected, ",")[[1]]
    selected_ratios <- trimws(selected_ratios)

    cat("\n")
    cat("\n")

    for (ratio in selected_ratios) {
      cat(paste0("\nCreation of the log ratio for ", ratio, "\n"))

      numerator <- safe_readline(paste("Numerator of", ratio, ": "))
      denominator <- safe_readline(paste("Denominator  of", ratio, ": "))

      var_name <- paste0("log_", numerator, "_", denominator)

      df[[var_name]] <- ifelse(
        !is.na(df[[numerator]]) & !is.na(df[[denominator]]) & df[[denominator]] != 0,
        log(df[[numerator]] / df[[denominator]]),
        NA
      )
      cat("\n")
      cat(paste0("Variable created: ", var_name, "\n"))
      cat("\n")
    }
  } else {
    cat("No log-transformed ratios were created.\n")
  }

  # --- Gestion des valeurs manquantes ---
  if (anyNA(df)) {
    cat("\nWARNING: The dataframe contains missing values (NA).\n")
    cat("Available columns:\n")
    print(colnames(df))
    cat("\n")

    exclude_response <- tolower(safe_readline("Do you want to exclude any columns from the NA filtering? (yes/no): "))
    excluded_columns <- NULL
    cat("\n")
    if (exclude_response == "yes") {
      input <- safe_readline("Enter column names to exclude (separated by commas): ")
      excluded_columns <- trimws(unlist(strsplit(input, ",")))
      invalid_columns <- setdiff(excluded_columns, colnames(df))
      if (length(invalid_columns) > 0) stop(paste("Invalid column names:", paste(invalid_columns, collapse = ", ")))
    }
    cat("\n")
    na_response <- tolower(safe_readline("Do you want to delete rows with NA values? (yes/no): "))
    if (na_response == "yes") {
      df_tmp <- df
      if (!is.null(excluded_columns)) df_tmp <- df[, !(names(df) %in% excluded_columns), drop = FALSE]
      na_rows <- which(apply(df_tmp, 1, function(x) any(is.na(x))))
      if (length(na_rows) > 0) {
        cat("Rows removed:", paste(na_rows, collapse = ", "), "\n")
        df <- df[complete.cases(df_tmp), ]
      } else {
        cat("No rows to remove: no NA values in selected columns.\n")
      }
      cat("The dataframe is now cleaned.\n")
    } else {
      cat("Rows containing NA values were retained.\n")
    }
  } else {
    cat("No missing values found in the dataframe.\n")
  }

  cat("\n")
  cat("\n")

  return(df)
}
