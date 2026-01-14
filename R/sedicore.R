#' SedimentR Workflow
#'
#' @description
#' Executes the full SedimentR workflow on a geochemical dataset:
#' 1. Loads the data (CSV or Excel) interactively if `file_path` is NULL.
#'    Uses the helper function `charger()` to import the data with user-specified
#'    separator (for CSV) or sheet (for Excel), and encoding.
#' 2. Performs noise reduction (EEMD and IMF methods) via `xrf_noise()`.
#' 3. Scales the data (CLR or log ratio) via `scale_data()`.
#' 4. Selects variables using PCA and loading thresholds via `prop.select()`.
#' 5. Performs clustering (K-means) via `xrf_clust()`.
#'
#' The workflow guides the user interactively through file selection,
#' encoding, sheet selection (for Excel), and parameters for each step.
#'
#' @param file_path Optional. Character string with the full path to the data file (CSV or XLSX).
#'                  If not provided, an interactive file selection dialog will be displayed (requires RStudio).
#' @param sheet Optional. Character string specifying the sheet name for Excel files (default: "data").
#'
#' @return Invisibly returns `NULL`. All intermediate steps are printed to the console,
#'         and functions like `xrf_clust()` may modify dataframes in the global environment.
#'
#' @details
#' - CSV files: the user can specify the separator and file encoding.
#' - Excel files: the user selects the sheet to import.
#' - Each step prints informative messages and confirms completion.
#' - The clustering step integrates variable selection, K-means clustering,
#'   cluster driver analysis, and optional PCA visualization.
#'
#' @examples
#' \dontrun{
#' # Run workflow interactively, select file through dialog
#' sedicore()
#'
#' # Provide file path directly
#' sedicore(file_path = "path/to/my_data.xlsx", sheet = "Sheet1")
#' }
#'
#' @export
sedicore <- function(file_path = NULL, sheet = "data") {

  # --- Required packages ---
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  library(rstudioapi)
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)

  cat("\n==========================================\n")
  cat("           SedimentR Workflow             \n")
  cat("==========================================\n")

  # --- Select file if not provided ---
  if (is.null(file_path)) {
    if (!rstudioapi::isAvailable()) stop("RStudio API not available for interactive selection.")

    cat("\nSelect your data file (CSV or XLSX)...\n")
    file_path <- rstudioapi::selectFile(
      caption = "Select your data file",
      label = "Open",
      path = getwd(),
      filter = list(
        "Data files" = c("csv", "xlsx"),
        "CSV files" = "csv",
        "Excel files" = "xlsx"
      ),
      existing = TRUE
    )
    if (!nzchar(file_path)) {
      cat("\nFile selection cancelled.\n")
      return(invisible(NULL))
    }
  }

  cat("\nSelected file:", basename(file_path), "\n")
  extension <- tools::file_ext(file_path)

  # --- Safe readline helper ---
  safe_readline <- function(prompt_msg) {
    input <- readline(prompt = prompt_msg)
    if (tolower(input) == "exit") stop("User interrupted execution via 'exit'.")
    return(input)
  }

  # # --- Prepare arguments for 'charger' ---
  sep <- ","
  fileEncoding <- "UTF-8"

  if (tolower(extension) == "csv") {
    cat("\n--- CSV file detected --------------------\n\n")
    sep <- safe_readline("Enter separator used in CSV (default ','): ")
    if (sep == "") sep <- ","
    cat("\n")
    fileEncoding <- safe_readline("Enter file encoding (default 'UTF-8'): ")
    if (fileEncoding == "") fileEncoding <- "UTF-8"
  }
  #
  if (tolower(extension) == "xlsx") {
    cat("\n--- Excel file detected ------------------\n\n")
    sheets <- readxl::excel_sheets(file_path)
    cat("Available sheets:\n")
    print(sheets)
    cat("\n")

    repeat {
      sheet_choice <- safe_readline("Enter sheet name to load: ")
      if (sheet_choice %in% sheets) {
        sheet <- sheet_choice
        break
      } else cat("Invalid sheet name. Try again.\n\n")
    }
  }

  # --- Step 1: Data loading ---
  cat("\n==========================================\n")
  cat("\n--- Step 1 : Loading data for Clustering Analysis -----------------\n")
  df <- charger(file_path, sheet = sheet, sep = sep, fileEncoding = fileEncoding)
  cat("\nData successfully loaded.\n")
  cat("\n==========================================\n")

  # --- Step 2: Noise reduction ---
  # cat("\n==========================================\n")
  # cat("\n--- Step 2 : Noise reduction (EEMD and IMF method) --------------\n")
  # cat("\n")
  # xrf_noise(df)
  # cat("\nNoise reduction completed.\n")
  # cat("\n==========================================\n")

  # --- Step 3: Scaling data ---
  cat("\n==========================================\n")
  cat("\n--- Step 3 : Scaling data (CLR and/or Log for ratio)-----------------\n")
  cat("\n")
  scale_data()
  cat("\nData scaling completed.\n")
  cat("\n==========================================\n")

  # --- Step 4: Selecting variables ---
  cat("\n==========================================\n")
  cat("\n--- Step 4 : Selecting variables (PCA and loadings threeshold) ----------\n")
  cat("\n")
  prop.select()
  cat("\nVariable selection completed.\n")
  cat("\n==========================================\n")

  # --- Step 5: Running clustering ---
  cat("\n==========================================\n")
  cat("\n--- Step 5 : Running clustering (K-means method) -----------\n")
  cat("\n")
  xrf_clust()
  cat("\n==========================================\n")

  cat("\n=== SedimentR workflow completed successfully ===\n")

  cat("\n")
}
