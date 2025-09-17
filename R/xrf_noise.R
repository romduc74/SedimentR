#' XRF Noise Detection and Denoising
#'
#' This function performs EEMD-based noise detection on XRF data and optionally removes noisy variables.
#' It also creates a denoised dataset by removing the first IMFs considered as noise.
#'
#' @param df data.frame containing XRF data (columns = elements, one column for depth).
#' @return A list containing:
#'   - results: table of noise scores per variable
#'   - cleaned_data: data.frame with noisy columns removed (if chosen)
#'   - depth_col: name of the depth column
#'   - df_denoised: data.frame with denoised XRF data (first IMFs removed)
#' @export
#'
xrf_noise <- function(df) {
  if (!requireNamespace("Rlibeemd", quietly = TRUE)) install.packages("Rlibeemd")
  library(Rlibeemd)

  cat("\nType 'exit' at any time to quit.\n\n")

  # --- Safe readline function ---
  safe_readline <- function(prompt = "", default = NULL) {
    input <- tryCatch({ readline(prompt) }, error = function(e) { "" })
    if (tolower(trimws(input)) == "exit") stop("Execution stopped by user with 'exit'.", call. = FALSE)
    if (input == "" && !is.null(default)) return(default)
    return(input)
  }

  # --- Interactive parameters ---
  message("EEMD parameters for XRF analysis:\n")

  noise_strength <- as.numeric(safe_readline(
    "1) Noise amplitude to add (noise_strength, recommended 0.2): ", default = "0.2"))
  cat("\n")
  ensemble_size <- as.integer(safe_readline(
    "2) Number of EEMD iterations (ensemble_size, recommended 100): ", default = "100"))
  cat("\n")
  drop_imf <- as.integer(safe_readline(
    "3) Number of first IMFs considered as noise (1 to 2 (2 recommended)): ", default = "2"))
  cat("\n")
  noise_threshold <- as.numeric(safe_readline(
    "4) Threshold for noise proportion to flag a variable (0-1, recommended 0.3): ", default = "0.3"))

  cat("\nParameters set:\n")
  cat(" - Noise strength = ", noise_strength, "\n")
  cat(" - EEMD iterations = ", ensemble_size, "\n")
  cat(" - Noise IMFs considered = ", drop_imf, "\n")
  cat(" - Noise threshold = ", noise_threshold, "\n\n")

  # --- Depth column selection ---
  message("Available columns in the dataset:")
  cat("\n")
  print(names(df))
  cat("\n")

  depth_col <- NULL
  while (is.null(depth_col)) {
    choice <- safe_readline("Enter the name of the depth column: ")
    cat("\n")
    if (choice %in% names(df)) depth_col <- choice else message("Invalid column name. Please try again.")
  }
  cat("\n")

  # --- Noise calculation ---
  results <- data.frame(
    variable = character(),
    noise_score = numeric(),
    is_noisy = logical(),
    noise_threshold = numeric(),
    stringsAsFactors = FALSE
  )

  # Dataframe for denoised signals
  df_clean <- df[depth_col]  # start with depth column

  for (col in setdiff(names(df), depth_col)) {
    x <- df[[col]]
    imfs <- Rlibeemd::eemd(x,
                           noise_strength = noise_strength,
                           ensemble_size = ensemble_size)

    var_total <- sum(apply(imfs, 2, var))
    var_noise <- sum(apply(imfs[, 1:drop_imf, drop = FALSE], 2, var))
    noise_score <- var_noise / var_total
    is_noisy <- noise_score > noise_threshold

    results <- rbind(results,
                     data.frame(
                       variable = col,
                       noise_score = noise_score,
                       is_noisy = is_noisy,
                       noise_threshold = noise_threshold,
                       stringsAsFactors = FALSE
                     ))

    # --- Denoise signal by removing first IMFs ---
    if(ncol(imfs) <= drop_imf){
      signal_denoised <- rowSums(imfs)
    } else {
      signal_denoised <- rowSums(imfs[, -(1:drop_imf), drop = FALSE])
    }
    df_clean[[col]] <- signal_denoised
  }

  # Assign denoised dataframe to Global Environment
  assign("df_clean", df_clean, envir = .GlobalEnv)
  message("Denoised dataframe 'df_clean' has been created in the Global Environment and is ready for further analysis.\n")

  # --- Optional removal of noisy columns (on denoised data) ---
  noisy_vars <- results$variable[results$is_noisy]

  if(length(noisy_vars) > 0){
    message("\nDetected noisy columns: ", paste(noisy_vars, collapse = ", "))


    cat("\n")
    # --- Prepare summary text ---
    cleaned_vars <- names(df_clean)
    removed_vars <- setdiff(setdiff(names(df), depth_col), cleaned_vars)

    summary_text1 <- paste0(
      "===== XRF Noise Detection =====\n\n",
      "Number of observations       : ", nrow(df), "\n",
      "Original variables           : ", ncol(df), "\n",
      "Number of noisy variables    : ", sum(results$is_noisy), "\n",
      "Noisy variables detected     : ", paste(results$variable[results$is_noisy], collapse = ", "), "\n",
      "======================================\n"
    )

    cat("\n")
    cat(summary_text1)
    cat("\n")


    ans <- ""
    while(!(tolower(ans) %in% c("yes", "no"))){
      ans <- safe_readline("Do you want to remove these columns from the denoised dataset? (yes/no): ")
      if(!(tolower(ans) %in% c("yes", "no"))) message("Invalid response.")
    }

    if(tolower(ans) == "yes"){
      df_clean <- df_clean[, !(names(df_clean) %in% noisy_vars), drop = FALSE]
      message("\nUpdated denoised dataframe 'df_clean' with noisy columns removed is ready in the Global Environment.")
      assign("df_clean", df_clean, envir = .GlobalEnv)
    }
  }

  cat("\n")
  # --- Prepare summary text ---
  cleaned_vars <- names(df_clean)
  removed_vars <- setdiff(setdiff(names(df), depth_col), cleaned_vars)

  summary_text <- paste0(
    "===== XRF Noise Detection Summary =====\n\n",
    "Number of observations       : ", nrow(df), "\n",
    "Original variables           : ", ncol(df), "\n",
    "Depth column                 : ", depth_col, "\n\n",
    "EEMD parameters:\n",
    " - Noise strength            : ", noise_strength, "\n",
    " - EEMD iterations           : ", ensemble_size, "\n",
    " - Noise IMFs considered     : ", drop_imf, "\n",
    " - Noise threshold           : ", noise_threshold, "\n\n",
    "Number of noisy variables    : ", sum(results$is_noisy), "\n",
    "Noisy variables detected     : ", paste(results$variable[results$is_noisy], collapse = ", "), "\n",
    "Variables retained           : ", paste(cleaned_vars, collapse = ", "), "\n",
    "======================================\n"
  )

  cat("\n")
  cat(summary_text)
  cat("\n")

  # --- Export summary ---
  save_summary <- ""
  while (!(tolower(save_summary) %in% c("yes", "no"))) {
    save_summary <- safe_readline("Do you want to save the summary report (.txt)? (yes/no): ")
    if (!(tolower(save_summary) %in% c("yes", "no"))) message("Invalid response.")
  }

  if (tolower(save_summary) == "yes") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      file_path <- rstudioapi::selectFile(
        caption = "Save summary",
        label = "Save",
        existing = FALSE
      )
    } else {
      file_path <- safe_readline("Enter full path for output file (.txt): ")
    }
    if (!is.null(file_path) && file_path != "") {
      if (!grepl("\\.txt$", file_path)) file_path <- paste0(file_path, ".txt")
      writeLines(summary_text, con = file_path)
      message("Summary saved to: ", file_path)
    } else message("No file selected. Summary not saved.")
  }
}
