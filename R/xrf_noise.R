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
    "1) Noise amplitude to add (recommended 0.2): ", default = "0.2"))
  cat("\n")
  ensemble_size <- as.integer(safe_readline(
    "2) Number of EEMD iterations (recommended 100): ", default = "100"))
  cat("\n")
  drop_imf <- as.integer(safe_readline(
    "3) Number of first IMFs considered as noise (recommended 2): ", default = "2"))
  cat("\n")
  noise_threshold <- as.numeric(safe_readline(
    "4) Threshold for noise proportion to flag a variable (0–1, recommended 0.3): ", default = "0.3"))

  cat("\nParameters set:\n")
  cat("\n")
  cat(" → Noise strength = ", noise_strength, "\n")
  cat(" → EEMD iterations = ", ensemble_size, "\n")
  cat(" → Noise IMFs considered = ", drop_imf, "\n")
  cat(" → Noise threshold = ", noise_threshold, "\n\n")

  # --- Depth column selection ---
  message("Available columns in the dataset:")
  cat("\n")
  print(names(df)); cat("\n")

  depth_col <- NULL
  while (is.null(depth_col)) {
    choice <- safe_readline("Enter the name of the depth column: ")
    if (choice %in% names(df)) depth_col <- choice else message("Invalid column name. Please try again.")
  }

  # --- Noise calculation ---
  results <- data.frame(
    variable = character(),
    noise_score = numeric(),
    is_noisy = logical(),
    noise_threshold = numeric(),
    stringsAsFactors = FALSE
  )

  df_denoised_total <- df[depth_col]  # all denoised
  df_clean <- df[depth_col]           # filtered later

  for (col in setdiff(names(df), depth_col)) {
    x <- df[[col]]
    imfs <- Rlibeemd::eemd(
      x,
      noise_strength = noise_strength,
      ensemble_size = ensemble_size
    )

    var_total <- sum(apply(imfs, 2, var))
    var_noise <- sum(apply(imfs[, 1:drop_imf, drop = FALSE], 2, var))
    noise_score <- var_noise / var_total
    is_noisy <- noise_score > noise_threshold

    results <- rbind(results, data.frame(
      variable = col,
      noise_score = noise_score,
      is_noisy = is_noisy,
      noise_threshold = noise_threshold,
      stringsAsFactors = FALSE
    ))

    # Denoised signal (remove first IMFs)
    if (ncol(imfs) <= drop_imf) {
      signal_denoised <- rowSums(imfs)
    } else {
      signal_denoised <- rowSums(imfs[, -(1:drop_imf), drop = FALSE])
    }

    df_denoised_total[[col]] <- signal_denoised
    df_clean[[col]] <- signal_denoised
  }

  noisy_vars <- results$variable[results$is_noisy]

  # --- Display summary ---
  if (length(noisy_vars) > 0) {
    cat("\n===== Noise Summary by Element =====\n\n")
    for (i in seq_len(nrow(results))) {
      cat(sprintf("%-10s : Noise Score = %.3f, Is Noisy = %s\n",
                  results$variable[i],
                  results$noise_score[i],
                  results$is_noisy[i]))
    }
    cat("\n")

    ans <- ""
    while (!(tolower(ans) %in% c("yes", "no"))) {
      ans <- safe_readline("Do you want to remove noisy columns from the denoised dataset? (yes/no): ")
      if (!(tolower(ans) %in% c("yes", "no"))) message("Invalid response.")
    }

    if (tolower(ans) == "yes") {
      df_clean <- df_clean[, !(names(df_clean) %in% noisy_vars), drop = FALSE]
      cat("\n Noisy columns removed — 'df_clean' created with only analysable variables.\n\n")
    } else {
      cat("\n WARNING: No noisy variables were removed.\n")
      cat("=== These variables may remain non-analysable due to excessive noise content ===\n")
      cat("\n")
    }
  } else {
    cat("\nNo noisy variables detected — all variables retained.\n")
  }

  # --- Save both dataframes to Global Environment ---
  assign("df_clean", df_clean, envir = .GlobalEnv)
  assign("df_denoised_total", df_denoised_total, envir = .GlobalEnv)

  set_cache("df_clean_cache",df_clean)
  set_cache("df_denoised_total_cache",df_denoised_total)
  # --- Summary ---
  summary_text <- paste0(
    "===== XRF Noise Detection Summary =====\n",
    "\n",
    "Number of observations       : ", nrow(df), "\n",
    "Original variables           : ", ncol(df), "\n",
    "Depth column                 : ", depth_col, "\n\n",
    "EEMD parameters:\n",
    " → Noise strength            : ", noise_strength, "\n",
    " → EEMD iterations           : ", ensemble_size, "\n",
    " → Noise IMFs considered     : ", drop_imf, "\n",
    " → Noise threshold           : ", noise_threshold, "\n\n",
    "Number of noisy variables    : ", sum(results$is_noisy), "\n",
    "Noisy variables detected     : ", paste(results$variable[results$is_noisy], collapse = ", "), "\n",
    "Variables retained (df_clean): ", paste(names(df_clean), collapse = ", "), "\n",
    "\n",
    "======================================\n"
  )

  cat(summary_text)

  cat("\n=== Dataframes created ===\n")
  cat("\n")
  cat("→ df_denoised_total: all denoised variables. WARNING: No noisy variables were removed === These variables may remain non-analysable due to excessive noise content === \n")
  cat("\n")
  cat("→ df_clean         : denoised dataset after optional removal of noisy variables  === Ready for further analysis === \n")
  cat("\n")
  cat("=============================================\n")

  # --- Optional save summary ---
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
    }
  }
  else {message("No file selected. Summary not saved.")}
}
