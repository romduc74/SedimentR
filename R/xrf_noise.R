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

  safe_readline <- function(prompt = "", default = NULL) {
    input <- tryCatch({ readline(prompt) }, error = function(e) { "" })
    if (tolower(trimws(input)) == "exit") stop("Execution stopped by user with 'exit'.", call. = FALSE)
    if (input == "" && !is.null(default)) return(default)
    return(input)
  }

  # EEMD parameters
  cat("EEMD parameters for XRF analysis:\n\n")
  noise_strength <- as.numeric(safe_readline("1) Noise amplitude (recommended 0.2): ", default = "0.2"))
  cat("\n")
  ensemble_size  <- as.integer(safe_readline("2) Number of EEMD iterations (recommended 100): ", default = "100"))
  cat("\n")
  drop_imf       <- as.integer(safe_readline("3) Number of first IMFs considered as noise (recommended 2): ", default = "2"))

  cat("\nParameters set:\n")
  cat("\n")
  cat(" → Noise strength  =", noise_strength, "\n")
  cat("\n")
  cat(" → EEMD iterations =", ensemble_size, "\n")
  cat("\n")
  cat(" → Noise IMFs      =", drop_imf, "\n\n")

  # Depth column
  cat("Available columns in the dataset:\n\n")
  print(names(df)); cat("\n")
  depth_col <- NULL
  while (is.null(depth_col)) {
    choice <- safe_readline("Enter the name of the depth column: ")
    if (choice %in% names(df)) depth_col <- choice else message("Invalid column name. Try again.")
  }

  # Noise computation
  results <- data.frame(variable = character(), noise_score = numeric(), stringsAsFactors = FALSE)
  df_denoised_total <- df[depth_col]
  df_clean <- df[depth_col]

  for (col in setdiff(names(df), depth_col)) {
    x <- df[[col]]
    if (!is.numeric(x)) suppressWarnings(x <- as.numeric(as.character(x)))
    if (all(is.na(x))) next

    imfs <- Rlibeemd::eemd(x, noise_strength = noise_strength, ensemble_size = ensemble_size)
    var_total <- sum(apply(imfs, 2, var, na.rm = TRUE))
    var_noise <- sum(apply(imfs[, 1:min(drop_imf, ncol(imfs)), drop = FALSE], 2, var, na.rm = TRUE))
    noise_score <- var_noise / var_total

    results <- rbind(results, data.frame(variable = col, noise_score = noise_score))

    if (ncol(imfs) <= drop_imf) {
      signal_denoised <- rowSums(imfs, na.rm = TRUE)
    } else {
      signal_denoised <- rowSums(imfs[, -(1:drop_imf), drop = FALSE], na.rm = TRUE)
    }

    df_denoised_total[[col]] <- signal_denoised
    df_clean[[col]] <- signal_denoised
  }

  # Show noise scores
  cat("\n===== Noise Summary by Element =====\n\n")
  for (i in seq_len(nrow(results))) {
    cat(sprintf("%-10s : Noise Score = %.3f\n", results$variable[i], results$noise_score[i]))
  }
  cat("\n")

  # Automatic threshold: median − MAD
  median_ns <- median(results$noise_score, na.rm = TRUE)
  mad_ns <- mad(results$noise_score, constant = 1, na.rm = TRUE)
  threshold_auto <- median_ns + mad_ns
  threshold_auto <- max(threshold_auto, 0)

  cat("\n--- Threshold determination ---\n")
  cat("\n")
  cat(sprintf("Median = %.3f, MAD = %.3f, Automatic threshold (median−MAD) = %.3f\n\n",
              median_ns, mad_ns, threshold_auto))
  cat("\n")

  # User chooses threshold
  choice <- safe_readline("Use automatic threshold ('auto') or enter custom ('custom')? [auto/custom]: ", default = "auto")
  if (tolower(choice) == "custom") {
    custom_val <- safe_readline("Enter custom threshold (0–1): ")
    noise_threshold <- suppressWarnings(as.numeric(custom_val))
    if (is.na(noise_threshold)) {
      cat("Invalid value. Using automatic threshold.\n")
      noise_threshold <- threshold_auto
    }
  } else {
    noise_threshold <- threshold_auto
  }

  cat(sprintf("\nUsing noise threshold = %.3f\n\n", noise_threshold))

  # Flag clean/noisy variables
  results$is_clean <- results$noise_score < noise_threshold
  clean_vars <- results$variable[results$is_clean]
  noisy_vars <- results$variable[!results$is_clean]

  cat(sprintf("Clean variables (n=%d): %s\n", length(clean_vars), paste(clean_vars, collapse = ", ")))
  cat("\n")
  cat(sprintf("Noisy variables (n=%d): %s\n\n", length(noisy_vars), paste(noisy_vars, collapse = ", ")))

  # Keep only clean variables
  if (length(clean_vars) > 0) df_clean <- df_clean[, c(depth_col, clean_vars), drop = FALSE]

  # Save globally
  assign("df_clean", df_clean, envir = .GlobalEnv)
  assign("df_denoised_total", df_denoised_total, envir = .GlobalEnv)

  # Optional caching
  if (exists("set_cache")) {
    set_cache("df_clean_cache", df_clean)
    set_cache("df_denoised_total_cache", df_denoised_total)
  }

  # Summary
  summary_text <- paste0(
    "===== XRF Noise Detection Summary =====\n",
    "Observations       : ", nrow(df), "\n",
    "Original variables : ", ncol(df), "\n",
    "Depth column       : ", depth_col, "\n\n",
    "EEMD parameters:\n",
    " → Noise strength : ", noise_strength, "\n",
    " → EEMD iterations: ", ensemble_size, "\n",
    " → Noise IMFs      : ", drop_imf, "\n",
    " → Threshold used  : ", round(noise_threshold,3), "\n\n",
    "Clean variables (", length(clean_vars), "): ", paste(clean_vars, collapse = ", "), "\n",
    "Noisy variables detected (", length(noisy_vars), "): ",
    if(length(noisy_vars) > 0) paste(noisy_vars, collapse = ", ") else "None", "\n",
    "\n",
    "Dataframe df_clean ", length(clean_vars), " variables: " , paste(names(df_clean), collapse = ", "), "\n",
    "======================================\n"
  )
  cat(summary_text)
  cat("\n")

  # Save summary option
  save_summary <- safe_readline("Do you want to save the summary report (.txt)? (yes/no): ", default = "no")
  cat("\n")
  if (tolower(save_summary) == "yes") {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      file_path <- rstudioapi::selectFile(caption = "Save summary", label = "Save", existing = FALSE)
    } else {
      file_path <- safe_readline("Enter full path for output file (.txt): ")
    }
    if (!is.null(file_path) && file_path != "") {
      if (!grepl("\\.txt$", file_path)) file_path <- paste0(file_path, ".txt")
      writeLines(summary_text, con = file_path)
      message("Summary saved to: ", file_path)
    }
  }

  invisible(list(
    results = results,
    df_clean = df_clean,
    df_denoised_total = df_denoised_total,
    depth_col = depth_col
  ))
}
