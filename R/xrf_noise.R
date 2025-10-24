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

  # --- EEMD parameters ---
  cat("EEMD parameters:\n\n")
  noise_strength <- as.numeric(safe_readline("1) Noise amplitude (recommended 0.2): ", default = "0.2"))
  cat("\n")
  ensemble_size  <- as.integer(safe_readline("2) Number of EEMD iterations (recommended 100): ", default = "100"))
  cat("\n")
  drop_imf       <- as.integer(safe_readline("3) Number of first IMFs considered as noise (recommended 2): ", default = "2"))
  cat("\n")

  cat("Parameters set:\n\n")
  cat(" → Noise strength  =", noise_strength, "\n\n")
  cat(" → EEMD iterations =", ensemble_size, "\n\n")
  cat(" → Noise IMFs      =", drop_imf, "\n\n")

  # --- Depth column ---
  cat("Available columns in the dataset:\n\n")
  print(names(df)); cat("\n")
  depth_col <- NULL
  while(is.null(depth_col)) {
    choice <- safe_readline("Enter the name of the depth column: ")
    if(choice %in% names(df)) depth_col <- choice else message("Invalid column name, try again.")
  }

  # --- Noise computation ---
  results <- data.frame(variable=character(), noise_score=numeric(), stringsAsFactors = FALSE)
  df_denoised_total <- df[depth_col]
  df_clean <- df[depth_col]

  for(col in setdiff(names(df), depth_col)) {
    x <- df[[col]]
    if(!is.numeric(x)) suppressWarnings(x <- as.numeric(as.character(x)))
    if(all(is.na(x))) next

    imfs <- Rlibeemd::eemd(x, noise_strength=noise_strength, ensemble_size=ensemble_size)
    var_total <- sum(apply(imfs,2,var,na.rm=TRUE))
    var_noise <- sum(apply(imfs[, 1:min(drop_imf, ncol(imfs)), drop=FALSE],2,var,na.rm=TRUE))
    noise_score <- var_noise / var_total
    results <- rbind(results, data.frame(variable=col, noise_score=noise_score))

    # Denoised signal
    signal_denoised <- if(ncol(imfs) <= drop_imf) rowSums(imfs, na.rm=TRUE) else rowSums(imfs[, -(1:drop_imf), drop=FALSE], na.rm=TRUE)
    df_denoised_total[[col]] <- signal_denoised
    df_clean[[col]] <- signal_denoised
  }

  # --- Robust Z-score ---
  median_ns <- median(results$noise_score, na.rm=TRUE)
  mad_ns <- mad(results$noise_score, constant=1, na.rm=TRUE)
  k <- 0.5
  results$robust_Z <- (results$noise_score - median_ns) / mad_ns

  cat("\n--- Robust Z-score explanation ---\n\n")
  cat("The robust Z-score is computed as:\n")
  cat("  Z_i = (noise_score_i - median(noise_scores)) / MAD(noise_scores)\n")
  cat("It measures how far each variable's noise is from the median, scaled by the MAD.\n")
  cat("This method is robust to outliers and non-normal distributions.\n")
  cat("Variables with Z > 1.5 are typically considered noisy.\n\n")

  # --- Show table ---
  cat("\n===== Noise Scores & Robust Z-scores =====\n\n")
  cat(sprintf("%-10s : %-10s : %-10s\n", "Variable", "Noise", "Z-score"))
  cat(strrep("-", 35), "\n")
  for(i in 1:nrow(results)) {
    cat(sprintf("%-10s : %-10.3f : %-10.3f\n",
                results$variable[i],
                results$noise_score[i],
                results$robust_Z[i]))
  }
  cat("\n")

  # --- User threshold choice ---
  thr_choice <- safe_readline("Use default Robust Z-score [enter] or enter custom threshold on noise_score? (0–1): ", default = "")
  cat("\n")
  if(thr_choice == "") {
    # Default robust Z
    results$is_clean <- results$robust_Z <= k
    cat(sprintf("Clean data → Using Robust Z-score threshold: Z ≤ %.1f\n\n", k))
    threshold_used <- paste0("Robust Z-score (Z >", k, ")")
  } else {
    # Custom threshold
    noise_threshold <- suppressWarnings(as.numeric(thr_choice))
    if(is.na(noise_threshold)) {
      cat("Invalid input. Using Robust Z-score.\n\n")
      results$is_clean <- results$robust_Z <= k
      threshold_used <- paste0("Robust Z-score (Z >", k, ")")
    } else {
      results$is_clean <- results$noise_score < noise_threshold
      threshold_used <- paste0("Custom noise_score <", noise_threshold)
      cat(sprintf("Using custom noise_score threshold: %.3f\n\n", noise_threshold))
    }
  }

  clean_vars <- results$variable[results$is_clean]
  noisy_vars <- results$variable[!results$is_clean]

  cat(sprintf("Clean variables (n=%d): %s\n\n", length(clean_vars), paste(clean_vars, collapse=", ")))
  cat(sprintf("Noisy variables (n=%d): %s\n\n", length(noisy_vars), paste(noisy_vars, collapse=", ")))

  if(length(clean_vars) > 0) df_clean <- df_clean[, c(depth_col, clean_vars), drop=FALSE]

  # --- Save globally & optional caching ---
  assign("df_clean", df_clean, envir=.GlobalEnv)
  assign("df_denoised_total", df_denoised_total, envir=.GlobalEnv)
  if(exists("set_cache")) {
    set_cache("df_clean_cache", df_clean)
    set_cache("df_denoised_total_cache", df_denoised_total)
  }

  # --- Summary ---
  summary_text <- paste0(
    "===== XRF Noise Detection Summary =====\n\n",
    "Observations       : ", nrow(df), "\n",
    "Original variables : ", ncol(df), "\n",
    "Depth column       : ", depth_col, "\n\n",
    "EEMD parameters:\n\n → Noise strength : ", noise_strength,
    "\n\n → EEMD iterations : ", ensemble_size,
    "\n\n → Noise IMFs      : ", drop_imf,
    "\n\n → Threshold used  : ", threshold_used, "\n\n",
    "Clean variables (", length(clean_vars), "): ", paste(clean_vars, collapse=", "), "\n\n",
    "Noisy variables detected (", length(noisy_vars), "): ", if(length(noisy_vars)>0) paste(noisy_vars, collapse=", ") else "None", "\n\n",
    "Dataframe df_clean: ", paste(names(df_clean), collapse=", "), "\n",
    "======================================\n"
  )
  cat(summary_text)
  cat("\n")

  # --- Save summary option ---
  save_summary <- safe_readline("Save summary report (.txt)? [yes/no]: ", default="no")
  cat("\n")
  if(tolower(save_summary)=="yes") {
    if(requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
      file_path <- rstudioapi::selectFile(caption="Save summary", label="Save", existing=FALSE)
    } else {
      file_path <- safe_readline("Enter full path for output file (.txt): ")
    }
    if(!is.null(file_path) && file_path!="") {
      if(!grepl("\\.txt$", file_path)) file_path <- paste0(file_path,".txt")
      writeLines(summary_text, con=file_path)
      message("Summary saved to: ", file_path)
    }
  }

  invisible(list(results=results, df_clean=df_clean, df_denoised_total=df_denoised_total, depth_col=depth_col))
}
