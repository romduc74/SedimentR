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
xrf_noise <- function(df = NULL) {
  if (!requireNamespace("Rlibeemd", quietly = TRUE)) install.packages("Rlibeemd")
  library(Rlibeemd)

  cat("\nType 'exit' at any time to quit.\n\n")

  safe_readline <- function(prompt = "", default = NULL) {
    input <- tryCatch({ readline(prompt) }, error = function(e) { "" })
    if (tolower(trimws(input)) == "exit") stop("Execution stopped by user with 'exit'.", call. = FALSE)
    if (input == "" && !is.null(default)) return(default)
    return(input)
  }

  # --- Retrieve dataframe from cache if not provided ---
  if (is.null(df)) {

    if (exists("list_cache") && "df_normalized_cache" %in% list_cache()) {
      df <- get_cache("df_normalized_cache")
      message("Using cached 'df_normalized_cache' dataframe.")
    } else {
      stop("No dataframe provided and 'df_normalized_cache' not found in cache.")
    }
  }
  cat("\n")
  # --- EEMD parameters ---
  cat("EEMD parameters:\n\n")
  noise_strength <- as.numeric(safe_readline("1) Noise amplitude [default = 0.2]: ", default = "0.2"))
  cat("\n")
  ensemble_size  <- as.integer(safe_readline("2) Number of EEMD iterations [default = 100]: ", default = "100"))
  cat("\n")

  #drop_imf       <- as.integer(safe_readline("3) Number of first IMFs considered as noise (recommended 2): ", default = "2"))
  cat(" ===== Denoising level selection =====\n\n")

  cat(" 1 → Reliable denoising (IMF1 removed)\n\n")

  cat(" 2 → Strong denoising (IMF1–2 removed)\n\n")

  cat("About IMFs (Intrinsic Mode Functions):\n\n")

  cat("- IMF 1 contains the highest-frequency components\n\n")

  cat("- Subsequent IMFs represent progressively lower-frequency, more structured signals\n\n")

  cat("WARNING: Removing too many IMFs may suppress meaningful geochemical variability\n\n")

  denoise_level <- as.integer(safe_readline("3) Denoising level : Choose level (1 or 2) [default = 1]: ",default = "1"))

  drop_imf <- switch(
    denoise_level,
    `1` = 1,  # conservative / reliable denoising
    `2` = 2,  # stronger denoising
    1         # fallback safety
  )

  denoising_strength <- switch(
    drop_imf,
    `1` = "Standard (reliable)",
    `2` = "Strong",
    "Standard (reliable)"
  )

  cat("\n")

  cat("Parameters set:\n\n")
  cat(" → Noise strength  =", noise_strength, "\n\n")
  cat(" → EEMD iterations =", ensemble_size, "\n\n")
  cat(" → Noise IMFs      =", drop_imf, "\n\n")
  cat(" → Denoising strength  =", denoising_strength, "\n\n")

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

  # --- Explanation of noise_score ---
  cat("\n--- Understanding the Noise Score ---\n\n")
  cat("The 'noise_score' represents the proportion of total signal variance\n")
  cat("that comes from the first IMFs (the highest-frequency, most 'noisy' components).\n")
  cat(" → A high noise_score (close to 1) means the signal is dominated by noise.\n")
  cat(" → A low noise_score (close to 0) means the signal is mostly clean.\n\n")
  cat("Choose a threshold to keep only variables that are less noisy.\n")
  cat("For example, setting threshold = 0.3 will keep variables with noise_score < 0.3.\n\n")

  # --- Display table of noise scores ---
  cat("\n===== Noise Scores per Variable =====\n\n")
  cat(sprintf("%-20s : %-10s\n", "Variable", "Noise_Score"))
  cat(strrep("-", 35), "\n")
  for(i in 1:nrow(results)) {
    cat(sprintf("%-20s : %-10.3f\n",
                results$variable[i],
                results$noise_score[i]))
  }
  cat("\n")

  # --- User threshold choice ---
  thr_choice <- safe_readline("Enter noise_score threshold to keep low-noise variables (0–1, e.g. 0.3): ", default = "0.3")
  cat("\n")
  noise_threshold <- suppressWarnings(as.numeric(thr_choice))
  if(is.na(noise_threshold)) {
    noise_threshold <- 0.3
    cat("Invalid input. Using default threshold = 0.3\n\n")
  }

  results$is_clean <- results$noise_score < noise_threshold
  threshold_used <- paste0("Custom noise_score <", noise_threshold)
  cat(sprintf("Using noise_score threshold: %.3f → keeping variables with less noise\n\n", noise_threshold))

  # --- Selection ---
  clean_vars <- results$variable[results$is_clean]
  noisy_vars <- results$variable[!results$is_clean]

  cat(sprintf("Low-noise variables kept (n=%d): %s\n\n", length(clean_vars), paste(clean_vars, collapse=", ")))
  cat(sprintf("High-noise variables excluded (n=%d): %s\n\n", length(noisy_vars), if(length(noisy_vars)>0) paste(noisy_vars, collapse=", ") else "None"))

  if(length(clean_vars) > 0) df_clean <- df_clean[, c(depth_col, clean_vars), drop=FALSE]

  # --- Save globally & optional caching ---
  # assign("df_clean", df_clean, envir=.GlobalEnv)
  #  assign("df_denoised_total", df_denoised_total, envir=.GlobalEnv)

  # --- Summary ---
  summary_text <- paste0(
    "===== XRF Noise Detection Summary =====\n\n",
    "Observations       : ", nrow(df), "\n",
    "Original variables : ", ncol(df), "\n",
    "Depth column       : ", depth_col, "\n\n",
    "EEMD parameters:\n\n → Noise strength : ", noise_strength,
    "\n\n → EEMD iterations : ", ensemble_size,
    "\n\n → Noise IMFs      : ", drop_imf,
    "\n\n → Denoising strength  : ", denoising_strength,
    "\n\n → Threshold used  : ", threshold_used, "\n\n",
    "Low-noise variables (", length(clean_vars), "): ", paste(clean_vars, collapse=", "), "\n\n",
    "High-noise variables (", length(noisy_vars), "): ", if(length(noisy_vars)>0) paste(noisy_vars, collapse=", ") else "None", "\n\n",
    "Dataframe df_clean: ", paste(names(df_clean), collapse=", "), "\n",
    "======================================\n"
  )
  cat(summary_text)
  cat("\n")

  # --- Save summary option ---
  save_summary <- safe_readline("Save summary report (.txt)? (yes/no): ", default="no")
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

  df_normalized <- df_clean[, setdiff(names(df_clean), depth_col), drop = FALSE]

  if (exists("set_cache")) {
    set_cache("df_normalized_cache", df_normalized)
    set_cache("df_denoised_total_cache", df_denoised_total)
  }


  # Retirer la colonne depth
  df_denoised_total_no_depth <- df_denoised_total[, setdiff(names(df_denoised_total), depth_col), drop = FALSE]

  # Renommer toutes les colonnes avec suffixe "_denoised"
  names(df_denoised_total_no_depth) <- paste0(names(df_denoised_total_no_depth), "_denoised")

  # Ajouter la colonne depth au début (avec les vraies valeurs)
  df_denoised_total_final <- cbind(depth = df_denoised_total[[depth_col]], df_denoised_total_no_depth)

  # Assigner dans le global environment
  assign("df_denoised", df_denoised_total_final, envir = .GlobalEnv)


  invisible(list(results=results, df_clean=df_clean, df_denoised_total=df_denoised_total, depth_col=depth_col))
}
