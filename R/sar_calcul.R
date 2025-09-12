  #' @title SAR and MAR Calculation with Optional Export
  #'
  #' @description
  #' Calculates the Sediment Accumulation Rate (SAR) and, if desired, the Mass Accumulation Rate (MAR),
  #' along with total accumulated mass and erosion. Prompts the user to optionally export the results as a CSV or Excel file.
  #'
  #' Requirements for MAR calculation:
  #' 1. An age-depth model
  #' 2. Knowledge of depths at sedimentation changes (e.g., breakpoints or clustering)
  #' 3. The size of the watershed in hectares
  #' 4. The surface area of the reservoir in square meters
  #'
  #' @return A `data.frame` with SAR (and MAR) results.
  #' @export
  sar_calcul <- function() {

      library(readxl)
    library(openxlsx)
    library(rstudioapi)
    library(tools)

    cat("==== SAR (Sediment Accumulation Rate) Calculation ====\n")


    cat("WARNING: This function requires the following to proceed with the calculation:\n")
    cat("1. An age-depth model of the sediment core.\n")
    cat("2. A dataframe with density values.\n")
    cat("3. Depths where sedimentation changes occur (e.g., from breakpoints or clustering).\n")
    cat("4. The size of the watershed (in hectares).\n")
    cat("5. The surface area of the reservoir (in square meters).\n\n")


    n <- as.integer(readline(prompt = "How many periods do you want to analyze? "))

    sar_results <- data.frame(
      Period = integer(n),
      Start_depth_mm = numeric(n),
      End_depth_mm = numeric(n),
      Start_year = numeric(n),
      End_year = numeric(n),
      Delta_cm = numeric(n),
      Delta_years = numeric(n),
      SAR_cm_per_year = numeric(n),
      Mean_density_g_cm3 = numeric(n),
      MAR_g_cm2_yr = numeric(n),
      Total_mass_tons = numeric(n),
      Erosion_tons_ha_yr = numeric(n)
    )

    # Step 1: Calculate SAR per period
    for (i in 1:n) {
      cat(paste0("\n--- Period ", i, " ---\n"))
      start_depth <- as.numeric(readline("Start depth (mm): "))
      end_depth <- as.numeric(readline("End depth (mm): "))
      start_year <- as.numeric(readline("Start year: "))
      end_year <- as.numeric(readline("End year: "))

      delta_depth_cm <- abs(end_depth - start_depth) / 10
      delta_years <- abs(end_year - start_year)
      sar <- delta_depth_cm / delta_years

      sar_results[i, ] <- list(
        Period = i,
        Start_depth_mm = start_depth,
        End_depth_mm = end_depth,
        Start_year = min(start_year, end_year),
        End_year = max(start_year, end_year),
        Delta_cm = delta_depth_cm,
        Delta_years = delta_years,
        SAR_cm_per_year = sar,
        Mean_density_g_cm3 = NA,
        MAR_g_cm2_yr = NA,
        Total_mass_tons = NA,
        Erosion_tons_ha_yr = NA
      )
    }

    # Step 2: Ask if user wants to calculate MAR
    calc_mar <- tolower(readline("\nDo you want to calculate MAR? (yes/no): "))
    if (calc_mar %in% c("yes", "y", "oui", "o")) {

      # Step 3: Let user choose density file
      cat("\nPlease select the file containing bulk density data...\n")
      dens_file <- rstudioapi::selectFile(
        caption = "Select the Excel or CSV file with density data",
        filter = c("CSV files" = "*.csv", "Excel files" = "*.xlsx"),
        path = getwd(),
        existing = TRUE
      )
      if (length(dens_file) == 0 || dens_file == "") stop("No file selected.")
      ext <- file_ext(dens_file)

      # Step 4: Read file
      if (ext == "xlsx") {
        sheets <- excel_sheets(dens_file)
        cat("Available sheets:\n"); print(sheets)
        sheet <- readline("Enter the sheet name: ")
        if (!sheet %in% sheets) stop("Sheet not found.")
        data_dens <- read_excel(dens_file, sheet = sheet)
      } else if (ext == "csv") {
        sep_choice <- readline("CSV separator? (1=comma ',', 2=semicolon ';', 3=tab '\\t', 4=space ' ') : ")
        sep <- switch(sep_choice, "1" = ",", "2" = ";", "3" = "\t", "4" = " ", stop("Invalid separator"))
        data_dens <- read.csv(dens_file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type.")
      }

      cat("Available columns:\n"); print(names(data_dens))
      depth_col <- readline("Name of the depth column: ")
      if (!depth_col %in% names(data_dens)) stop("Depth column not found.")
      density_col <- readline("Name of the density column: ")
      if (!density_col %in% names(data_dens)) stop("Density column not found.")

      df <- data_dens[, c(depth_col, density_col)]
      names(df) <- c("depth", "density")

      # Step 5: Ask for reservoir & watershed areas
      reservoir_area_m2 <- as.numeric(readline("Enter reservoir surface area (m²): "))
      watershed_area_ha <- as.numeric(readline("Enter watershed area (ha): "))

      # Step 6: Calculate MAR
      for (i in 1:n) {
        p1 <- sar_results$Start_depth_mm[i]
        p2 <- sar_results$End_depth_mm[i]

        subset <- df[df$depth >= min(p1, p2) & df$depth <= max(p1, p2), ]
        mean_density <- mean(subset$density, na.rm = TRUE)
        sar <- sar_results$SAR_cm_per_year[i]
        mar <- sar * mean_density

        # Total mass & erosion
        B <- mar * 10000
        C <- B * reservoir_area_m2
        D <- C / 1e6
        erosion <- D / watershed_area_ha

        sar_results[i, "Mean_density_g_cm3"] <- mean_density
        sar_results[i, "MAR_g_cm2_yr"] <- mar
        sar_results[i, "Total_mass_tons"] <- D
        sar_results[i, "Erosion_tons_ha_yr"] <- erosion
      }

      cat("\n==== MAR and Erosion Results ====\n")
      print(sar_results)
    }

    # Step 7: Export results
    export_choice <- tolower(readline("\nDo you want to export the results? (yes/no): "))
    if (export_choice %in% c("yes", "y", "oui", "o")) {
      export_format <- tolower(readline("Which format? (csv/xlsx): "))
      file_path <- rstudioapi::selectFile(caption = "Save As", existing = FALSE)

      if (export_format == "csv") {
        if (!grepl("\\.csv$", file_path)) file_path <- paste0(file_path, ".csv")
        write.csv(sar_results, file_path, row.names = FALSE)
        cat(paste("Results exported to", file_path, "\n"))
      } else if (export_format == "xlsx") {
        if (!grepl("\\.xlsx$", file_path)) file_path <- paste0(file_path, ".xlsx")
        write.xlsx(sar_results, file_path)
        cat(paste("Results exported to", file_path, "\n"))
      } else {
        cat("Unsupported export format.\n")
      }
    }

    invisible(sar_results)
  }
