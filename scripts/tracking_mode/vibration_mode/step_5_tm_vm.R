# -----------------------------------------------------------
# File: process_zones.R
# -----------------------------------------------------------
# Harmonized version of the process_zones function for vibration_mode.
# This function processes experimental zones from enriched data.
# It displays zone definitions, optionally shows a visual schema,
# prompts for zone numbers, filters the data per zone, optionally calculates Zone 1,
# and saves the processed data globally as 'zone_data_list'.
# -----------------------------------------------------------

process_zones <- function(enriched_data) {
  message("\n---\n")
  message("👋 Welcome to the Zone Processing Function!\n")
  message("📋 This function will help you:")
  message("   • Filter enriched data by user-specified zones.")
  message("   • Optionally display a visual representation of zones.")
  message("   • Optionally calculate Zone 1 if Zones 0 and 2 are present.")
  message("   • Save processed zone data globally as 'zone_data_list'.\n")
  
  message("ℹ️ Zones overview:")
  message("   - Zone 0: Outermost zone.")
  message("   - Zone 1: Between Zone 0 and Zone 2 (if applicable).")
  message("   - Zone 2: Innermost zone.\n")
  message("ℹ️ See 'inputs/tracking_mode/vibration_mode/docs/schema_well_zones.jpg' for a visual schema.\n")
  
  # Load pre-recorded inputs.
  pipeline_inputs <- list()
  inputs_path <- "inputs/inputs_values"
  inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
  inputs_file_csv  <- file.path(inputs_path, "pipeline_inputs.csv")
  if (file.exists(inputs_file_xlsx)) {
    df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("❌ The pipeline_inputs.xlsx file must contain columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  } else if (file.exists(inputs_file_csv)) {
    df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("❌ The pipeline_inputs.csv file must contain columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  }
  
  # Unified input helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(candidate)) {
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 1: Optionally display zone visualization.
  zones_vizualisation <- get_input_local("zones_vizualisation",
                                         "❓ Do you need a visual representation of the zones? (yes/no): ",
                                         validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                         transform_fn = function(x) tolower(trimws(x)),
                                         error_msg = "❌ Please enter 'yes' or 'no'.")
  if (zones_vizualisation %in% c("yes", "y")) {
    message("🖼️ Displaying zone visualization...")
    img_path <- "inputs/tracking_mode/vibration_mode/docs/schema_well_zones.jpg"
    if (file.exists(img_path)) {
      if (interactive()) {
        utils::browseURL(img_path)
      } else {
        message("⚠️ Non-interactive session. Open the image manually at: ", img_path)
      }
    } else {
      message("❌ Image not found at: ", img_path)
    }
  }
  
  # Step 2: Prompt for zone numbers.
  zones_input <- get_input_local("zones_number",
                                 "❓ Enter the zone numbers (comma-separated, e.g., 0,1,2): ",
                                 validate_fn = function(x) {
                                   z <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                   length(z) > 0 && !any(is.na(z)) && all(z %in% c(0, 1, 2))
                                 },
                                 transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                 error_msg = "❌ Please enter valid zone numbers (only 0, 1, and 2 allowed).")
  zones <- zones_input
  message("✔️ Zones recorded: ", paste(zones, collapse = ", "))
  
  calculate_zone_1 <- all(c(0, 2) %in% zones)
  
  # Process each zone.
  zone_data <- list()
  for (zone in zones) {
    message(sprintf("🛠️ Processing Zone %d...", zone))
    zone_data[[as.character(zone)]] <- enriched_data %>% filter(an == zone)
  }
  
  # Calculate Zone 1 if applicable.
  if (calculate_zone_1) {
    message("🧮 Calculating Zone 1 by subtracting Zone 2 from Zone 0...")
    numeric_columns <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", 
                         "larct", "lardur", "lardist", "emptyct", "emptydur")
    if ("0" %in% names(zone_data) && "2" %in% names(zone_data)) {
      for (col in numeric_columns) {
        zone_data[["0"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["0"]][[col]])))
        zone_data[["2"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["2"]][[col]])))
      }
      zone_data[["1"]] <- zone_data[["0"]]
      zone_data[["1"]][, numeric_columns] <- zone_data[["0"]][, numeric_columns] - zone_data[["2"]][, numeric_columns]
      message("✔️ Zone 1 calculated successfully.")
    } else {
      message("⚠️ Unable to calculate Zone 1: missing Zone 0 or Zone 2 data.")
    }
  } else {
    message("⚠️ Zone 1 will not be calculated (not all required zones present).")
  }
  
  message("🎉 Zone data processed.")
  message("💾 Zone data saved globally as 'zone_data_list'.\n")
  assign("zone_data_list", zone_data, envir = .GlobalEnv)
  return(zone_data)
}
