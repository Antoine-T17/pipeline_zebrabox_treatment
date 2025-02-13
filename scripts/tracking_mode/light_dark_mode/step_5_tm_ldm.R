# -----------------------------------------------------------
# process_zones.R
# -----------------------------------------------------------
# This function processes experimental zones from the enriched data.
# It performs the following steps:
#
#   1. Displays instructions and explains zone definitions.
#   2. Prompts the user (or uses pre-recorded input) for whether they need
#      a visual representation of the zones.
#   3. Prompts the user (or uses pre-recorded input) for the zone numbers
#      as a comma-separated string.
#   4. Filters the enriched data for each specified zone.
#   5. Optionally calculates Zone 1 by subtracting Zone 2 from Zone 0 (if both 0 and 2 are present).
#   6. Saves the processed zone data globally as 'zone_data_list'.
#
# Note: This function records its used inputs in the global list
#       'input_record_list'. Ensure that input_record_list is initialized
#       globally in your main script.
# -----------------------------------------------------------

process_zones <- function(enriched_data) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Zone Processing Function!\n")
  message("This function helps you:")
  message("  • Load enriched data with periods.")
  message("  • Filter data by user-specified zones.")
  message("  • Optionally calculate Zone 1 by subtracting Zone 2 from Zone 0.")
  message("  • Save processed data for each zone as separate datasets.\n")
  
  
  message("🔍 Configuring Zones:")
  message("  - Zone 0: The outermost zone.")
  message("  - Zone 1: The area between Zone 0 and Zone 2 (if applicable).")
  message("  - Zone 2: The innermost zone.\n")
  message("💡 A graphical representation of the zones is available at:")
  message("    'inputs/tracking_mode/light_dark_mode/docs/schema_well_zones.jpg'")
  message("💡 Zones are defined by the 'zone' column in your raw data.")
  message("💡 If your raw data contains only 0 and 1 values, you have one zone.")
  message("💡 If your raw data contains 0, 1, and 2 values, you have two zones.")
  message("⚠️ This pipeline supports only up to two zones (0, 1, and 2).\n")
  
  # Load pre-recorded inputs for zone processing.
  pipeline_inputs <- list()
  inputs_path <- "inputs/tracking_mode/light_dark_mode/inputs_values"
  inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
  inputs_file_csv <- file.path(inputs_path, "pipeline_inputs.csv")
  
  if (file.exists(inputs_file_xlsx)) {
    df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.xlsx file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  } else if (file.exists(inputs_file_csv)) {
    df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.csv file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  }
  
  # Local helper function to get input (from pre-recorded file or interactively)
  # and record the value in the global input_record_list.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      # Force the pre-recorded input to be treated as a character.
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(candidate)) {
        message("Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message("Pre-recorded input for '", param, "' is invalid. Falling back to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 1: Prompt for visual representation of zones.
  zones_vizualisation <- get_input_local("zones_vizualisation",
                                         "❓ Do you need a visual representation of the zones? (yes/no): ",
                                         validate_fn = function(x) x %in% c("yes", "y", "no", "n"),
                                         transform_fn = function(x) tolower(trimws(x)),
                                         error_msg = "❌ Invalid input. Please enter 'yes' or 'no'.")
  
  if (zones_vizualisation %in% c("yes", "y")) {
    message("🖼️ Displaying the visual representation of the zones...")
    img_path <- "inputs/tracking_mode/light_dark_mode/docs/schema_well_zones.jpg"
    if (file.exists(img_path)) {
      if (interactive()) {
        utils::browseURL(img_path)
      } else {
        message("⚠️ Non-interactive session. Please open the image manually at: ", img_path)
      }
    } else {
      message("❌ Unable to find the image file at: ", img_path, "\n")
    }
  }
  
  # Step 2: Prompt for zone numbers.
  zones_input <- get_input_local("zones_number",
                                 "🔢 Enter the zone numbers separated by commas (e.g., 0,1,2): ",
                                 validate_fn = function(x) {
                                   z <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                   (length(z) > 0) && !any(is.na(z)) && all(z %in% c(0, 1, 2))
                                 },
                                 transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                 error_msg = "⚠️ Invalid input. Please enter valid zone numbers (only 0, 1, and 2 allowed).")
  zones <- zones_input
  message("✔️ Zones successfully recorded: ", paste(zones, collapse = ", "))
  
  # Determine if Zone 1 should be calculated (if both Zone 0 and Zone 2 are present).
  calculate_zone_1 <- all(c(0, 2) %in% zones)
  
  # Step 3: Process each zone by filtering the enriched data.
  zone_data <- list()
  for (zone in zones) {
    message(sprintf("📊 Processing Zone %d...", zone))
    # Adjust the filtering column if necessary (currently filtering on 'an').
    zone_data[[as.character(zone)]] <- enriched_data %>% filter(an == zone)
  }
  
  # Step 4: Calculate Zone 1 if applicable.
  if (calculate_zone_1) {
    message("🧮 Calculating Zone 1 by subtracting Zone 2 from Zone 0...")
    numeric_columns <- c(
      "inact", "inadur", "inadist", 
      "smlct", "smldist", "smldur", 
      "larct", "lardur", "lardist", 
      "emptyct", "emptydur"
    )
    if ("0" %in% names(zone_data) && "2" %in% names(zone_data)) {
      for (col in numeric_columns) {
        zone_data[["0"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["0"]][[col]])))
        zone_data[["2"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["2"]][[col]])))
      }
      zone_data[["1"]] <- zone_data[["0"]]
      zone_data[["1"]][, numeric_columns] <- 
        zone_data[["0"]][, numeric_columns] - zone_data[["2"]][, numeric_columns]
      message("✔️ Zone 1 successfully calculated.\n")
    } else {
      message("⚠️ Could not calculate Zone 1 because Zone 0 or Zone 2 data is missing.")
    }
  } else {
    message("⚠️ Zone 1 will not be calculated as only Zone 0 or Zones 0 and 1 are present.")
  }
  
  # Step 5: Save processed zone data globally.
  message("🎉 Zone data successfully processed.")
  message("💾 Zone data has been saved globally as 'zone_data_list'.\n")
  assign("zone_data_list", zone_data, envir = .GlobalEnv)
  
  return(zone_data)
}
