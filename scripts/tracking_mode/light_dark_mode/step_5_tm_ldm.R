process_zones <- function(enriched_data_list) {
  # Display welcome message with zone overview.
  message("\n---\n")
  message("👋 Welcome to the Zone Processing Function!")
  message("📋 This function will help you:")
  message("   • Filter enriched data by user-specified zones.")
  message("   • Optionally display a visual representation of zones.")
  message("   • Optionally calculate Zone 1 if Zones 0 and 2 are present.")
  message("   • Save processed zone data globally as 'zone_data_list'.\n")
  message("ℹ️ Zones overview:")
  message("   - Zone 0: Outermost zone.")
  message("   - Zone 1: Between Zone 0 and Zone 2 (if applicable).")
  message("   - Zone 2: Innermost zone.\n")
  message("ℹ️ See 'inputs/tracking_mode/light_dark_mode/docs/schema_well_zones.jpg' for a visual schema.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Helper function to obtain and validate user inputs.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    # Check for pre-recorded input: must not be NULL, NA, or an empty string.
    if (!is.null(pipeline_inputs[[param]]) && 
        !is.na(pipeline_inputs[[param]]) && 
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(candidate)) {
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
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
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 4: Optionally display the zone visualization image.
  zones_vizualisation <- get_input_local("zones_vizualisation",
                                         "❓ Do you need a visual representation of the zones? (yes/no): ",
                                         validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                         transform_fn = function(x) tolower(trimws(x)),
                                         error_msg = "❌ Please enter 'yes' or 'no'.")
  if (zones_vizualisation %in% c("yes", "y")) {
    message("🖼️ Displaying zone visualization...")
    img_path <- "inputs/tracking_mode/light_dark_mode/docs/schema_well_zones.jpg"
    if (file.exists(img_path)) {
      if (interactive()) {
        browseURL(img_path)
      } else {
        message("⚠️ Non-interactive session. Open the image manually at: ", img_path)
      }
    } else {
      message("❌ Image not found at: ", img_path)
    }
  }
  
  # Step 5: Prompt for zone numbers.
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
  
  # Step 6: Determine if Zone 1 should be calculated.
  calculate_zone_1 <- all(c(0, 2) %in% zones)
  
  # Step 7: Process each enriched data frame (each plate) individually.
  zone_data_list <- list()
  for (p in seq_along(enriched_data_list)) {
    message(sprintf("\n--- Processing Zones for Plate %d ---", p))
    df <- enriched_data_list[[p]]
    
    # Process zones for the current plate.
    zone_data <- list()
    for (zone in zones) {
      message(sprintf("🛠️ Processing Zone %d for Plate %d...", zone, p))
      # Assuming the column 'an' in df indicates the zone.
      zone_data[[as.character(zone)]] <- df[ df$an == zone, ]
    }
    
    # Step 8: Calculate Zone 1 if required for the current plate.
    if (calculate_zone_1) {
      message("🧮 Calculating Zone 1 by subtracting Zone 2 from Zone 0 for Plate ", p, "...")
      numeric_columns <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", 
                           "larct", "lardur", "lardist", "emptyct", "emptydur")
      if ("0" %in% names(zone_data) && "2" %in% names(zone_data)) {
        for (col in numeric_columns) {
          zone_data[["0"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["0"]][[col]])))
          zone_data[["2"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["2"]][[col]])))
        }
        zone_data[["1"]] <- zone_data[["0"]]
        zone_data[["1"]][, numeric_columns] <- zone_data[["0"]][, numeric_columns] - zone_data[["2"]][, numeric_columns]
        message("✔️ Zone 1 calculated successfully for Plate ", p, ".")
      } else {
        message("⚠️ Unable to calculate Zone 1 for Plate ", p, ": missing Zone 0 or Zone 2 data.")
      }
    } else {
      message("⚠️ Zone 1 will not be calculated for Plate ", p, " (not all required zones present).")
    }
    
    zone_data_list[[p]] <- zone_data
  }
  
  message("\n🎉 Zone data processed for all plates!")
  message("💾 Zone data saved globally as 'zone_data_list'.\n")
  assign("zone_data_list", zone_data_list, envir = .GlobalEnv)
  return(zone_data_list)
}
