# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : vibration mode
# Function: process_zones
# Purpose: Processes experimental zones from enriched data.
#          It displays zone definitions, optionally shows a visual schema,
#          prompts for zone numbers, filters the data by zone, optionally
#          calculates Zone 1 (if Zones 0 and 2 are present), and saves the
#          processed data globally as 'zone_data_list'.
#          Now supports multiple plates by processing each plate separately.
# -----------------------------------------------------------
process_zones <- function(enriched_data_list) {
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
  message("ℹ️ See 'inputs/tracking_mode/vibration_mode/docs/schema_well_zones.jpg' for a visual schema.\n")
  
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Helper function that forces manual prompt mode if input is empty
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
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
      # If input is empty, it forces manual re-entry
      if (user_input == "") {
        message("⚠️ Empty input detected. Please enter a valid value.")
        next
      }
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
  
  # Optionally display the zone visualization image.
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
  
  # Prompt for zone numbers.
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
  
  zone_data_list <- list()  # will store processed zone data per plate
  
  for (i in seq_along(enriched_data_list)) {
    message(sprintf("\n🛠️ Processing zones for plate %d...", i))
    edata <- enriched_data_list[[i]]
    
    # Process zones for current plate: assume column 'an' indicates zone number.
    zone_data <- list()
    for (zone in zones) {
      message(sprintf("🛠️ Processing Zone %d for plate %d...", zone, i))
      zone_data[[as.character(zone)]] <- edata %>% dplyr::filter(an == zone)
    }
    
    # Calculate Zone 1 if Zones 0 and 2 are present.
    if (all(c(0, 2) %in% zones)) {
      message(sprintf("🧮 Calculating Zone 1 for plate %d by subtracting Zone 2 from Zone 0...", i))
      numeric_columns <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", 
                           "larct", "lardur", "lardist", "emptyct", "emptydur")
      if ("0" %in% names(zone_data) && "2" %in% names(zone_data)) {
        for (col in numeric_columns) {
          zone_data[["0"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["0"]][[col]])))
          zone_data[["2"]][[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[["2"]][[col]])))
        }
        zone_data[["1"]] <- zone_data[["0"]]
        zone_data[["1"]][, numeric_columns] <- zone_data[["0"]][, numeric_columns] - zone_data[["2"]][, numeric_columns]
        message(sprintf("✔️ Zone 1 calculated successfully for plate %d.", i))
      } else {
        message(sprintf("⚠️ Unable to calculate Zone 1 for plate %d: missing Zone 0 or Zone 2 data.", i))
      }
    } else {
      message(sprintf("⚠️ Zone 1 will not be calculated for plate %d (not all required zones present).", i))
    }
    zone_data_list[[i]] <- zone_data
  }
  
  message("\n🎉 Zone data processed.")
  message("💾 Zone data saved globally as 'zone_data_list'.\n")
  assign("zone_data_list", zone_data_list, envir = .GlobalEnv)
  return(zone_data_list)
}
