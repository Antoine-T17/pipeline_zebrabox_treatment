process_zones <- function(enriched_data) {
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(grid)
    library(jpeg)
  }))
  
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("👋 Welcome to the Zone Processing Function!\n")
  message("This function helps you:\n")
  message("🔄 Load enriched data with periods.")
  message("📊 Filter data by user-specified zones.")
  message("🧮 Optionally calculate Zone 1 by subtracting Zone 2 from Zone 0.")
  message("💾 Save processed data for each zone as separate datasets.\n")
  
  # Explain zones to the user
  message("🔍 Configuring Zones:\n")
  message("Zones represent specific regions of the experiment:\n")
  message("- Zone 0: The outermost zone.")
  message("- Zone 1: The area between Zone 0 and Zone 2 (if applicable).")
  message("- Zone 2: The innermost zone.\n")
  message("💡 A graphical representation of the zones is available at 'inputs/docs/schema_well_zones.jpg'.")
  message("💡 Zones are defined by the 'zone' column in your raw data.")
  message("💡 If your raw data contains only 0 and 1 values in the 'zone' column, you have one zone.")
  message("💡 If your raw data contains 0, 1, and 2 values, you have two zones.")
  message("⚠️ This pipeline supports only up to two zones (0, 1, and 2).\n")
  
  # Ask the user if they want to see a visual representation
  repeat {
    show_visual <- readline(prompt = "❓ Do you need a visual representation of the zones? (yes/no): ")
    if (tolower(show_visual) %in% c("yes", "no")) {
      break
    } else {
      message("❌ Invalid input. Please enter 'yes' or 'no'.\n")
    }
  }
  
  if (tolower(show_visual) == "yes") {
    message("\n🖼️ Displaying the visual representation of the zones...")
    img_path <- "inputs/light_dark_mode/docs/schema_well_zones.jpg"
    
    if (file.exists(img_path)) {
      if (interactive()) {
        utils::browseURL(img_path)
      } else {
        message("⚠️ This is not an interactive session. Please open the image manually at: ", img_path, "\n")
      }
    } else {
      message("❌ Unable to find the image file at: ", img_path, "\n")
    }
  }
  
  # Prompt for zones
  repeat {
    zones_input <- readline(prompt = "🔢 Enter the zone numbers separated by commas (e.g., 0,1,2): ")
    zones <- as.numeric(trimws(unlist(strsplit(zones_input, ","))))
    
    if (!any(is.na(zones)) && length(zones) > 0 && all(zones %in% c(0, 1, 2))) {
      message("✔️ Zones successfully recorded: ", paste(zones, collapse = ", "), "\n")
      break
    } else {
      message("⚠️ Invalid input. Please enter valid zone numbers (e.g., 0, 1, 2). Ensure only 0, 1, and 2 are included.\n")
    }
  }
  
  calculate_zone_1 <- all(c(0, 2) %in% zones)
  
  # Process each zone
  zone_data <- list()
  for (zone in zones) {
    message(sprintf("📊 Processing Zone %d...", zone))
    zone_data[[as.character(zone)]] <- enriched_data %>% filter(an == zone)
  }
  
  # Calculate Zone 1 if applicable
  if (calculate_zone_1) {
    message("\n🧮 Calculating Zone 1 by subtracting Zone 2 from Zone 0...")
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
      message("⚠️ Could not calculate Zone 1 because Zone 0 or Zone 2 data is missing.\n")
    }
  } else {
    message("⚠️ Zone 1 will not be calculated as only Zone 0 or Zone 0 and 1 are present.\n")
  }
  
  # Final message
  message("🎉 Zone data successfully processed and saved in the global environment as 'zone_data_list'.\n")
  assign("zone_data_list", zone_data, envir = .GlobalEnv)
  
  return(zone_data)
}
