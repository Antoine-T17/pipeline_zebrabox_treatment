calculate_and_clean_zone_data <- function(zone_data_list) {
  
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("\n👋 Welcome to the Zone Calculation and Cleaning Process!\n")
  message("This function helps you:\n")
  message("🔄 Load zone-specific data.")
  message("📊 Calculate new variables for each zone.")
  message("⏱️ Convert the 'start' column based on user-defined units.")
  message("🗑️ Remove rows corresponding to the last minute.")
  message("🛠️ Add a `zone` column and reorder columns.")
  message("📊 Combine all zones into a single dataframe.")
  message("💾 Save cleaned and processed data in the global environment.\n")
  
  processed_zones <- list()
  
  # Step 1: Ask the user for the unit of the 'start' column
  repeat {
    start_unit <- readline(prompt = "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ")
    if (tolower(start_unit) %in% c("h", "m", "s")) {
      break
    } else {
      message("❌ Invalid input. Please enter 'h', 'm', or 's'.")
    }
  }
  
  for (zone_name in names(zone_data_list)) {
    message("\n📂 Processing Zone: ", zone_name)
    zone_data <- zone_data_list[[zone_name]]
    message("✔️ Zone data successfully loaded.")
    
    # Step 2: Calculate new variables
    message("📊 Calculating new variables...")
    numeric_columns <- c("smldist", "lardist", "smldur", "lardur", "smlct", "larct")
    
    for (col in numeric_columns) {
      if (col %in% colnames(zone_data)) {
        zone_data[[col]] <- gsub(",", ".", as.character(zone_data[[col]]))
        zone_data[[col]] <- as.numeric(zone_data[[col]])
      }
    }
    
    zone_data <- zone_data %>%
      mutate(
        totaldist = ifelse(!is.na(smldist) & !is.na(lardist), smldist + lardist, NA),
        totaldur  = ifelse(!is.na(smldur) & !is.na(lardur),  smldur + lardur,   NA),
        totalct   = ifelse(!is.na(smlct) & !is.na(larct),    smlct + larct,     NA)
      )
    message("✔️ Variables successfully calculated.")
    
    # Step 3: Convert the 'start' column
    message("⏱️ Converting 'start' column based on user-defined units...")
    if ("start" %in% colnames(zone_data)) {
      zone_data$start <- gsub(",", ".", as.character(zone_data$start))
      zone_data$start <- as.numeric(zone_data$start)
      
      if (tolower(start_unit) == "h") {
        zone_data <- zone_data %>% mutate(start = start * 60)  # Convert hours to minutes
        message("✔️ 'start' column converted from hours to minutes.")
      } else if (tolower(start_unit) == "s") {
        zone_data <- zone_data %>% mutate(start = start / 60)  # Convert seconds to minutes
        message("✔️ 'start' column converted from seconds to minutes.")
      } else {
        message("✔️ 'start' column is already in minutes. No conversion needed.")
      }
    } else {
      message("⚠️ 'start' column not found in the dataset. Skipping conversion.")
    }
    
    # Step 4: Remove rows for the last minute
    message("⏱️ Removing rows corresponding to the last minute...")
    if ("start" %in% colnames(zone_data)) {
      max_start <- max(zone_data$start, na.rm = TRUE)
      message("📊 The last minute identified is: ", max_start)
      
      if (any(zone_data$start == max_start)) {
        zone_data <- zone_data %>% filter(start < max_start)
        message("✔️ Rows corresponding to the last minute successfully removed.")
      } else {
        message("⚠️ No rows found for the last minute.")
      }
    }
    
    # Step 5: Add the `zone` column
    message("🛠️ Adding the `zone` column...")
    zone_data <- zone_data %>%
      mutate(zone = zone_name) %>%
      relocate(zone, .after = period_without_numbers)
    message("✔️ `zone` column successfully added.")
    
    # Step 6: Filter and reorder columns
    message("🛠️ Filtering and reordering columns...")
    desired_columns <- c(
      "animal", "condition", "condition_grouped", "condition_tagged",
      "period", "period_with_numbers", "period_without_numbers",
      "zone", "start", 
      "inact", "inadur", "inadist", 
      "emptyct", "emptydur",
      "smlct", "larct", "totalct",
      "smldur", "lardur", "totaldur",
      "smldist", "lardist", "totaldist"
    )
    
    zone_data <- zone_data %>% select(any_of(desired_columns))
    message("✔️ Columns successfully filtered and reordered.")
    
    processed_zones[[zone_name]] <- zone_data
  }
  
  # Step 7: Combine all processed zones
  message("📊 Combining all processed zones into a single dataframe...")
  zone_combined <- bind_rows(processed_zones)
  message("✔️ All zones successfully combined.")
  
  # Final message
  message("🎉 All zone data has been successfully processed.")
  message("💾 Zone data has been saved in the global environment as 'zone_calculated_list'.\n")
  assign("zone_calculated_list", list(processed_zones = processed_zones, zone_combined   = zone_combined), envir = .GlobalEnv)
  
  return(list(processed_zones = processed_zones, zone_combined = zone_combined))
}
