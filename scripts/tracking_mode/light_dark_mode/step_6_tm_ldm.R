# -----------------------------------------------------------
# File: calculate_and_clean_zone_data.R
# -----------------------------------------------------------
# Harmonized version of the calculate_and_clean_zone_data function for light_dark mode.
# Harmonized version of the calculate_and_clean_zone_data function.
# This function processes each zone’s data by computing new variables,
# converting the 'start' column to minutes (using a user-specified unit),
# filtering out the last minute, reordering columns, and combining all zones.
# The final result is saved globally as 'zone_calculated_list'.
# -----------------------------------------------------------

calculate_and_clean_zone_data <- function(zone_data_list) {
  message("\n---\n")
  message("👋 Welcome to the Zone Calculation and Cleaning Process!\n")
  message("📋 This function will help you:")
  message("   • Process and clean zone-specific data.")
  message("   • Calculate new numeric variables and convert 'start' time to minutes.")
  message("   • Remove the last minute of data and reorder columns.")
  message("   • Combine all zones into a single dataset and save globally as 'zone_calculated_list'.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Unified helper.
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
  
  # Step 1: Determine the unit of the 'start' column.
  start_unit <- get_input_local("start_column_unit",
                                "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ",
                                validate_fn = function(x) tolower(x) %in% c("h", "m", "s"),
                                transform_fn = function(x) tolower(trimws(x)),
                                error_msg = "❌ Please enter 'h', 'm', or 's'.")
  
  processed_zones <- list()
  for (zone_name in names(zone_data_list)) {
    message("\n🛠️ Processing Zone: ", zone_name)
    zone_data <- zone_data_list[[zone_name]]
    message("✔️ Zone data loaded.")
    
    # Step 2: Calculate new numeric variables.
    message("🛠️ Calculating new variables...")
    numeric_columns <- c("smldist", "lardist", "smldur", "lardur", "smlct", "larct")
    for (col in numeric_columns) {
      if (col %in% colnames(zone_data)) {
        zone_data[[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[[col]])))
      }
    }
    zone_data <- zone_data %>%
      mutate(
        totaldist = ifelse(!is.na(smldist) & !is.na(lardist), smldist + lardist, NA),
        totaldur  = ifelse(!is.na(smldur) & !is.na(lardur), smldur + lardur, NA),
        totalct   = ifelse(!is.na(smlct) & !is.na(larct), smlct + larct, NA)
      )
    message("✔️ New variables calculated.")
    
    # Step 3: Convert the 'start' column.
    message("🛠️ Converting 'start' column to minutes...")
    if ("start" %in% colnames(zone_data)) {
      zone_data$start <- as.numeric(gsub(",", ".", as.character(zone_data$start)))
      if (start_unit == "h") {
        zone_data <- zone_data %>% mutate(start = start * 60)
        message("✔️ 'start' converted from hours to minutes.")
      } else if (start_unit == "s") {
        zone_data <- zone_data %>% mutate(start = start / 60)
        message("✔️ 'start' converted from seconds to minutes.")
      } else {
        message("✔️ 'start' assumed to be in minutes; no conversion done.")
      }
    } else {
      message("⚠️ 'start' column not found; skipping conversion.")
    }
    
    # Step 4: Remove rows corresponding to the last minute.
    message("🛠️ Removing rows from the last minute...")
    if ("start" %in% colnames(zone_data)) {
      max_start <- max(zone_data$start, na.rm = TRUE)
      message("ℹ️ Last minute value: ", max_start)
      zone_data <- zone_data %>% filter(start < max_start)
      message("✔️ Last minute rows removed.")
    } else {
      message("⚠️ 'start' column missing; skipping row removal.")
    }
    
    # Step 5: Add the 'zone' column and reorder.
    message("🛠️ Adding 'zone' column...")
    zone_data <- zone_data %>% mutate(zone = zone_name) %>% relocate(zone, .after = period_without_numbers)
    message("✔️ 'zone' column added.")
    
    # Step 6: Filter and reorder columns.
    message("🛠️ Filtering and reordering columns...")
    desired_columns <- c("animal", "condition", "condition_grouped", "condition_tagged",
                         "period", "period_with_numbers", "period_without_numbers",
                         "zone", "start", 
                         "inact", "inadur", "inadist", "emptyct", "emptydur",
                         "smlct", "larct", "totalct",
                         "smldur", "lardur", "totaldur",
                         "smldist", "lardist", "totaldist")
    zone_data <- zone_data %>% select(any_of(desired_columns))
    message("✔️ Columns filtered and reordered.")
    
    processed_zones[[zone_name]] <- zone_data
  }
  
  message("🛠️ Combining processed zone data...")
  zone_combined <- dplyr::bind_rows(processed_zones)
  message("✔️ Zones combined successfully.")
  
  message("🎉 Zone calculation and cleaning completed!")
  message("💾 Processed zone data saved globally as 'zone_calculated_list'.\n")
  assign("zone_calculated_list", list(processed_zones = processed_zones, zone_combined = zone_combined), envir = .GlobalEnv)
  
  return(list(processed_zones = processed_zones, zone_combined = zone_combined))
}
