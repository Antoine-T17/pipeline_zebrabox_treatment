calculate_and_clean_zone_data <- function(zone_data_list) {
  # Step 1: Display welcome message with bullet points.
  message("\n---\n")
  message("👋 Welcome to the Zone Calculation and Cleaning Process!")
  message("📋 This function will help you:")
  message("   • Process and clean zone-specific data for each plate.")
  message("   • Calculate new numeric variables and convert 'start' time to minutes.")
  message("   • Optionally remove rows where 'start' exceeds a user-specified threshold (in seconds).")
  message("   • Reorder columns and combine zones for each plate.")
  message("   • Save the final result globally as 'zone_calculated_list'.\n")
  
  # Step 2: Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Step 3: Define a helper function to obtain and validate user inputs.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
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
  
  # Step 4: Determine the unit of the 'start' column.
  start_unit <- get_input_local("start_column_unit",
                                "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ",
                                validate_fn = function(x) tolower(x) %in% c("h", "m", "s"),
                                transform_fn = function(x) tolower(trimws(x)),
                                error_msg = "❌ Please enter 'h', 'm', or 's'.")
  
  # Step 5: Process each plate’s zone data (zone_data_list is a list of plates;
  # each element is itself a list with keys like "0", "1", "2").
  processed_zones_list <- list()  # one element per plate
  for (p in seq_along(zone_data_list)) {
    message(sprintf("\n🛠️ Processing zones for Plate %d", p))
    plate_zone_data <- zone_data_list[[p]]
    processed_zones <- list()  # will store processed zone data for this plate
    # Process each zone for the current plate.
    for (zone_name in names(plate_zone_data)) {
      message(sprintf("   🛠️ Processing Zone %s for Plate %d...", zone_name, p))
      zone_data <- plate_zone_data[[zone_name]]
      
      # Step 6: Calculate new numeric variables.
      message("      Calculating new variables...")
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
      message("      New variables calculated.")
      
      # Step 7: Convert the 'start' column to minutes.
      message("      Converting 'start' column to minutes...")
      if ("start" %in% colnames(zone_data)) {
        zone_data$start <- as.numeric(gsub(",", ".", as.character(zone_data$start)))
        if (start_unit == "h") {
          zone_data <- zone_data %>% mutate(start = start * 60)
          message("      'start' converted from hours to minutes.")
        } else if (start_unit == "s") {
          zone_data <- zone_data %>% mutate(start = start / 60)
          message("      'start' converted from seconds to minutes.")
        } else {
          message("      'start' assumed to be in minutes; no conversion done.")
        }
      } else {
        message("      'start' column not found; skipping conversion.")
      }
      
      # Step 8: Optionally remove rows based on a user-specified threshold.
      # Prompt the user with "remove_time": either "no" or a numeric value in seconds.
      message("      Optional row removal based on 'start' threshold.")
      remove_time <- get_input_local("remove_time",
                                     "❓ Enter the removal threshold for 'start' in seconds (or type 'no' to skip removal): ",
                                     validate_fn = function(x) {
                                       x_trim <- tolower(trimws(x))
                                       if(x_trim == "no") return(TRUE)
                                       else return(!is.na(as.numeric(x_trim)) && as.numeric(x_trim) > 0)
                                     },
                                     transform_fn = function(x) trimws(x),
                                     error_msg = "❌ Please enter a positive number or 'no'.")
      if (tolower(remove_time) != "no") {
        remove_sec <- as.numeric(remove_time)
        threshold_min <- remove_sec / 60
        message("      Removing rows where 'start' is greater or equal to ", threshold_min, " minutes.")
        zone_data <- zone_data %>% filter(start < threshold_min)
        message("      Rows removed based on the threshold.")
      } else {
        message("      No removal threshold provided; keeping all rows.")
      }
      
      # Step 9: Add the 'zone' column.
      message("      Adding 'zone' column...")
      zone_data <- zone_data %>% mutate(zone = zone_name)
      message("      'zone' column added.")
      
      # Step 10: Filter and reorder columns.
      message("      Filtering and reordering columns...")
      desired_columns <- c("animal", "condition", "condition_grouped", "condition_tagged",
                           "period", "period_with_numbers", "period_without_numbers",
                           "zone", "start", 
                           "inact", "inadur", "inadist", "emptyct", "emptydur",
                           "smlct", "larct", "totalct",
                           "smldur", "lardur", "totaldur",
                           "smldist", "lardist", "totaldist")
      zone_data <- zone_data %>% select(any_of(desired_columns))
      message("      Columns filtered and reordered.")
      
      processed_zones[[zone_name]] <- zone_data
    }
    processed_zones_list[[p]] <- processed_zones
  }
  
  # Step 11: Combine processed zone data for each plate.
  # For each plate, if more than one zone exists, combine them; otherwise, just use the single zone data.
  combined_per_plate <- list()
  for (p in seq_along(processed_zones_list)) {
    plate_zones <- processed_zones_list[[p]]
    # If there is more than one zone, use bind_rows; if only one, take that dataframe.
    if (length(plate_zones) > 1) {
      combined_per_plate[[p]] <- dplyr::bind_rows(plate_zones)
    } else {
      combined_per_plate[[p]] <- plate_zones[[1]]
    }
  }
  # Optionally, you could combine across plates if needed:
  # zone_combined <- dplyr::bind_rows(combined_per_plate)
  # For now, we save both the per-plate processed zones and the combined per-plate data.
  
  # Step 12: Finalize and save processed data globally.
  message("\n🎉 Zone calculation and cleaning completed!")
  message("💾 Processed zone data saved globally as 'zone_calculated_list'.\n")
  assign("zone_calculated_list", list(processed_zones = processed_zones_list,
                                      combined_per_plate = combined_per_plate), envir = .GlobalEnv)
  
  return(list(processed_zones = processed_zones_list, combined_per_plate = combined_per_plate))
}
