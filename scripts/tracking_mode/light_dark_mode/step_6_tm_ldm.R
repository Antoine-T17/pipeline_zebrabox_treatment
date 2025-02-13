# -----------------------------------------------------------
# calculate_and_clean_zone_data.R
# -----------------------------------------------------------
# This function processes and cleans zone-specific data.
# It performs the following steps:
#
#   1. Prompts for the unit of the 'start' column (h, m, or s)
#      and records this input as "start_column_unit".
#   2. For each zone in zone_data_list:
#         a. Calculates new numeric variables (e.g., total distance, duration, count).
#         b. Converts the 'start' column to minutes based on the specified unit.
#         c. Removes rows corresponding to the last minute.
#         d. Adds a new 'zone' column and reorders columns.
#   3. Combines all processed zones into a single dataframe.
#   4. Saves the processed zones and combined data globally as 'zone_calculated_list'.
#
# Note: This function records its used inputs in the global list
#       'input_record_list'. Ensure that input_record_list is initialized
#       in your main script.
# -----------------------------------------------------------

calculate_and_clean_zone_data <- function(zone_data_list) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Zone Calculation and Cleaning Process!\n")
  message("This function helps you:")
  message("  • Load zone-specific data.")
  message("  • Calculate new variables for each zone.")
  message("  • Convert the 'start' column based on user-defined units.")
  message("  • Remove rows corresponding to the last minute.")
  message("  • Add a `zone` column and reorder columns.")
  message("  • Combine all zones into a single dataframe.")
  message("  • Save cleaned and processed data in the global environment.\n")
  
  
  # Load pre-recorded inputs for zone cleaning.
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
  
  # Local helper to get input (from pre-recorded file or interactively)
  # and record the value in the global input_record_list.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
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
  
  # Step 1: Ask for the unit of the 'start' column.
  start_unit <- get_input_local("start_column_unit",
                                "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ",
                                validate_fn = function(x) tolower(x) %in% c("h", "m", "s"),
                                transform_fn = function(x) tolower(trimws(x)),
                                error_msg = "❌ Invalid input. Please enter 'h', 'm', or 's'.")
  
  processed_zones <- list()
  
  # Process each zone in the provided zone_data_list.
  for (zone_name in names(zone_data_list)) {
    message("\n📂 Processing Zone: ", zone_name)
    zone_data <- zone_data_list[[zone_name]]
    message("✔️ Zone data successfully loaded.")
    
    # Step 2: Calculate new numeric variables.
    message("📊 Calculating new variables...")
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
    message("✔️ Variables successfully calculated.")
    
    # Step 3: Convert the 'start' column.
    message("⏱️ Converting 'start' column based on user-defined units...")
    if ("start" %in% colnames(zone_data)) {
      zone_data$start <- as.numeric(gsub(",", ".", as.character(zone_data$start)))
      
      if (start_unit == "h") {
        zone_data <- zone_data %>% mutate(start = start * 60)  # Convert hours to minutes
        message("✔️ 'start' column converted from hours to minutes.")
      } else if (start_unit == "s") {
        zone_data <- zone_data %>% mutate(start = start / 60)  # Convert seconds to minutes
        message("✔️ 'start' column converted from seconds to minutes.")
      } else {
        message("✔️ 'start' column is assumed to be in minutes. No conversion needed.")
      }
    } else {
      message("⚠️ 'start' column not found in the dataset. Skipping conversion.")
    }
    
    # Step 4: Remove rows corresponding to the last minute.
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
    
    # Step 5: Add the `zone` column.
    message("🛠️ Adding the `zone` column...")
    zone_data <- zone_data %>%
      mutate(zone = zone_name) %>%
      relocate(zone, .after = period_without_numbers)
    message("✔️ `zone` column successfully added.")
    
    # Step 6: Filter and reorder columns.
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
  
  # Step 7: Combine all processed zones.
  message("📊 Combining all processed zones into a single dataframe...")
  zone_combined <- bind_rows(processed_zones)
  message("✔️ All zones successfully combined.")
  
  # Final Step: Save results globally.
  message("🎉 All zone data has been successfully processed.")
  message("💾 Zone data has been saved globally as 'zone_calculated_list'.\n")
  assign("zone_calculated_list", list(processed_zones = processed_zones, zone_combined = zone_combined), envir = .GlobalEnv)
  
  return(list(processed_zones = processed_zones, zone_combined = zone_combined))
}
