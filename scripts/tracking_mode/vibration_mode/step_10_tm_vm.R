prepare_delta_data_for_analysis <- function(
    zone_calculated_list = get("zone_calculated_list", envir = .GlobalEnv),
    output_dir = "outputs/tracking_mode/vibration_mode/figures/boxplots",
    excel_output_dir = "outputs/tracking_mode/vibration_mode/tables"
) {
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("\n👋 Welcome to the Delta Data Preparation Process!\n")
  message("This function helps you:\n")
  message("📊 Prepare data for delta analysis, focusing on ranges around period boundaries (n).")
  message("🔍 The 'delta' represents the range of data to extract around a given period boundary (n):")
  message("     - Rows corresponding to (n - delta) are labeled as 'before'.")
  message("     - Rows corresponding to (n) are labeled as 'switch'.")
  message("     - Rows corresponding to (n + delta) are labeled as 'after'.")
  message("🛠️ Your task: Define the period boundaries (n) and the delta to extract data from the dataset.")
  message("🔧 Customize your delta extraction to focus on the range of interest around period boundaries.")
  message("💾 Save processed data (including 'before' and 'after' rows) for downstream analysis and visualization.\n")
  
  # Check that 'boundary_associations' exists in the global environment
  if (!exists("boundary_associations", envir = .GlobalEnv)) {
    stop("❌ Error: 'boundary_associations' does not exist. Please define it before running this function.")
  }
  
  boundary_associations <- get("boundary_associations", envir = .GlobalEnv)
  
  # Print boundary associations for user reference
  message("📋 Available period boundaries and their associated transitions:")
  message(paste0(
    apply(boundary_associations, 1, function(row) {
      paste0("Boundary: ", row["boundary_time"], " (", row["transition"], ")")
    }),
    collapse = "\n"
  ))
  
  # Step 1: Prompt user for the period boundaries (n)
  repeat {
    boundaries_input <- readline(prompt = "🛠️ Enter one or more period boundaries (e.g., 45, 55), separated by commas: ")
    selected_boundaries <- as.numeric(trimws(unlist(strsplit(boundaries_input, ","))))
    
    # Remove NA values (in case of non-numeric input) and check validity
    selected_boundaries <- selected_boundaries[!is.na(selected_boundaries)]
    invalid_boundaries <- setdiff(selected_boundaries, boundary_associations$boundary_time)
    
    if (length(selected_boundaries) == 0) {
      message("❌ No valid numeric boundaries were entered. Please try again.")
    } else if (length(invalid_boundaries) > 0) {
      message("❌ Invalid boundaries: ", paste(invalid_boundaries, collapse = ", "))
      message("💡 Please enter valid boundaries from the list above.")
    } else {
      message("✔️ Selected boundaries: ", paste(selected_boundaries, collapse = ", "))
      break
    }
  }
  
  # Validate 'zone_calculated_list' and 'zone_combined'
  if (!is.list(zone_calculated_list) || is.null(zone_calculated_list$zone_combined)) {
    stop("❌ Error: 'zone_combined' is missing or NULL in the provided 'zone_calculated_list'. Please check your input.")
  }
  zone_combined <- zone_calculated_list$zone_combined
  
  # For convenience, store the 'start' values in a vector
  all_starts <- zone_combined$start
  
  # Step 2: Prompt user for the delta (x) and verify existence of (n-x) and (n+x) in 'start'
  repeat {
    delta_input <- readline(prompt = "Enter the delta as the numeric difference for before/after rows (e.g., 1, 2, 5): ")
    delta_input <- as.numeric(delta_input)
    
    # Check if delta_input is a positive number
    if (is.na(delta_input) || delta_input <= 0) {
      message("❌ Invalid input. Please enter a positive numeric value for x.")
      next
    }
    
    # Verify for each selected boundary n:
    #   - n - x exists in 'start'
    #   - n + x exists in 'start'
    # If any check fails, ask user to choose a new delta
    delta_is_valid_for_all <- TRUE
    
    for (boundary in selected_boundaries) {
      if (!((boundary - delta_input) %in% all_starts)) {
        message(
          "❌ For boundary ", boundary, 
          ", (n - x) = ", boundary - delta_input,
          " does not exist in the data. Please choose a different delta."
        )
        delta_is_valid_for_all <- FALSE
        break
      }
      if (!((boundary + delta_input) %in% all_starts)) {
        message(
          "❌ For boundary ", boundary, 
          ", (n + x) = ", boundary + delta_input,
          " does not exist in the data. Please choose a different delta."
        )
        delta_is_valid_for_all <- FALSE
        break
      }
    }
    
    # If delta is valid for all boundaries, break the repeat loop
    if (delta_is_valid_for_all) {
      message("✔️ Delta value set to ", delta_input, ".")
      break
    }
  }
  
  # Step 3: Filter and collect rows (n - x), n, and (n + x)
  message("🔍 Filtering data for the selected boundaries and delta...")
  filtered_data <- data.frame()
  
  for (boundary in selected_boundaries) {
    
    # Find rows where start == boundary - x and label them as "before"
    before_data <- zone_combined %>%
      filter(.data$start == boundary - delta_input) %>%
      mutate(momentum = "before")
    
    # Find rows where start == boundary and label them as "switch"
    switch_data <- zone_combined %>%
      filter(.data$start == boundary) %>%
      mutate(momentum = "switch")
    
    # Find rows where start == boundary + x and label them as "after"
    after_data <- zone_combined %>%
      filter(.data$start == boundary + delta_input) %>%
      mutate(momentum = "after")
    
    # Combine before, switch, and after rows
    filtered_data <- bind_rows(filtered_data, before_data, switch_data, after_data)
  }
  
  assign("filtered_delta_data", filtered_data, envir = .GlobalEnv)
  
  # Step 4: Calculate means
  calculate_means <- function(data) {
    data %>%
      group_by(condition_tagged, zone, momentum) %>%
      summarise(
        start                   = first(start),
        condition_grouped       = first(condition_grouped),
        animal                  = first(animal),
        condition               = first(condition),
        period                  = first(period),
        period_with_numbers     = first(period_with_numbers),
        period_without_numbers  = first(period_without_numbers),
        mean_totaldist          = mean(totaldist, na.rm = TRUE),
        mean_smldist            = mean(smldist, na.rm = TRUE),
        mean_lardist            = mean(lardist, na.rm = TRUE),
        mean_totaldur           = mean(totaldur, na.rm = TRUE),
        mean_smldur             = mean(smldur, na.rm = TRUE),
        mean_lardur             = mean(lardur, na.rm = TRUE),
        mean_totalct            = mean(totalct, na.rm = TRUE),
        mean_smlct              = mean(smlct, na.rm = TRUE),
        mean_larct              = mean(larct, na.rm = TRUE),
        mean_inact              = mean(inact, na.rm = TRUE),
        mean_inadur             = mean(inadur, na.rm = TRUE),
        mean_inadist            = mean(inadist, na.rm = TRUE),
        mean_emptyct            = mean(emptyct, na.rm = TRUE),
        mean_emptydur           = mean(emptydur, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Calculate means on filtered_delta_data
  pretreated_delta_data_for_boxplots_df <- calculate_means(filtered_data)
  
  # Step 5: Save the filtered data in the global environment
  assign("pretreated_delta_data_for_boxplots_df", pretreated_delta_data_for_boxplots_df, envir = .GlobalEnv)
  message("🎉 Data successfully filtered and saved as 'pretreated_delta_data_for_boxplots_df' in the global environment.")
  message("💾 Use this data for delta-based visualizations or further analysis.\n")
  
  return(filtered_data)
}