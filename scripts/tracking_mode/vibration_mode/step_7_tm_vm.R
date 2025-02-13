pre_visualization_data_treatment <- function(zone_combined_data) {
  
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("\n👋 Welcome to the Data Pretreatment Process for Visualization!\n")
  message("This function helps you:\n")
  message("📋 Define the order of conditions and grouped conditions for figures.")
  message("🕵️ Remove suspect wells specified by the user.")
  message("📊 Calculate the number of wells per condition and zone.")
  message("🔄 Calculate normalized sums for conditions and zones.")
  message("  💡 Define an integration period to aggregate data over consistent time intervals.")
  message("🔊/💤 Define vibration and rest periods for filtering.")
  message("📊 Calculate mean values for boxplots.")
  message("💾 Save the outputs as datasets for line plots and box plots.\n")
  
  # Step 1: Define condition orders
  message("📋 Defining the order of conditions and condition_grouped...")
  get_valid_order <- function(prompt_message, available_items) {
    repeat {
      message("Available options: ", paste(available_items, collapse = ", "))
      user_input <- readline(prompt = prompt_message)
      selected_order <- unlist(strsplit(trimws(user_input), ","))
      selected_order <- trimws(selected_order)
      
      # Identify any missing items or invalid entries
      missing_items <- setdiff(available_items, selected_order)
      invalid_items <- setdiff(selected_order, available_items)
      
      if (length(invalid_items) > 0) {
        message("❌ Invalid entries: ", paste(invalid_items, collapse = ", "))
      }
      if (length(missing_items) > 0) {
        message("❌ Missing items: ", paste(missing_items, collapse = ", "))
        message("💡 Please include all available options in your input.")
      }
      if (length(missing_items) == 0 && length(invalid_items) == 0) {
        message("✔️ Selected order: ", paste(selected_order, collapse = ", "))
        return(selected_order)
      }
    }
  }
  
  # Prompt for condition order
  message("📋 Defining conditions...")
  available_conditions <- unique(zone_combined_data$condition)
  condition_order <- get_valid_order(
    "Enter the desired order of conditions (e.g., control_1, control_2, control_3, contaminated_1, contaminated_2, contaminated_3,...): ",
    available_conditions
  )
  
  # Prompt for condition_grouped order
  message("📋 Defining condition_grouped...")
  available_condition_grouped <- unique(zone_combined_data$condition_grouped)
  condition_grouped_order <- get_valid_order(
    "Enter the desired order of condition_grouped (e.g., control, contaminated,...): ",
    available_condition_grouped
  )
  
  # Save the orders in the global environment
  assign("generated_condition_order", condition_order, envir = .GlobalEnv)
  assign("generated_condition_grouped_order", condition_grouped_order, envir = .GlobalEnv)
  message("✔️ Condition orders saved as 'generated_condition_order' and 'generated_condition_grouped_order'.")
  
  # Step 2: Remove conditions (e.g., blank defined by "X") if the user desires
  message("🧹 Removing conditions specified by the user...")
  repeat {
    remove_conditions <- readline(prompt = "Enter the condition(s) to remove (e.g., X), separated by commas, or press Enter to skip: ")
    remove_conditions <- unlist(strsplit(trimws(remove_conditions), ","))
    remove_conditions <- trimws(remove_conditions)
    
    if (length(remove_conditions) == 0 || all(remove_conditions == "")) {
      message("✔️ No conditions removed.")
      break
    }
    
    # Check that each entered condition exists in the 'condition' column
    invalid_conditions <- remove_conditions[!remove_conditions %in% unique(zone_combined_data$condition)]
    if (length(invalid_conditions) > 0) {
      message("❌ The following conditions do not exist: ", paste(invalid_conditions, collapse = ", "))
      message("💡 Please enter valid condition names from: ", paste(unique(zone_combined_data$condition), collapse = ", "))
    } else {
      message("✔️ Conditions to remove: ", paste(remove_conditions, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!condition %in% remove_conditions)
      message("✔️ Specified conditions successfully removed.")
      break
    }
  }
  
  # Step 3: Remove suspect wells
  message("🕵️ Removing suspect wells specified by the user...")
  repeat {
    suspect_wells <- readline(prompt = "Enter the suspect wells to remove (e.g., A03, D06), separated by commas, or press Enter to skip: ")
    suspect_wells <- unlist(strsplit(trimws(suspect_wells), ","))
    suspect_wells <- trimws(suspect_wells)
    
    if (length(suspect_wells) == 0 || all(suspect_wells == "")) {
      message("✔️ No suspect wells specified.")
      break
    }
    
    # Check if all wells exist in the 'animal' column
    invalid_wells <- suspect_wells[!suspect_wells %in% zone_combined_data$animal]
    if (length(invalid_wells) > 0) {
      message("❌ The following wells do not exist in the 'animal' column: ", paste(invalid_wells, collapse = ", "))
      message("💡 Please re-enter the suspect wells.")
    } else {
      message("✔️ Wells to remove: ", paste(suspect_wells, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!animal %in% suspect_wells)
      message("✔️ Suspect wells successfully removed.")
      break
    }
  }
  
  # Step 4: Calculate the number of wells per condition and zone
  message("📊 Calculating the number of wells per condition and zone...")
  specific_minute <- 1
  wells_per_condition <- zone_combined_data %>%
    filter(!is.na(start) & start == specific_minute) %>%
    group_by(zone, condition) %>%
    summarise(n_wells = n_distinct(animal), .groups = "drop")
  
  zone_combined_data <- zone_combined_data %>%
    left_join(wells_per_condition, by = c("zone", "condition"))
  message("✔️ Well counts appended.")
  
  # Step 5: Calculate normalized sums
  message("🔄 Calculating normalized sums based on aggregation periods...")
  message("ℹ️ Aggregation Period Explanation: ")
  message("The aggregation period is the time interval (in seconds) used to group data points for normalization.")
  message("💡 For example, if you select 60 seconds, all data points within each minute will be grouped together.")
  message("💡 This is useful for aggregating data over consistent time intervals for better comparison across conditions.\n")
  
  # Prompt for aggregation period
  repeat {
    aggregation_period <- as.numeric(readline(prompt = "Enter the aggregation period in seconds (e.g., 60): "))
    if (!is.na(aggregation_period) && aggregation_period > 0) {
      message("✔️ Aggregation period set to ", aggregation_period, " seconds.")
      break
    } else {
      message("❌ Invalid input. Enter a positive numeric value.")
    }
  }
  
  # Convert aggregation period to minutes for calculations
  aggregation_period_minutes <- aggregation_period / 60
  
  # Apply the aggregation period to round and group data
  zone_combined_data <- zone_combined_data %>%
    mutate(start_rounded = floor(start / aggregation_period_minutes) * aggregation_period_minutes)
  
  normalized_sums <- zone_combined_data %>%
    group_by(condition, period_with_numbers, zone, start_rounded) %>%
    summarise(
      animal               = first(animal),
      condition            = first(condition),
      condition_grouped    = first(condition_grouped),
      condition_tagged     = first(condition_tagged),
      period               = first(period),
      period_with_numbers  = first(period_with_numbers),
      period_without_numbers = first(period_without_numbers),
      zone                 = first(zone),
      n_wells              = first(n_wells),
      sum_totaldist        = sum(totaldist, na.rm = TRUE) / n_wells,
      sum_smldist          = sum(smldist,   na.rm = TRUE) / n_wells,
      sum_lardist          = sum(lardist,   na.rm = TRUE) / n_wells,
      sum_totaldur         = sum(totaldur,  na.rm = TRUE) / n_wells,
      sum_smldur           = sum(smldur,    na.rm = TRUE) / n_wells,
      sum_lardur           = sum(lardur,    na.rm = TRUE) / n_wells,
      sum_totalct          = sum(totalct,   na.rm = TRUE) / n_wells,
      sum_smlct            = sum(smlct,     na.rm = TRUE) / n_wells,
      sum_larct            = sum(larct,     na.rm = TRUE) / n_wells,
      sum_inact            = sum(inact,     na.rm = TRUE) / n_wells,
      sum_inadur           = sum(inadur,    na.rm = TRUE) / n_wells,
      sum_inadist          = sum(inadist,   na.rm = TRUE) / n_wells,
      sum_emptyct          = sum(emptyct,   na.rm = TRUE) / n_wells,
      sum_emptydur         = sum(emptydur,  na.rm = TRUE) / n_wells,
      .groups = "drop"
    )
  
  # Step 6: Define vibration/rest periods
  message("🔊/💤 Define vibration and rest periods...")
  
  # Display available periods
  available_periods <- unique(zone_combined_data$period_with_numbers)
  message("Available periods: ", paste(available_periods, collapse = ", "))
  
  # Function to validate user input for periods
  get_valid_periods <- function(prompt_message, period_type) {
    repeat {
      user_input <- readline(prompt = prompt_message)
      selected_periods <- unlist(strsplit(trimws(user_input), ","))
      selected_periods <- trimws(selected_periods) # Remove extra spaces
      invalid_periods <- setdiff(selected_periods, available_periods)
      
      if (length(selected_periods) == 0 || any(selected_periods == "")) {
        message("❌ You must select at least one ", period_type, " period.")
      } else if (length(invalid_periods) > 0) {
        message("❌ Invalid periods: ", paste(invalid_periods, collapse = ", "))
        message("💡 Please enter valid periods from the available options: ", 
                paste(available_periods, collapse = ", "))
      } else {
        message("✔️ Selected ", period_type, " periods: ", paste(selected_periods, collapse = ", "))
        return(selected_periods)
      }
    }
  }
  
  # Ask user to select vibration periods
  message("🔊 Define vibration periods...")
  vibration_periods <- get_valid_periods("Enter vibration periods to include (e.g., vibration_1, vibration_2): ", "vibration")
  
  # Ask user to select rest periods
  message("💤 Define rest periods...")
  rest_periods <- get_valid_periods("Enter rest periods to include (e.g., rest_1, rest_2): ", "rest")
  
  # Filter the data for vibration and rest periods
  vibration_data <- zone_combined_data %>% filter(period_with_numbers %in% vibration_periods)
  rest_data <- zone_combined_data %>% filter(period_with_numbers %in% rest_periods)
  
  # Step 7: Calculate mean values for boxplots
  message("📊 Calculating mean values for vibration and rest periods...")
  vibration_data <- zone_combined_data %>% filter(period_with_numbers %in% vibration_periods)
  rest_data <- zone_combined_data %>% filter(period_with_numbers %in% rest_periods)
  
  calculate_means <- function(data) {
    data %>%
      group_by(condition_tagged, period_without_numbers, zone) %>%
      summarise(
        start_rounded           = first(start_rounded),
        condition_grouped       = first(condition_grouped),
        animal                  = first(animal),
        condition               = first(condition),
        period                  = first(period),
        period_with_numbers     = first(period_with_numbers),
        period_without_numbers  = first(period_without_numbers),
        n_wells                 = n_distinct(animal),
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
  
  vibration_boxplot_data <- calculate_means(vibration_data)
  rest_boxplot_data <- calculate_means(rest_data)
  boxplot_data <- dplyr::bind_rows(vibration_boxplot_data, rest_boxplot_data)
  
  # Save results in the global environment
  assign("pretreated_data_for_lineplots_df", normalized_sums, envir = .GlobalEnv)
  assign("pretreated_data_for_boxplots_df", boxplot_data, envir = .GlobalEnv)
  message("🎉 Pretreatment complete! Saved as:\n")
  message("  - Line plots: 'pretreated_data_for_lineplots_df'")
  message("  - Box plots: 'pretreated_data_for_boxplots_df'\n")
  
  return(list(lineplots = normalized_sums, boxplots = boxplot_data))
}
