# -----------------------------------------------------------
# File: pre_visualization_data_treatment.R
# Harmonized version of the pre_visualization_data_treatment function for vibration_mode.
# This function prepares combined zone data for visualization.
# It prompts for condition ordering, removal of conditions and suspect wells,
# calculates well counts, normalizes sums, filters vibration/rest periods,
# and saves outputs for line and box plots globally.
# -----------------------------------------------------------

pre_visualization_data_treatment <- function(zone_combined_data) {
  message("\n---\n")
  message("👋 Welcome to the Data Pretreatment Process for Visualization!\n")
  message("📋 This function will help you:")
  message("   • Define condition orders and remove unwanted data.")
  message("   • Calculate well counts and normalized sums.")
  message("   • Filter data for vibration and rest periods and prepare datasets for line plots and box plots.")
  message("   • Save outputs globally as 'pretreated_data_for_lineplots_df' and 'pretreated_data_for_boxplots_df'.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  split_and_trim <- function(x) trimws(unlist(strsplit(x, ",")))
  
  # Unified helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (length(candidate) == 1 && candidate == "") candidate <- character(0)
      message("💾 Using pre-recorded input for '", param, "': ", paste(candidate, collapse = ", "))
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (length(candidate) == 1 && candidate == "") candidate <- character(0)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ", paste(candidate, collapse = ", "))
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 1: Define condition orders.
  message("📋 Defining condition orders...")
  available_conditions <- unique(zone_combined_data$condition)
  available_condition_grouped <- unique(zone_combined_data$condition_grouped)
  
  condition_order <- get_input_local("conditions_order",
                                     "❓ Enter the desired order of conditions (comma-separated): ",
                                     validate_fn = function(x) {
                                       orders <- split_and_trim(x)
                                       missing_items <- setdiff(available_conditions, orders)
                                       invalid_items <- setdiff(orders, available_conditions)
                                       (length(missing_items) == 0) && (length(invalid_items) == 0)
                                     },
                                     transform_fn = split_and_trim)
  message("✔️ Condition order set: ", paste(condition_order, collapse = ", "))
  
  condition_grouped_order <- get_input_local("conditions_grouped_order",
                                             "❓ Enter the desired order of condition_grouped (comma-separated): ",
                                             validate_fn = function(x) {
                                               orders <- split_and_trim(x)
                                               missing_items <- setdiff(available_condition_grouped, orders)
                                               invalid_items <- setdiff(orders, available_condition_grouped)
                                               (length(missing_items) == 0) && (length(invalid_items) == 0)
                                             },
                                             transform_fn = split_and_trim)
  message("✔️ Condition_grouped order set: ", paste(condition_grouped_order, collapse = ", "))
  
  assign("generated_condition_order", condition_order, envir = .GlobalEnv)
  assign("generated_condition_grouped_order", condition_grouped_order, envir = .GlobalEnv)
  message("💾 Condition orders saved globally.")
  
  # Step 2: Remove conditions.
  message("🧹 Removing specified conditions (if any)...")
  remove_conditions <- get_input_local("remove_conditions",
                                       "❓ Enter condition(s) to remove (comma-separated), or press Enter to skip: ",
                                       validate_fn = function(x) TRUE,
                                       transform_fn = split_and_trim)
  if (length(remove_conditions) == 0) {
    message("✔️ No conditions removed.")
  } else {
    invalid_conditions <- remove_conditions[!remove_conditions %in% unique(zone_combined_data$condition)]
    if (length(invalid_conditions) > 0) {
      message("❌ The following conditions do not exist: ", paste(invalid_conditions, collapse = ", "))
      message("⚠️ No conditions were removed.")
    } else {
      message("✔️ Removing conditions: ", paste(remove_conditions, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!condition %in% remove_conditions)
    }
  }
  
  # Step 3: Remove suspect wells.
  message("🕵️ Removing suspect wells (if specified)...")
  remove_suspect_well <- get_input_local("remove_suspect_well",
                                         "❓ Enter suspect wells to remove (comma-separated), or press Enter to skip: ",
                                         validate_fn = function(x) TRUE,
                                         transform_fn = split_and_trim)
  if (length(remove_suspect_well) == 0) {
    message("✔️ No suspect wells specified.")
  } else {
    invalid_wells <- remove_suspect_well[!remove_suspect_well %in% zone_combined_data$animal]
    if (length(invalid_wells) > 0) {
      message("❌ The following wells do not exist: ", paste(invalid_wells, collapse = ", "))
      message("⚠️ Skipping removal of suspect wells.")
    } else {
      message("✔️ Removing suspect wells: ", paste(remove_suspect_well, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!animal %in% remove_suspect_well)
    }
  }
  
  # Step 4: Calculate the number of wells per condition and zone.
  message("📊 Calculating well counts per condition and zone...")
  specific_minute <- 1
  wells_per_condition <- zone_combined_data %>% filter(!is.na(start) & start == specific_minute) %>%
    group_by(zone, condition) %>% summarise(n_wells = n_distinct(animal), .groups = "drop")
  zone_combined_data <- zone_combined_data %>% left_join(wells_per_condition, by = c("zone", "condition"))
  message("✔️ Well counts appended.")
  
  # Step 5: Calculate normalized sums.
  message("🛠️ Calculating normalized sums using aggregation period...")
  aggregation_period <- as.numeric(get_input_local("aggregation_period",
                                                   "❓ Enter aggregation period in seconds (e.g., 60): ",
                                                   validate_fn = function(x) !is.na(as.numeric(x)) && as.numeric(x) > 0,
                                                   transform_fn = function(x) as.numeric(trimws(x))))
  message("✔️ Aggregation period set to ", aggregation_period, " seconds.")
  aggregation_period_minutes <- aggregation_period / 60
  zone_combined_data <- zone_combined_data %>% mutate(start_rounded = floor(start / aggregation_period_minutes) * aggregation_period_minutes)
  normalized_sums <- zone_combined_data %>% group_by(condition, period_with_numbers, zone, start_rounded) %>%
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
      sum_smldist          = sum(smldist, na.rm = TRUE) / n_wells,
      sum_lardist          = sum(lardist, na.rm = TRUE) / n_wells,
      sum_totaldur         = sum(totaldur, na.rm = TRUE) / n_wells,
      sum_smldur           = sum(smldur, na.rm = TRUE) / n_wells,
      sum_lardur           = sum(lardur, na.rm = TRUE) / n_wells,
      sum_totalct          = sum(totalct, na.rm = TRUE) / n_wells,
      sum_smlct            = sum(smlct, na.rm = TRUE) / n_wells,
      sum_larct            = sum(larct, na.rm = TRUE) / n_wells,
      sum_inact            = sum(inact, na.rm = TRUE) / n_wells,
      sum_inadur           = sum(inadur, na.rm = TRUE) / n_wells,
      sum_inadist          = sum(inadist, na.rm = TRUE) / n_wells,
      sum_emptyct          = sum(emptyct, na.rm = TRUE) / n_wells,
      sum_emptydur         = sum(emptydur, na.rm = TRUE) / n_wells,
      .groups = "drop"
    )
  
  # Step 6: Define vibration and rest periods.
  message("🔊/💤 Define vibration and rest periods...")
  available_periods <- unique(zone_combined_data$period_with_numbers)
  message("ℹ️ Available periods: ", paste(available_periods, collapse = ", "))
  
  # For vibration periods, accept any input that starts with "vibration"
  vibration_period <- get_input_local("vibration_period",
                                      "❓ Enter vibration periods to include (comma-separated): ",
                                      validate_fn = function(x) {
                                        periods <- split_and_trim(x)
                                        length(periods) > 0 && all(grepl("^vibration", periods, ignore.case = TRUE))
                                      },
                                      transform_fn = split_and_trim,
                                      error_msg = "❌ Invalid vibration periods. Please enter values starting with 'vibration'.")
  message("✔️ Selected vibration periods: ", paste(vibration_period, collapse = ", "))
  
  # For rest periods, accept any input that starts with "rest"
  rest_period <- get_input_local("rest_period",
                                 "❓ Enter rest periods to include (comma-separated): ",
                                 validate_fn = function(x) {
                                   periods <- split_and_trim(x)
                                   length(periods) > 0 && all(grepl("^rest", periods, ignore.case = TRUE))
                                 },
                                 transform_fn = split_and_trim,
                                 error_msg = "❌ Invalid rest periods. Please enter values starting with 'rest'.")
  message("✔️ Selected rest periods: ", paste(rest_period, collapse = ", "))
  
  vibration_data <- zone_combined_data %>% filter(period_with_numbers %in% vibration_period)
  rest_data  <- zone_combined_data %>% filter(period_with_numbers %in% rest_period)
  
  # Step 7: Calculate mean values for boxplots.
  message("📊 Calculating mean values for boxplot data...")
  calculate_means <- function(data) {
    data %>% group_by(condition_tagged, period_without_numbers, zone) %>%
      summarise(
        start_rounded = first(start_rounded),
        condition_grouped = first(condition_grouped),
        animal = first(animal),
        condition = first(condition),
        period = first(period),
        period_with_numbers = first(period_with_numbers),
        period_without_numbers = first(period_without_numbers),
        n_wells = first(n_wells),
        mean_totaldist = mean(totaldist, na.rm = TRUE),
        mean_smldist = mean(smldist, na.rm = TRUE),
        mean_lardist = mean(lardist, na.rm = TRUE),
        mean_totaldur = mean(totaldur, na.rm = TRUE),
        mean_smldur = mean(smldur, na.rm = TRUE),
        mean_lardur = mean(lardur, na.rm = TRUE),
        mean_totalct = mean(totalct, na.rm = TRUE),
        mean_smlct = mean(smlct, na.rm = TRUE),
        mean_larct = mean(larct, na.rm = TRUE),
        mean_inact = mean(inact, na.rm = TRUE),
        mean_inadur = mean(inadur, na.rm = TRUE),
        mean_inadist = mean(inadist, na.rm = TRUE),
        mean_emptyct = mean(emptyct, na.rm = TRUE),
        mean_emptydur = mean(emptydur, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  vibration_boxplot_data <- calculate_means(vibration_data)
  rest_boxplot_data  <- calculate_means(rest_data)
  boxplot_data <- dplyr::bind_rows(vibration_boxplot_data, rest_boxplot_data)
  
  assign("pretreated_data_for_lineplots_df", normalized_sums, envir = .GlobalEnv)
  assign("pretreated_data_for_boxplots_df", boxplot_data, envir = .GlobalEnv)
  message("🎉 Pretreatment complete!")
  message("💾 Line plot data: 'pretreated_data_for_lineplots_df'")
  message("💾 Box plot data: 'pretreated_data_for_boxplots_df'\n")
  
  return(list(lineplots = normalized_sums, boxplots = boxplot_data))
}
