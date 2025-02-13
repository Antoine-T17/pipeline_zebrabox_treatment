# -----------------------------------------------------------
# pre_visualization_data_treatment.R
# -----------------------------------------------------------
# This function prepares the combined zone data for visualization.
# It performs the following tasks:
#
#   1. Prompts the user (or uses pre-recorded inputs) to define:
#         - The desired order of conditions (conditions_order)
#         - The desired order of condition_grouped (conditions_grouped_order)
#         - Conditions to remove (remove_conditions)
#         - Suspect wells to remove (remove_suspect_well)
#         - The aggregation period in seconds (aggregation_period)
#         - The light periods to include (light_period)
#         - The dark periods to include (dark_period)
#   2. Saves the condition orders globally.
#   3. Removes unwanted conditions and suspect wells from the data.
#   4. Calculates the number of wells per condition and zone.
#   5. Rounds the 'start' time based on the aggregation period and calculates normalized sums.
#   6. Filters the data for light and dark periods and calculates mean values for boxplots.
#   7. Saves the outputs in the global environment as datasets for line plots and box plots.
#
# Note: This function records its used inputs in the global list 
#       'input_record_list'. Ensure that input_record_list is initialized 
#       in your main script.
# -----------------------------------------------------------

pre_visualization_data_treatment <- function(zone_combined_data) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Data Pretreatment Process for Visualization!\n")
  message("This function helps you:")
  message("  • Define the order of conditions and condition_grouped for figures.")
  message("  • Remove suspect wells specified by the user.")
  message("  • Calculate the number of wells per condition and zone.")
  message("  • Calculate normalized sums for conditions and zones using an integration period to aggregate data over consistent time intervals.")
  message("  • Define light and dark periods for filtering.")
  message("  • Calculate mean values for boxplots.")
  message("  • Save the outputs as datasets for line plots and box plots.\n")
  
  # ---------------------------
  # Load pre-recorded inputs.
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
  
  # ---------------------------
  # Helper: Split a comma-separated string and trim whitespace.
  split_and_trim <- function(x) {
    res <- unlist(strsplit(x, ","))
    trimws(res)
  }
  
  # ---------------------------
  # Local helper: get_input_local
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      # If candidate is a vector of length one and an empty string, treat as no input.
      if (length(candidate) == 1 && candidate == "") {
        candidate <- character(0)
      }
      message("Using pre-recorded input for '", param, "': ", paste(candidate, collapse = ", "))
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message("Pre-recorded input for '", param, "' is invalid. Falling back to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (length(candidate) == 1 && candidate == "") {
        candidate <- character(0)
      }
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # ---------------------------
  # Step 1: Define condition orders.
  message("📋 Defining the order of conditions and condition_grouped...")
  available_conditions <- unique(zone_combined_data$condition)
  available_condition_grouped <- unique(zone_combined_data$condition_grouped)
  
  condition_order <- get_input_local(
    "conditions_order",
    prompt_msg = "Enter the desired order of conditions (comma-separated, e.g., control_1, control_2, contaminated_1,...): ",
    validate_fn = function(x) {
      orders <- split_and_trim(x)
      missing_items <- setdiff(available_conditions, orders)
      invalid_items <- setdiff(orders, available_conditions)
      (length(missing_items) == 0) && (length(invalid_items) == 0)
    },
    transform_fn = split_and_trim
  )
  message("✔️ Selected condition order: ", paste(condition_order, collapse = ", "))
  
  condition_grouped_order <- get_input_local(
    "conditions_grouped_order",
    prompt_msg = "Enter the desired order of condition_grouped (comma-separated, e.g., control, contaminated): ",
    validate_fn = function(x) {
      orders <- split_and_trim(x)
      missing_items <- setdiff(available_condition_grouped, orders)
      invalid_items <- setdiff(orders, available_condition_grouped)
      (length(missing_items) == 0) && (length(invalid_items) == 0)
    },
    transform_fn = split_and_trim
  )
  message("✔️ Selected condition_grouped order: ", paste(condition_grouped_order, collapse = ", "))
  
  # Save orders in the global environment.
  assign("generated_condition_order", condition_order, envir = .GlobalEnv)
  assign("generated_condition_grouped_order", condition_grouped_order, envir = .GlobalEnv)
  message("✔️ Condition orders saved as 'generated_condition_order' and 'generated_condition_grouped_order'.")
  
  # ---------------------------
  # Step 2: Remove conditions (optional).
  message("🧹 Removing conditions specified by the user...")
  remove_conditions <- get_input_local(
    "remove_conditions",
    prompt_msg = "Enter the condition(s) to remove (comma-separated, e.g., X), or press Enter to skip: ",
    validate_fn = function(x) TRUE,
    transform_fn = split_and_trim
  )
  if (length(remove_conditions) == 0) {
    message("✔️ No conditions removed.")
  } else {
    invalid_conditions <- remove_conditions[!remove_conditions %in% unique(zone_combined_data$condition)]
    if (length(invalid_conditions) > 0) {
      message("❌ The following conditions do not exist: ", paste(invalid_conditions, collapse = ", "))
      message("💡 Please enter valid condition names. No conditions were removed.")
    } else {
      message("✔️ Conditions to remove: ", paste(remove_conditions, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!condition %in% remove_conditions)
      message("✔️ Specified conditions successfully removed.")
    }
  }
  
  # ---------------------------
  # Step 3: Remove suspect wells.
  message("🕵️ Removing suspect wells specified by the user...")
  remove_suspect_well <- get_input_local(
    "remove_suspect_well",
    prompt_msg = "Enter the suspect wells to remove (comma-separated, e.g., A09, C08, F01, G02, G10, H07, H12), or press Enter to skip: ",
    validate_fn = function(x) TRUE,
    transform_fn = split_and_trim
  )
  if (length(remove_suspect_well) == 0) {
    message("✔️ No suspect wells specified.")
  } else {
    invalid_wells <- remove_suspect_well[!remove_suspect_well %in% zone_combined_data$animal]
    if (length(invalid_wells) > 0) {
      message("❌ The following wells do not exist in the 'animal' column: ", paste(invalid_wells, collapse = ", "))
      message("💡 Please re-enter the suspect wells. Skipping removal.")
    } else {
      message("✔️ Wells to remove: ", paste(remove_suspect_well, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% filter(!animal %in% remove_suspect_well)
      message("✔️ Suspect wells successfully removed.")
    }
  }
  
  # ---------------------------
  # Step 4: Calculate the number of wells per condition and zone.
  message("📊 Calculating the number of wells per condition and zone...")
  specific_minute <- 1
  wells_per_condition <- zone_combined_data %>%
    filter(!is.na(start) & start == specific_minute) %>%
    group_by(zone, condition) %>%
    summarise(n_wells = n_distinct(animal), .groups = "drop")
  zone_combined_data <- zone_combined_data %>% left_join(wells_per_condition, by = c("zone", "condition"))
  message("✔️ Well counts appended.")
  
  # ---------------------------
  # Step 5: Calculate normalized sums.
  message("🔄 Calculating normalized sums based on aggregation periods...")
  message("ℹ️ The aggregation period (in seconds) is used to group data for normalization.")
  aggregation_period <- as.numeric(get_input_local(
    "aggregation_period",
    prompt_msg = "Enter the aggregation period in seconds (e.g., 60): ",
    validate_fn = function(x) { !is.na(as.numeric(x)) && as.numeric(x) > 0 },
    transform_fn = function(x) as.numeric(trimws(x))
  ))
  message("✔️ Aggregation period set to ", aggregation_period, " seconds.")
  aggregation_period_minutes <- aggregation_period / 60
  
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
  
  # ---------------------------
  # Step 6: Define light and dark periods.
  message("🌞/🌑 Define light and dark periods...")
  available_periods <- unique(zone_combined_data$period_with_numbers)
  message("Available periods: ", paste(available_periods, collapse = ", "))
  
  light_period <- get_input_local(
    "light_period",
    prompt_msg = "Enter light periods to include (comma-separated, e.g., light_2, light_3): ",
    validate_fn = function(x) {
      periods <- split_and_trim(x)
      length(periods) > 0 && all(periods %in% available_periods)
    },
    transform_fn = split_and_trim,
    error_msg = "❌ Invalid input. Please enter valid light periods from the available options."
  )
  message("✔️ Selected light periods: ", paste(light_period, collapse = ", "))
  
  dark_period <- get_input_local(
    "dark_period",
    prompt_msg = "Enter dark periods to include (comma-separated, e.g., dark_1, dark_2): ",
    validate_fn = function(x) {
      periods <- split_and_trim(x)
      length(periods) > 0 && all(periods %in% available_periods)
    },
    transform_fn = split_and_trim,
    error_msg = "❌ Invalid input. Please enter valid dark periods from the available options."
  )
  message("✔️ Selected dark periods: ", paste(dark_period, collapse = ", "))
  
  light_data <- zone_combined_data %>% filter(period_with_numbers %in% light_period)
  dark_data <- zone_combined_data %>% filter(period_with_numbers %in% dark_period)
  
  # ---------------------------
  # Step 7: Calculate mean values for boxplots.
  message("📊 Calculating mean values for light and dark periods...")
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
        n_wells                 = first(n_wells),
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
  
  light_boxplot_data <- calculate_means(light_data)
  dark_boxplot_data <- calculate_means(dark_data)
  boxplot_data <- dplyr::bind_rows(light_boxplot_data, dark_boxplot_data)
  
  # ---------------------------
  # Step 8: Save outputs globally.
  assign("pretreated_data_for_lineplots_df", normalized_sums, envir = .GlobalEnv)
  assign("pretreated_data_for_boxplots_df", boxplot_data, envir = .GlobalEnv)
  message("🎉 Pretreatment complete! Saved as:")
  message("  - Line plots: 'pretreated_data_for_lineplots_df'")
  message("  - Box plots: 'pretreated_data_for_boxplots_df'\n")
  
  return(list(lineplots = normalized_sums, boxplots = boxplot_data))
}
