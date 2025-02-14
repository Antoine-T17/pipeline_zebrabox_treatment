# -----------------------------------------------------------
# File: prepare_delta_data_for_analysis.R
# -----------------------------------------------------------
# Harmonized version of the prepare_delta_data_for_analysis function for vibration_mode.
# This function prepares data for delta analysis by:
#   • Displaying available period boundaries.
#   • Prompting for selected boundaries and a delta value.
#   • Validating that rows exist for (n - delta) and (n + delta).
#   • Filtering the data and labeling rows as "before", "switch", or "after".
#   • Calculating group means and saving the filtered data globally.
# -----------------------------------------------------------

prepare_delta_data_for_analysis <- function(zone_calculated_list = get("zone_calculated_list", envir = .GlobalEnv),
                                            output_dir = "outputs/tracking_mode/vibration_mode/figures/boxplots",
                                            excel_output_dir = "outputs/tracking_mode/vibration_mode/tables") {
  message("\n---\n")
  message("👋 Welcome to the Delta Data Preparation Process!\n")
  message("📋 This function will help you:")
  message("   • Select period boundaries and specify a delta value.")
  message("   • Filter data around each selected boundary and label rows as 'before', 'switch', or 'after'.")
  message("   • Save the filtered delta data globally for further analysis.\n")
  
  if (!exists("boundary_associations", envir = .GlobalEnv)) {
    stop("❌ 'boundary_associations' not found. Define it before running this function.")
  }
  boundary_associations <- get("boundary_associations", envir = .GlobalEnv)
  message("📋 Available period boundaries and transitions:")
  message(paste0(apply(boundary_associations, 1, function(row) {
    paste0("Boundary: ", row["boundary_time"], " (", row["transition"], ")")
  }), collapse = "\n"))
  
  # Load pre-recorded inputs.
  pipeline_inputs <- list()
  inputs_path <- "inputs/inputs_values"
  inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
  inputs_file_csv  <- file.path(inputs_path, "pipeline_inputs.csv")
  if (file.exists(inputs_file_xlsx)) {
    df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("❌ The pipeline_inputs.xlsx file must contain columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  } else if (file.exists(inputs_file_csv)) {
    df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("❌ The pipeline_inputs.csv file must contain columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  }
  
  # Unified input helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "" &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      message("💾 Using pre-recorded input for '", param, "': ", paste(candidate, collapse = ", "))
      input_record_list[[param]] <<- paste(candidate, collapse = ", ")
      return(candidate)
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ", paste(candidate, collapse = ", "))
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  split_and_trim <- function(x) trimws(unlist(strsplit(x, ",")))
  
  # Step 1: Prompt for period boundaries.
  selected_boundaries <- get_input_local("selected_period_boundaries",
                                         "❓ Enter one or more period boundaries (comma-separated): ",
                                         validate_fn = function(x) {
                                           vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
                                           vals <- vals[!is.na(vals)]
                                           length(vals) > 0 && length(setdiff(vals, boundary_associations$boundary_time)) == 0
                                         },
                                         transform_fn = function(x) {
                                           vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
                                           vals[!is.na(vals)]
                                         },
                                         error_msg = "❌ Invalid boundaries. Enter numeric values present in the list above.")
  message("✔️ Selected boundaries: ", paste(selected_boundaries, collapse = ", "))
  
  # Step 2: Prompt for the delta value.
  delta_value <- get_input_local("delta_value",
                                 "❓ Enter the delta value (numeric, e.g., 1, 2, 5): ",
                                 validate_fn = function(x) {
                                   val <- as.numeric(trimws(x))
                                   !is.na(val) && val > 0
                                 },
                                 transform_fn = function(x) as.numeric(trimws(x)),
                                 error_msg = "❌ Enter a positive numeric delta.")
  message("✔️ Delta value set to: ", delta_value)
  
  # Validate that for each boundary, (n - delta) and (n + delta) exist.
  if (!is.list(zone_calculated_list) || is.null(zone_calculated_list$zone_combined)) {
    stop("❌ 'zone_combined' is missing or NULL in zone_calculated_list.")
  }
  zone_combined <- zone_calculated_list$zone_combined
  all_starts <- zone_combined$start
  for (boundary in selected_boundaries) {
    if (!((boundary - delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n - delta) = %s not found in data.", boundary, boundary - delta_value))
    }
    if (!((boundary + delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n + delta) = %s not found in data.", boundary, boundary + delta_value))
    }
  }
  
  # Step 3: Remove conditions if already removed.
  remove_conditions <- get_input_local("remove_conditions",
                                       "❓ Enter condition(s) to remove (comma-separated), or press Enter to skip: ",
                                       validate_fn = function(x) TRUE,
                                       transform_fn = split_and_trim)
  if (length(remove_conditions) > 0) {
    zone_combined <- zone_combined %>% filter(!condition %in% remove_conditions)
    message("✔️ Removed conditions: ", paste(remove_conditions, collapse = ", "))
  }
  
  # Step 4: Filter data for (n - delta), n, and (n + delta).
  message("🛠️ Filtering data for delta analysis...")
  filtered_data <- data.frame()
  for (boundary in selected_boundaries) {
    before_data <- zone_combined %>% filter(start == boundary - delta_value) %>% mutate(momentum = "before")
    switch_data <- zone_combined %>% filter(start == boundary) %>% mutate(momentum = "switch")
    after_data  <- zone_combined %>% filter(start == boundary + delta_value) %>% mutate(momentum = "after")
    filtered_data <- dplyr::bind_rows(filtered_data, before_data, switch_data, after_data)
  }
  assign("filtered_delta_data", filtered_data, envir = .GlobalEnv)
  
  # Step 5: Calculate group means.
  calculate_means <- function(data) {
    data %>% group_by(condition_tagged, zone, momentum) %>%
      summarise(
        start = first(start),
        condition_grouped = first(condition_grouped),
        animal = first(animal),
        condition = first(condition),
        period = first(period),
        period_with_numbers = first(period_with_numbers),
        period_without_numbers = first(period_without_numbers),
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
  
  pretreated_delta_data_for_boxplots_df <- calculate_means(filtered_data)
  assign("pretreated_delta_data_for_boxplots_df", pretreated_delta_data_for_boxplots_df, envir = .GlobalEnv)
  message("🎉 Delta data prepared and saved as 'pretreated_delta_data_for_boxplots_df'.\n")
  
  return(filtered_data)
}
