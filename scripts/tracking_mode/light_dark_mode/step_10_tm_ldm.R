# -----------------------------------------------------------
# prepare_delta_data_for_analysis.R
# -----------------------------------------------------------
# This function prepares data for delta analysis by extracting rows 
# corresponding to period boundaries (n) and a specified delta (x).
#
# The process is as follows:
#   1. It prints the available period boundaries (from 'boundary_associations')
#      so the user can choose valid boundary values.
#   2. It then prompts (via pre‐recorded input if available, otherwise interactively)
#      for the period boundaries to analyze (selected_period_boundaries).
#   3. Next, it prompts for the delta value (delta_value) to extract data around n:
#         - Rows where start == (n - delta_value) are labeled "before".
#         - Rows where start == n are labeled "switch".
#         - Rows where start == (n + delta_value) are labeled "after".
#   4. It validates that for each chosen boundary, both (n - delta_value) and (n + delta_value)
#      exist in the dataset.
#   5. It filters the data accordingly, calculates group means, and applies any condition removals
#      (as specified in function 7) to the combined zone data.
#   6. Finally, it saves the filtered delta data in the global environment.
#
# All inputs used are recorded in the global list 'input_record_list'
# (which must be initialized in your main script).
# -----------------------------------------------------------

prepare_delta_data_for_analysis <- function(
    zone_calculated_list = get("zone_calculated_list", envir = .GlobalEnv),
    output_dir = "outputs/tracking_mode/light_dark_mode/figures/boxplots",
    excel_output_dir = "outputs/tracking_mode/light_dark_mode/tables"
) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Delta Data Preparation Process!\n")
  message("This function helps you:")
  message("  • Prepare data for delta analysis, focusing on ranges around period boundaries.")
  message("  • Label rows corresponding to (n - delta) as 'before', (n) as 'switch', and (n + delta) as 'after'.")
  message("  • Define the period boundaries (n) and the delta to extract data from the dataset.")
  message("  • Save the filtered data for downstream delta-based analysis.\n")
  
  
  # -------------------------------------------------------------------
  # Check that 'boundary_associations' exists in the global environment.
  if (!exists("boundary_associations", envir = .GlobalEnv)) {
    stop("❌ Error: 'boundary_associations' does not exist. Please define it before running this function.")
  }
  boundary_associations <- get("boundary_associations", envir = .GlobalEnv)
  
  # Print boundary associations for user reference.
  message("📋 Available period boundaries and their associated transitions:")
  message(paste0(
    apply(boundary_associations, 1, function(row) {
      paste0("Boundary: ", row["boundary_time"], " (", row["transition"], ")")
    }),
    collapse = "\n"
  ))
  
  # -------------------------------------------------------------------
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
  
  # -------------------------------------------------------------------
  # Local helper function to get input:
  # It first checks for a pre-recorded value; if not found, it prompts interactively.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      message("Using pre-recorded input for '", param, "': ", paste(candidate, collapse = ", "))
      input_record_list[[param]] <<- paste(candidate, collapse = ", ")
      return(candidate)
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
  
  # Helper: Split comma-separated string and trim whitespace.
  split_and_trim <- function(x) {
    result <- unlist(strsplit(x, ","))
    trimws(result)
  }
  
  # -------------------------------------------------------------------
  # Step 1: Prompt for the period boundaries (n)
  selected_boundaries <- get_input_local(
    "selected_period_boundaries",
    prompt_msg = "🛠️ Enter one or more period boundaries (e.g., 45, 55), separated by commas: ",
    validate_fn = function(x) {
      vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(FALSE)
      missing <- setdiff(vals, boundary_associations$boundary_time)
      length(missing) == 0
    },
    transform_fn = function(x) {
      vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
      vals[!is.na(vals)]
    },
    error_msg = "❌ Invalid boundaries. Please enter numeric values that exist in the list above."
  )
  message("✔️ Selected boundaries: ", paste(selected_boundaries, collapse = ", "))
  
  # -------------------------------------------------------------------
  # Step 2: Prompt for the delta value (x)
  delta_value <- get_input_local(
    "delta_value",
    prompt_msg = "Enter the delta as the numeric difference for before/after rows (e.g., 1, 2, 5): ",
    validate_fn = function(x) {
      val <- as.numeric(trimws(x))
      !is.na(val) && val > 0
    },
    transform_fn = function(x) as.numeric(trimws(x)),
    error_msg = "❌ Invalid input. Please enter a positive numeric value for delta."
  )
  
  # Validate that for each selected boundary, both (n - delta) and (n + delta) exist.
  if (!is.list(zone_calculated_list) || is.null(zone_calculated_list$zone_combined)) {
    stop("❌ Error: 'zone_combined' is missing or NULL in 'zone_calculated_list'.")
  }
  zone_combined <- zone_calculated_list$zone_combined
  all_starts <- zone_combined$start
  for (boundary in selected_boundaries) {
    if (!((boundary - delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n - delta) = %s does not exist in the data.", boundary, boundary - delta_value))
    }
    if (!((boundary + delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n + delta) = %s does not exist in the data.", boundary, boundary + delta_value))
    }
  }
  message("✔️ Delta value set to ", delta_value, ".")
  
  # -------------------------------------------------------------------
  # Step 3: Remove conditions already removed in function 7 (if any)
  # If "remove_conditions" is provided in the pipeline inputs, filter them out.
  remove_conditions <- get_input_local(
    "remove_conditions",
    prompt_msg = "Enter the condition(s) to remove (comma-separated, e.g., X), or press Enter to skip: ",
    validate_fn = function(x) TRUE,
    transform_fn = split_and_trim
  )
  if (length(remove_conditions) > 0) {
    zone_combined <- zone_combined %>% filter(!condition %in% remove_conditions)
    message("✔️ Removed conditions: ", paste(remove_conditions, collapse = ", "))
  }
  
  # -------------------------------------------------------------------
  # Step 4: Filter and collect rows for (n - delta), n, and (n + delta).
  message("🔍 Filtering data for the selected boundaries and delta...")
  filtered_data <- data.frame()
  for (boundary in selected_boundaries) {
    before_data <- zone_combined %>% filter(start == boundary - delta_value) %>% mutate(momentum = "before")
    switch_data <- zone_combined %>% filter(start == boundary) %>% mutate(momentum = "switch")
    after_data <- zone_combined %>% filter(start == boundary + delta_value) %>% mutate(momentum = "after")
    filtered_data <- bind_rows(filtered_data, before_data, switch_data, after_data)
  }
  
  assign("filtered_delta_data", filtered_data, envir = .GlobalEnv)
  
  # -------------------------------------------------------------------
  # Step 5: Calculate group means on the filtered data.
  calculate_means <- function(data) {
    data %>%
      group_by(condition_tagged, zone, momentum) %>%
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
  
  # -------------------------------------------------------------------
  # Step 6: Save the filtered delta data globally.
  assign("pretreated_delta_data_for_boxplots_df", pretreated_delta_data_for_boxplots_df, envir = .GlobalEnv)
  message("🎉 Data successfully filtered and saved as 'pretreated_delta_data_for_boxplots_df' in the global environment.")
  message("💾 Use this data for delta-based visualizations or further analysis.\n")
  
  return(filtered_data)
}
