# -----------------------------------------------------------
# File: pre_visualization_and_delta_data_treatment.R
# -----------------------------------------------------------
# This combined function performs three pretreatment steps:
#
# Part I – Pretreatment for Lineplots:
#   - Removes unwanted conditions, suspect wells, and response variable columns.
#   - Aggregates the data over a specified aggregation period.
#   - Calculates normalized sums (for visualization lineplots).
#
# Part II – Pretreatment for Boxplots:
#   - Filters the data for user-selected light and dark periods.
#   - Calculates group means (for generating boxplots).
#
# Part III – Pretreatment for Delta Boxplots:
#   - Displays available period boundaries (from a global 'boundary_associations').
#   - Prompts for one or more boundaries and a delta value.
#   - Validates that (n - delta) and (n + delta) exist.
#   - Filters data at (n - delta), n, and (n + delta), labeling them as 
#     "before", "switch", or "after".
#   - Calculates group means for delta boxplot pretreatment.
#
# The function returns a list with three elements:
#   - lineplots: Data for visualization lineplots (normalized_sums)
#   - boxplots: Data for pretreatment boxplots (boxplot_data)
#   - delta_boxplots: Data for pretreatment delta boxplots (delta_boxplot_data)
#
# Note: Inputs are recorded in the global list 'input_record_list' and some
#       pre-recorded inputs are read from the global 'pipeline_inputs'.
# -----------------------------------------------------------

pre_visualization_data_treatment <- function(zone_combined_data) {
  message("\n---\n")
  message("👋 Welcome to the Data Pretreatment Process!\n")
  message("This pipeline will prepare data for visualization (lineplots and boxplots) ")
  message("and for delta analysis (delta boxplots pretreatment).\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Helper: splits comma-separated input and trims whitespace.
  split_and_trim <- function(x) trimws(unlist(strsplit(x, ",")))
  
  # Unified input helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      # Treat a single "no" (case-insensitive) as an empty vector.
      if (length(candidate) == 1 && tolower(candidate) == "no") candidate <- character(0)
      message("💾 Using pre-recorded input for '", param, "': ",
              if(length(candidate)==0) "none" else paste(candidate, collapse = ", "))
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- if(length(candidate)==0) "none" else paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (length(candidate) == 1 && tolower(candidate) == "no") candidate <- character(0)
      if (length(candidate) == 1 && candidate == "") candidate <- character(0)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ",
                if(length(candidate)==0) "none" else paste(candidate, collapse = ", "))
        input_record_list[[param]] <<- if(length(candidate)==0) "none" else paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  ## ===================== Common Pretreatment Steps =====================
  # Step 1: Define condition orders.
  message("\n📋 Common pretreatment steps...")
  available_conditions <- unique(zone_combined_data$condition)
  available_condition_grouped <- unique(zone_combined_data$condition_grouped)
  
  condition_order <- get_input_local("conditions_order",
                                     "❓ Enter the desired order of conditions (comma-separated): ",
                                     validate_fn = function(x) {
                                       orders <- split_and_trim(x)
                                       missing_items <- setdiff(available_conditions, orders)
                                       invalid_items <- setdiff(orders, available_conditions)
                                       length(missing_items) == 0 && length(invalid_items) == 0
                                     },
                                     transform_fn = split_and_trim)
  message("✔️ Condition order set: ", paste(condition_order, collapse = ", "))
  
  condition_grouped_order <- get_input_local("conditions_grouped_order",
                                             "❓ Enter the desired order of condition_grouped (comma-separated): ",
                                             validate_fn = function(x) {
                                               orders <- split_and_trim(x)
                                               missing_items <- setdiff(available_condition_grouped, orders)
                                               invalid_items <- setdiff(orders, available_condition_grouped)
                                               length(missing_items) == 0 && length(invalid_items) == 0
                                             },
                                             transform_fn = split_and_trim)
  message("✔️ Condition_grouped order set: ", paste(condition_grouped_order, collapse = ", "))
  
  assign("generated_condition_order", condition_order, envir = .GlobalEnv)
  assign("generated_condition_grouped_order", condition_grouped_order, envir = .GlobalEnv)
  message("💾 Condition orders saved globally.")
  
  # Step 2: Remove specified conditions.
  message("🧹 Removing specified conditions (if any)...")
  remove_conditions <- get_input_local("remove_conditions",
                                       "❓ Enter condition(s) to remove (comma-separated) or type 'no' to keep all: ",
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
                                         "❓ Enter suspect wells to remove (comma-separated) or type 'no' to keep all: ",
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
  
  # Step 4: Remove specified response variable columns.
  message("🧹 Removing specified response variable columns (if any)...")
  remove_variables <- get_input_local("remove_variables",
                                      "❓ Enter response variable column(s) to remove (comma-separated) or type 'no' to keep all: ",
                                      validate_fn = function(x) TRUE,
                                      transform_fn = split_and_trim)
  default_response_vars <- c("totaldist", "smldist", "lardist",
                             "totaldur", "smldur", "lardur",
                             "totalct", "smlct", "larct",
                             "inact", "inadur", "inadist",
                             "emptyct", "emptydur")
  
  if (length(remove_variables) == 0) {
    message("✔️ No response variable columns removed.")
    response_vars <- default_response_vars
  } else {
    invalid_vars <- remove_variables[!remove_variables %in% colnames(zone_combined_data)]
    if (length(invalid_vars) > 0) {
      message("❌ The following variables do not exist in the data: ", paste(invalid_vars, collapse = ", "))
      message("⚠️ Skipping removal for these variables.")
    }
    vars_to_remove <- intersect(remove_variables, colnames(zone_combined_data))
    if (length(vars_to_remove) > 0) {
      message("✔️ Removing response variable columns: ", paste(vars_to_remove, collapse = ", "))
      zone_combined_data <- zone_combined_data %>% select(-all_of(vars_to_remove))
    }
    response_vars <- setdiff(default_response_vars, vars_to_remove)
  }
  
  # Step 5: Calculate number of wells per condition and zone.
  message("📊 Calculating well counts per condition and zone...")
  specific_minute <- 1
  wells_per_condition <- zone_combined_data %>% 
    filter(!is.na(start) & start == specific_minute) %>%
    group_by(zone, condition) %>% 
    summarise(n_wells = n_distinct(animal), .groups = "drop")
  zone_combined_data <- zone_combined_data %>% left_join(wells_per_condition, by = c("zone", "condition"))
  message("✔️ Well counts appended.")
  
  ## ===================== Part I – Pretreatment for Lineplots =====================
  message("\n📋 Preparing lineplot data (pretreatment for visualization)...")
  aggregation_period <- as.numeric(get_input_local("aggregation_period",
                                                   "❓ Enter aggregation period in seconds (e.g., 60): ",
                                                   validate_fn = function(x) !is.na(as.numeric(x)) && as.numeric(x) > 0,
                                                   transform_fn = function(x) as.numeric(trimws(x))))
  message("✔️ Aggregation period set to ", aggregation_period, " seconds.")
  aggregation_period_minutes <- aggregation_period / 60
  zone_combined_data <- zone_combined_data %>% 
    mutate(start_rounded = floor(start / aggregation_period_minutes) * aggregation_period_minutes)
  
  normalized_sums <- zone_combined_data %>% 
    group_by(condition, period_with_numbers, zone, start_rounded) %>%
    summarise(
      animal = first(animal),
      condition = first(condition),
      condition_grouped = first(condition_grouped),
      condition_tagged = first(condition_tagged),
      period = first(period),
      period_with_numbers = first(period_with_numbers),
      period_without_numbers = first(period_without_numbers),
      zone = first(zone),
      n_wells = first(n_wells),
      across(all_of(response_vars), ~sum(.x, na.rm = TRUE) / first(n_wells), .names = "sum_{.col}"),
      .groups = "drop"
    )
  
  ## ===================== Part II – Pretreatment for Boxplots =====================
  message("\n📋 Preparing boxplot data (pretreatment for visualization)...")
  available_periods <- unique(zone_combined_data$period_with_numbers)
  message("ℹ️ Available periods: ", paste(available_periods, collapse = ", "))
  
  light_period <- get_input_local("light_period",
                                  "❓ Enter light periods to include (comma-separated): ",
                                  validate_fn = function(x) {
                                    periods <- split_and_trim(x)
                                    length(periods) > 0 && all(periods %in% available_periods)
                                  },
                                  transform_fn = split_and_trim,
                                  error_msg = "❌ Invalid light periods. Please use available options.")
  message("✔️ Selected light periods: ", paste(light_period, collapse = ", "))
  
  dark_period <- get_input_local("dark_period",
                                 "❓ Enter dark periods to include (comma-separated): ",
                                 validate_fn = function(x) {
                                   periods <- split_and_trim(x)
                                   length(periods) > 0 && all(periods %in% available_periods)
                                 },
                                 transform_fn = split_and_trim,
                                 error_msg = "❌ Invalid dark periods. Please use available options.")
  message("✔️ Selected dark periods: ", paste(dark_period, collapse = ", "))
  
  light_data <- zone_combined_data %>% filter(period_with_numbers %in% light_period)
  dark_data  <- zone_combined_data %>% filter(period_with_numbers %in% dark_period)
  
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
        across(all_of(response_vars), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
        .groups = "drop"
      )
  }
  
  light_boxplot_data <- calculate_means(light_data)
  dark_boxplot_data  <- calculate_means(dark_data)
  boxplot_data <- dplyr::bind_rows(light_boxplot_data, dark_boxplot_data)
  
  ## ===================== Part III – Pretreatment for Delta Boxplots =====================
  message("\n📋 Preparing delta boxplot data (pretreatment for visualization)...")
  
  # Ensure that boundary_associations exists.
  if (!exists("boundary_associations", envir = .GlobalEnv)) {
    stop("❌ 'boundary_associations' not found. Define it before running this function.")
  }
  boundary_associations <- get("boundary_associations", envir = .GlobalEnv)
  message("ℹ️ Available period boundaries and transitions:")
  message(paste0(apply(boundary_associations, 1, function(row) {
    paste0("Boundary: ", row["boundary_time"], " (", row["transition"], ")")
  }), collapse = "\n"))
  
  selected_boundaries <- get_input_local("selected_period_boundaries",
                                         "❓ Enter one or more period boundaries (comma-separated): ",
                                         validate_fn = function(x) {
                                           # x should be a numeric vector
                                           vals <- x
                                           length(vals) > 0 && length(setdiff(vals, boundary_associations$boundary_time)) == 0
                                         },
                                         transform_fn = function(x) {
                                           # If input is already numeric, return it; otherwise, convert from a comma-separated string.
                                           if (is.numeric(x)) return(x)
                                           as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                         },
                                         error_msg = "❌ Invalid boundaries. Enter numeric values present in the list above."
  )
  message("✔️ Selected boundaries: ", paste(selected_boundaries, collapse = ", "))
  
  delta_value <- get_input_local("delta_value",
                                 "❓ Enter the delta value (numeric, e.g., 1, 2, 5): ",
                                 validate_fn = function(x) {
                                   val <- as.numeric(trimws(x))
                                   !is.na(val) && val > 0
                                 },
                                 transform_fn = function(x) as.numeric(trimws(x)),
                                 error_msg = "❌ Enter a positive numeric delta."
  )
  message("✔️ Delta value set to: ", delta_value)
  
  # Validate that for each boundary, (n - delta) and (n + delta) exist.
  all_starts <- zone_combined_data$start
  for (boundary in selected_boundaries) {
    if (!((boundary - delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n - delta) = %s not found in data.", boundary, boundary - delta_value))
    }
    if (!((boundary + delta_value) %in% all_starts)) {
      stop(sprintf("❌ For boundary %s, (n + delta) = %s not found in data.", boundary, boundary + delta_value))
    }
  }
  
  message("🛠️ Filtering data for delta pretreatment...")
  filtered_delta <- data.frame()
  for (boundary in selected_boundaries) {
    before_data <- zone_combined_data %>% filter(start == boundary - delta_value) %>% mutate(momentum = "before")
    switch_data <- zone_combined_data %>% filter(start == boundary) %>% mutate(momentum = "switch")
    after_data  <- zone_combined_data %>% filter(start == boundary + delta_value) %>% mutate(momentum = "after")
    filtered_delta <- dplyr::bind_rows(filtered_delta, before_data, switch_data, after_data)
  }
  
  calculate_delta_means <- function(data) {
    data %>% group_by(condition_tagged, zone, momentum) %>%
      summarise(
        start = first(start),
        condition_grouped = first(condition_grouped),
        animal = first(animal),
        condition = first(condition),
        period = first(period),
        period_with_numbers = first(period_with_numbers),
        period_without_numbers = first(period_without_numbers),
        across(all_of(response_vars), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
        .groups = "drop"
      )
  }
  
  delta_boxplot_data <- calculate_delta_means(filtered_delta)
  
  ## ===================== Save Global Outputs (Optional) =====================
  assign("pretreated_data_for_lineplots_df", normalized_sums, envir = .GlobalEnv)
  assign("pretreated_data_for_boxplots_df", boxplot_data, envir = .GlobalEnv)
  assign("pretreated_delta_data_for_boxplots_df", delta_boxplot_data, envir = .GlobalEnv)
  
  message("\n🎉 Pretreatment complete!")
  message("💾 Line plot data saved as 'pretreated_data_for_lineplots_df'")
  message("💾 Box plot data saved as 'pretreated_data_for_boxplots_df'")
  message("💾 Delta boxplot data saved as 'pretreated_delta_data_for_boxplots_df'\n")
  
  return(list(
    lineplots = normalized_sums,
    boxplots = boxplot_data,
    delta_boxplots = delta_boxplot_data
  ))
}
