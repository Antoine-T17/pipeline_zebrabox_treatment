# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light dark mode
# Function: pre_visualization_data_treatment
# Purpose: Prepares data for visualization by performing three pretreatment steps:
#          Part I – Lineplots: Remove unwanted conditions, suspect wells, response columns, etc.
#          Part II – Boxplots: Filter data for user-selected light/dark periods and calculate group means.
#          Part III – Delta Boxplots: Prompt for boundaries and delta, validate/filter data, and calculate group means.
#
#          Some variables are per-plate (tokens separated by ";"):
#            - conditions_order, conditions_grouped_order, remove_conditions,
#              remove_conditions_grouped, remove_suspect_well, remove_variables, remove_period.
#          Others are universal: aggregation_period, light_period, dark_period.
#
#          At the end, data from each plate are combined (via rbind) for final visualization.
#          Returns a list with lineplots, boxplots, and delta_boxplots elements.
# -----------------------------------------------------------
pre_visualization_data_treatment <- function(zone_calculated_list) {
  # Make sure we have the correct number of plates
  n_plates <- length(zone_calculated_list)
  if (n_plates < 1) {
    stop("❌ zone_calculated_list is empty or not provided. Make sure Function 6 has populated it.")
  }
  
  message("\n---\n")
  message("👋 Welcome to the Data Pretreatment Process!")
  message("📋 This function will help you:")
  message("   • Prepare data for visualization (lineplots, boxplots, delta boxplots).")
  message("   • Remove unwanted conditions and suspect wells, and reorder columns.")
  message("   • Process per-plate inputs for conditions_order, conditions_grouped_order,")
  message("     remove_conditions, remove_conditions_grouped, remove_suspect_well,")
  message("     remove_variables, and remove_period.")
  message("   • Use universal inputs for aggregation_period, light_period, and dark_period.")
  message("   • Combine all plates (via rbind) for final visualization datasets.\n")
  
  # Retrieve pipeline inputs from global environment
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Helper for standard interactive input
  get_input_local <- function(param, prompt_msg, validate_fn, transform_fn,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(as.character(pipeline_inputs[[param]]))) {
        message("💾 Using pre-recorded input for '", param, "': ", as.character(pipeline_inputs[[param]]))
        input_record_list[[param]] <<- as.character(pipeline_inputs[[param]])
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      if (user_input == "") {
        message("⚠️ Empty input detected. Please enter a valid value.")
        next
      }
      if (validate_fn(user_input)) {
        candidate <- transform_fn(user_input)
        message("✔️ Input for '", param, "' recorded: ", user_input)
        input_record_list[[param]] <<- user_input
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Helper for per-plate inputs. We pass in n_plates explicitly.
  get_per_plate_input <- function(param, prompt_msg, n_plates) {
    validate_fn <- function(x) {
      tokens <- unlist(strsplit(x, "\\s*;\\s*"))
      length(tokens) == n_plates
    }
    transform_fn <- function(x) {
      tokens <- unlist(strsplit(x, "\\s*;\\s*"))
      tokens
    }
    result <- get_input_local(param, prompt_msg, validate_fn, transform_fn,
                              error_msg = sprintf("❌ Please enter exactly %d value(s) separated by ';'.", n_plates))
    return(result)
  }
  
  # ---------- PER-PLATE VARIABLES ----------
  conditions_order_tokens <- get_per_plate_input("conditions_order",
                                                 sprintf("❓ Enter the desired order of conditions for %d plate(s), separated by ';': ", n_plates), n_plates)
  
  conditions_grouped_order_tokens <- get_per_plate_input("conditions_grouped_order",
                                                         sprintf("❓ Enter the desired order of condition_grouped for %d plate(s), separated by ';': ", n_plates), n_plates)
  
  remove_conditions_tokens <- get_per_plate_input("remove_conditions",
                                                  sprintf("❓ Enter condition(s) to remove for %d plate(s) (comma-separated or 'no'), separated by ';': ", n_plates), n_plates)
  
  remove_conditions_grouped_tokens <- get_per_plate_input("remove_conditions_grouped",
                                                          sprintf("❓ Enter grouped condition(s) to remove for %d plate(s) (comma-separated or 'no'), separated by ';': ", n_plates), n_plates)
  
  remove_suspect_well_tokens <- get_per_plate_input("remove_suspect_well",
                                                    sprintf("❓ Enter suspect wells to remove for %d plate(s) (comma-separated or 'no'), separated by ';': ", n_plates), n_plates)
  
  remove_variables_tokens <- get_per_plate_input("remove_variables",
                                                 sprintf("❓ Enter response variable column(s) to remove for %d plate(s) (comma-separated or 'no'), separated by ';': ", n_plates), n_plates)
  
  remove_period_tokens <- get_per_plate_input("remove_period",
                                              sprintf("❓ Enter period(s) to remove for %d plate(s) (comma-separated or 'no'), separated by ';': ", n_plates), n_plates)
  
  # ---------- UNIVERSAL VARIABLES ----------
  aggregation_period <- get_input_local("aggregation_period",
                                        "❓ Enter aggregation period in seconds (e.g., 60): ",
                                        validate_fn = function(x) !is.na(as.numeric(x)) && as.numeric(x) > 0,
                                        transform_fn = function(x) as.numeric(trimws(x)),
                                        error_msg = "❌ Please enter a positive number for aggregation period.")
  
  light_period <- get_input_local("light_period",
                                  "❓ Enter light period(s) to include (comma-separated, e.g., light1,light2): ",
                                  validate_fn = function(x) length(trimws(unlist(strsplit(x, ",")))) > 0,
                                  transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
                                  error_msg = "❌ Please enter at least one light period.")
  
  dark_period <- get_input_local("dark_period",
                                 "❓ Enter dark period(s) to include (comma-separated, e.g., dark1,dark2): ",
                                 validate_fn = function(x) length(trimws(unlist(strsplit(x, ",")))) > 0,
                                 transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
                                 error_msg = "❌ Please enter at least one dark period.")
  
  # ---------- SPLIT PER-PLATE TOKENS FURTHER ----------
  conditions_order_list <- lapply(conditions_order_tokens, function(tok) trimws(unlist(strsplit(tok, ","))))
  conditions_grouped_order_list <- lapply(conditions_grouped_order_tokens, function(tok) trimws(unlist(strsplit(tok, ","))))
  
  remove_conditions_list <- lapply(remove_conditions_tokens, function(tok) {
    tmp <- trimws(unlist(strsplit(tok, ",")))
    if (length(tmp) == 1 && tolower(tmp) == "no") character(0) else tmp
  })
  remove_conditions_grouped_list <- lapply(remove_conditions_grouped_tokens, function(tok) {
    tmp <- trimws(unlist(strsplit(tok, ",")))
    if (length(tmp) == 1 && tolower(tmp) == "no") character(0) else tmp
  })
  remove_suspect_well_list <- lapply(remove_suspect_well_tokens, function(tok) {
    tmp <- trimws(unlist(strsplit(tok, ",")))
    if (length(tmp) == 1 && tolower(tmp) == "no") character(0) else tmp
  })
  remove_variables_list <- lapply(remove_variables_tokens, function(tok) {
    tmp <- trimws(unlist(strsplit(tok, ",")))
    if (length(tmp) == 1 && tolower(tmp) == "no") character(0) else tmp
  })
  
  # For remove_period, verify that each token matches the plate's available periods
  remove_period_list <- vector("list", n_plates)
  for (i in seq_len(n_plates)) {
    valid <- FALSE
    while (!valid) {
      token <- trimws(unlist(strsplit(remove_period_tokens[i], ",")))
      available_periods <- unique(zone_calculated_list[[i]]$zone_combined$period_with_numbers)
      if (length(token) == 1 && tolower(token) == "no") {
        remove_period_list[[i]] <- character(0)
        valid <- TRUE
      } else if (all(token %in% available_periods)) {
        remove_period_list[[i]] <- token
        valid <- TRUE
      } else {
        message(sprintf("⚠️ For plate %d, the entered period(s) '%s' do not match available periods: %s",
                        i, paste(token, collapse = ", "), paste(available_periods, collapse = ", ")))
        new_input <- readline(prompt = sprintf("❓ Please re-enter valid remove_period(s) for plate %d (comma-separated or 'no'): ", i))
        if (new_input == "") next
        remove_period_tokens[i] <- new_input
      }
    }
  }
  
  # ---------- PER-PLATE PRETREATMENT ----------
  lineplot_list <- list()
  boxplot_list <- list()
  delta_boxplot_list <- list()
  
  for (i in seq_len(n_plates)) {
    message(sprintf("\n📋 Processing common pretreatment steps for plate %d...", i))
    
    # Retrieve the combined data from Function 6
    data_plate <- zone_calculated_list[[i]]$zone_combined
    
    available_conditions <- unique(data_plate$condition)
    available_condition_grouped <- unique(data_plate$condition_grouped)
    message("✅ Available conditions: ", paste(available_conditions, collapse = ", "))
    message("✅ Available condition_grouped: ", paste(available_condition_grouped, collapse = ", "))
    
    # Apply per-plate orders
    cond_order <- conditions_order_list[[i]]
    cond_group_order <- conditions_grouped_order_list[[i]]
    message(sprintf("✔️ For plate %d, condition order set to: %s", i, paste(cond_order, collapse = ", ")))
    message(sprintf("✔️ For plate %d, condition_grouped order set to: %s", i, paste(cond_group_order, collapse = ", ")))
    
    # Remove specified conditions
    rem_conds <- remove_conditions_list[[i]]
    if (length(rem_conds) == 0) {
      message(sprintf("✔️ For plate %d, no conditions removed.", i))
    } else {
      missing_conds <- setdiff(rem_conds, available_conditions)
      if (length(missing_conds) > 0) {
        message(sprintf("❌ For plate %d, the following conditions do not exist: %s. No conditions removed.",
                        i, paste(missing_conds, collapse = ", ")))
      } else {
        message(sprintf("✔️ For plate %d, removing conditions: %s", i, paste(rem_conds, collapse = ", ")))
        data_plate <- filter(data_plate, !(condition %in% rem_conds))
      }
    }
    
    # Remove specified grouped conditions
    rem_conds_grouped <- remove_conditions_grouped_list[[i]]
    if (length(rem_conds_grouped) == 0) {
      message(sprintf("✔️ For plate %d, no grouped conditions removed.", i))
    } else {
      missing_grouped <- setdiff(rem_conds_grouped, available_condition_grouped)
      if (length(missing_grouped) > 0) {
        message(sprintf("❌ For plate %d, the following grouped conditions do not exist: %s. No grouped conditions removed.",
                        i, paste(missing_grouped, collapse = ", ")))
      } else {
        message(sprintf("✔️ For plate %d, removing grouped conditions: %s", i, paste(rem_conds_grouped, collapse = ", ")))
        data_plate <- filter(data_plate, !(condition_grouped %in% rem_conds_grouped))
      }
    }
    
    # Remove suspect wells
    rem_wells <- remove_suspect_well_list[[i]]
    if (length(rem_wells) == 0) {
      message(sprintf("✔️ For plate %d, no suspect wells removed.", i))
    } else {
      missing_wells <- setdiff(rem_wells, data_plate$animal)
      if (length(missing_wells) > 0) {
        message(sprintf("❌ For plate %d, the following wells do not exist: %s. No suspect wells removed.",
                        i, paste(missing_wells, collapse = ", ")))
      } else {
        message(sprintf("✔️ For plate %d, removing suspect wells: %s", i, paste(rem_wells, collapse = ", ")))
        data_plate <- filter(data_plate, !(animal %in% rem_wells))
      }
    }
    
    # Remove specified response variable columns
    rem_vars <- remove_variables_list[[i]]
    default_response_vars <- c("totaldist", "smldist", "lardist",
                               "totaldur", "smldur", "lardur",
                               "totalct", "smlct", "larct",
                               "inact", "inadur", "inadist",
                               "emptyct", "emptydur")
    if (length(rem_vars) == 0) {
      message(sprintf("✔️ For plate %d, no response variable columns removed.", i))
      response_vars <- default_response_vars
    } else {
      invalid_vars <- setdiff(rem_vars, colnames(data_plate))
      if (length(invalid_vars) > 0) {
        message(sprintf("❌ For plate %d, the following variables do not exist: %s. Skipping removal for these variables.",
                        i, paste(invalid_vars, collapse = ", ")))
      }
      vars_to_remove <- intersect(rem_vars, colnames(data_plate))
      if (length(vars_to_remove) > 0) {
        message(sprintf("✔️ For plate %d, removing response variable columns: %s", i, paste(vars_to_remove, collapse = ", ")))
        data_plate <- data_plate[, !(names(data_plate) %in% vars_to_remove)]
      }
      response_vars <- setdiff(default_response_vars, vars_to_remove)
    }
    
    # Remove specified periods
    rem_period <- remove_period_list[[i]]
    if (length(rem_period) == 0) {
      message(sprintf("✔️ For plate %d, no periods removed.", i))
    } else {
      message(sprintf("✔️ For plate %d, removing period(s): %s", i, paste(rem_period, collapse = ", ")))
      data_plate <- filter(data_plate, !(period %in% rem_period))
    }
    
    # Compute well counts per condition & zone at minute=1
    specific_minute <- 1
    wells_per_condition <- data_plate %>% 
      filter(!is.na(start) & start == specific_minute) %>% 
      group_by(zone, condition) %>% 
      summarise(n_wells = n_distinct(animal))
    data_plate <- merge(data_plate, wells_per_condition, by = c("zone", "condition"), all.x = TRUE)
    message(sprintf("✔️ For plate %d, well counts appended.", i))
    
    ## ===================== Part I – Lineplots =====================
    message(sprintf("\n📋 Preparing lineplot data for plate %d...", i))
    data_plate <- mutate(data_plate, start_rounded = floor(start / (aggregation_period / 60)) * (aggregation_period / 60))
    normalized_sums <- data_plate %>% 
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
    
    ## ===================== Part II – Boxplots =====================
    message(sprintf("\n📋 Preparing boxplot data for plate %d...", i))
    available_periods <- unique(data_plate$period_with_numbers)
    message("ℹ️ Available periods: ", paste(available_periods, collapse = ", "))
    
    light_data <- filter(data_plate, period_with_numbers %in% light_period)
    dark_data  <- filter(data_plate, period_with_numbers %in% dark_period)
    
    calculate_means <- function(d) {
      d %>% group_by(condition_tagged, period_without_numbers, zone) %>%
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
    boxplot_data <- rbind(light_boxplot_data, dark_boxplot_data)
    
    ## ===================== Part III – Delta Boxplots =====================
    message(sprintf("\n📋 Preparing delta boxplot data for plate %d...", i))
    if (!exists("boundary_associations_list", envir = .GlobalEnv)) {
      stop("❌ 'boundary_associations_list' not found. Please run the period assignment function first.")
    }
    boundary_associations <- boundary_associations_list[[1]]  # assume same for all plates
    message("ℹ️ Available period boundaries (rounded) and transitions:")
    message(paste(apply(boundary_associations, 1, function(row) {
      paste0("Rounded Boundary: ", row["boundary_time"], " (", row["transition"], ")")
    }), collapse = "\n"))
    
    tolerance <- 0.2
    selected_boundaries <- get_input_local("selected_period_boundaries",
                                           sprintf("❓ Enter one or more period boundaries (comma-separated) for plate %d: ", i),
                                           validate_fn = function(x) {
                                             vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
                                             length(vals) > 0 && all(sapply(vals, function(v) {
                                               any(abs(v - as.numeric(boundary_associations$boundary_time)) < tolerance)
                                             }))
                                           },
                                           transform_fn = function(x) as.numeric(trimws(unlist(strsplit(x, ",")))),
                                           error_msg = "❌ Invalid boundaries. Enter numeric values that match the rounded list (within tolerance)."
    )
    delta_value <- get_input_local("delta_value",
                                   sprintf("❓ Enter the delta value (numeric) for plate %d (e.g., 1, 2, 5): ", i),
                                   validate_fn = function(x) {
                                     val <- as.numeric(trimws(x))
                                     !is.na(val) && val > 0
                                   },
                                   transform_fn = function(x) as.numeric(trimws(x)),
                                   error_msg = "❌ Enter a positive numeric delta."
    )
    
    for (boundary in selected_boundaries) {
      if (!((boundary - delta_value) %in% data_plate$start)) {
        stop(sprintf("❌ For plate %d, boundary %s: (n - delta)=%s not found in data.", i, boundary, boundary - delta_value))
      }
      if (!((boundary + delta_value) %in% data_plate$start)) {
        stop(sprintf("❌ For plate %d, boundary %s: (n + delta)=%s not found in data.", i, boundary, boundary + delta_value))
      }
    }
    
    filtered_delta <- data.frame()
    for (boundary in selected_boundaries) {
      before_data <- filter(data_plate, start == boundary - delta_value) %>% mutate(momentum = "before")
      switch_data <- filter(data_plate, start == boundary) %>% mutate(momentum = "switch")
      after_data  <- filter(data_plate, start == boundary + delta_value) %>% mutate(momentum = "after")
      filtered_delta <- rbind(filtered_delta, before_data, switch_data, after_data)
    }
    calculate_delta_means <- function(d) {
      d %>% group_by(condition_tagged, zone, momentum) %>%
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
    
    # Store results
    lineplot_list[[i]] <- normalized_sums
    boxplot_list[[i]] <- boxplot_data
    delta_boxplot_list[[i]] <- delta_boxplot_data
  }
  
  # Combine data from all plates
  final_lineplots <- do.call(rbind, lineplot_list)
  final_boxplots <- do.call(rbind, boxplot_list)
  final_delta_boxplots <- do.call(rbind, delta_boxplot_list)
  
  message("\n🎉 Pretreatment complete!")
  message("💾 Line plot data saved globally as 'pretreated_data_for_lineplots_df'")
  message("💾 Box plot data saved globally as 'pretreated_data_for_boxplots_df'")
  message("💾 Delta boxplot data saved globally as 'pretreated_delta_data_for_boxplots_df'\n")
  
  assign("pretreated_data_for_lineplots_df", final_lineplots, envir = .GlobalEnv)
  assign("pretreated_data_for_boxplots_df", final_boxplots, envir = .GlobalEnv)
  assign("pretreated_delta_data_for_boxplots_df", final_delta_boxplots, envir = .GlobalEnv)
  
  return(list(
    lineplots = final_lineplots,
    boxplots = final_boxplots,
    delta_boxplots = final_delta_boxplots
  ))
}
