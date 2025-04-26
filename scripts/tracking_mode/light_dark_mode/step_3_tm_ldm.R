# -----------------------------------------------------------
# Function: create_data_frame
# Purpose: Combine processed zone data and generate summary datasets
# -----------------------------------------------------------
create_data_frame <- function(zone_calculated_list) {
  # TAG: UNIFIED WELCOME MESSAGE
  message("\n---\n")
  message("👋 Welcome to the Combined DataFrame Creation and Plot Data Generator! \n")
  message("📊 This function will process the final data into summarized forms ready for plotting:")
  message("   • Merge all plate zone data")
  message("   • Count wells per condition/zone")
  message("   • Generate cumulative boxplot, lineplot, and heatmap datasets")
  message("   • Generate normalized lineplot, light/dark boxplot, and delta boxplot datasets")
  message("   • Export these as global objects\n")
  
  # TAG: HELPER FUNCTIONS
  convert_numeric_cols <- function(df, cols) {
    for (col in intersect(names(df), cols)) {
      df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
    }
    df
  }
  
  get_input_local <- function(param, prompt_msg,
                              validate_fn = function(x) TRUE,
                              transform_fn  = function(x) x,
                              error_msg    = "❌ Invalid input. Please try again.") {
    if (!is.null(input_record_list[[param]])) {
      message("💾 Using cached input for '", param, "'.")
      return(input_record_list[[param]])
    }
    if (!is.null(pipeline_inputs[[param]]) &&
        !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      raw       <- pipeline_inputs[[param]]
      candidate <- transform_fn(raw)
      if (validate_fn(raw)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("💾 Using pre-recorded input for '", param, "': ",
                  paste(candidate, collapse = ", "))
        } else {
          message("💾 Using pre-recorded input for '", param, "': ", candidate)
        }
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param,
                "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate  <- transform_fn(user_input)
      if (validate_fn(user_input)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("✔️ Input for '", param, "' recorded: ",
                  paste(candidate, collapse = ", "))
        } else {
          message("✔️ Input for '", param, "' recorded: ", candidate)
        }
        return(candidate)
      }
      message(error_msg)
    }
  }
  
  # TAG: INPUT COLLECTION
  aggregation_period <- get_input_local(
    "aggregation_period",
    "❓ Enter aggregation period in seconds (e.g., 60): ",
    validate_fn = function(x) !is.na(as.numeric(x)) && as.numeric(x) > 0,
    transform_fn = function(x) as.numeric(trimws(x)),
    error_msg = "❌ Please enter a positive number for aggregation period."
  )
  light_period <- get_input_local(
    "light_period",
    "❓ Enter light period(s) to include (comma-separated, e.g., light1,light2): ",
    validate_fn = function(x) length(trimws(unlist(strsplit(x, ",")))) > 0,
    transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
    error_msg = "❌ Please enter at least one light period."
  )
  dark_period <- get_input_local(
    "dark_period",
    "❓ Enter dark period(s) to include (comma-separated, e.g., dark1,dark2): ",
    validate_fn = function(x) length(trimws(unlist(strsplit(x, ",")))) > 0,
    transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
    error_msg = "❌ Please enter at least one dark period."
  )
  selected_boundaries <- get_input_local(
    "selected_period_boundaries",
    "❓ Enter one or more period boundaries (comma-separated): ",
    validate_fn = function(x) {
      vals <- as.numeric(trimws(unlist(strsplit(x, ","))))
      exists("boundary_associations_list", envir = .GlobalEnv) &&
        length(vals) > 0 &&
        all(sapply(vals, function(v) {
          any(abs(v - as.numeric(boundary_associations_list[[1]]$boundary_time)) < 0.2)
        }))
    },
    transform_fn = function(x) as.numeric(trimws(unlist(strsplit(x, ",")))),
    error_msg = "❌ Invalid boundaries. Enter numeric values matching the rounded list (within tolerance)."
  )
  delta_value <- get_input_local(
    "delta_value",
    "❓ Enter the delta value (numeric) (e.g., 1, 2, 5): ",
    validate_fn = function(x) { val <- as.numeric(trimws(x)); !is.na(val) && val > 0 },
    transform_fn = function(x) as.numeric(trimws(x)),
    error_msg = "❌ Enter a positive numeric delta."
  )
  
  # TAG: INITIAL AGGREGATION & COUNT
  response_vars <- c(
    "totaldist","totaldur","totalct",
    "smldist","smldur","smlct",
    "lardist","lardur","larct",
    "emptydur","emptyct"
  )

  all_zone_combined <- bind_rows(lapply(zone_calculated_list, `[[`, "zone_combined"))
  assign("all_zone_combined_df", all_zone_combined, envir = .GlobalEnv)
  message("✔️ All cleaned zone data successfully combined.")
  
  write.xlsx(all_zone_combined_df, "all_zone_combined_df.xlsx")
  
  wells_per_condition <- all_zone_combined %>%
    group_by(condition_grouped, zone, plate_id) %>%
    summarise(n_wells_plate = n_distinct(animal), .groups="drop") %>%
    group_by(condition_grouped, zone) %>%
    summarise(n_wells = sum(n_wells_plate), .groups="drop")
  
  assign("wells_per_condition_df", wells_per_condition, envir = .GlobalEnv)
  message("✔️ Well counts per condition and zone computed")
  
  wells_per_plate <- all_zone_combined %>%
    distinct(condition_grouped, zone, plate_id, animal) %>% 
    # une ligne par puits, plaque, condition, zone
    group_by(condition_grouped, zone, plate_id) %>%
    summarise(n_wells_plate = n(), .groups = "drop")
  
  assign("wells_per_plate_df", wells_per_plate, envir = .GlobalEnv)
  message("✔️ Well counts per condition, zone & plate computed.")
  
  
  # TAG: HELPER FUNCTIONS – SUMMARY CALCS
  summarize_cum_box <- function(var) {
    all_zone_combined %>%
      group_by(condition_grouped, zone, plate_id, animal) %>%
      summarise(
        cum              = sum(.data[[var]], na.rm = TRUE),
        condition_tagged = first(condition_tagged),
        .groups = "drop"
      ) %>%
      left_join(
        wells_per_plate,                                 # <— table par plaque
        by = c("condition_grouped","zone","plate_id")
      ) %>%
      mutate(
        cum_per_well = cum / n_wells_plate               # <— normalisation plaque par plaque
      )
  }
  
  summarize_cum_heat <- function(var) {
    all_zone_combined %>%
      filter(zone %in% c(1,2)) %>%
      group_by(condition_grouped, zone) %>%
      summarise(total_val = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
      left_join(wells_per_condition, by = c("condition_grouped","zone")) %>%
      mutate(val_per_well = total_val / n_wells)
  }
  summarize_line <- function(var) {
    all_zone_combined %>%
      mutate(start_rounded = floor(start / (aggregation_period/60)) * (aggregation_period/60)) %>%
      group_by(condition_grouped, zone, start_rounded) %>%
      summarise(total_val = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
      left_join(wells_per_condition, by = c("condition_grouped","zone")) %>%
      mutate(val_per_well = total_val / n_wells)
  }
  calculate_means <- function(var) {
    all_zone_combined %>% 
    filter(period_with_numbers %in% c(light_period, dark_period)) %>%
    group_by(period_without_numbers, zone, condition_tagged, plate_id) %>%
    summarise(
      plate_id = first(plate_id),
      start = first(start),
      period_with_numbers = first(period_with_numbers),
      condition_grouped = first(condition_grouped),
      condition = first(condition),
      animal = first(animal),
      mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  }
  calculate_delta_means <- function(var,
                                    boundaries = selected_boundaries,
                                    delta      = delta_value,
                                    data       = all_zone_combined) {
    # 1) S’assurer que boundaries et delta sont numériques
    boundaries <- as.numeric(boundaries)
    delta      <- as.numeric(delta)
    
    # 2) Empiler en une passe les points « before / switch / after »
    df_momentum <- map_dfr(boundaries, function(b) {
      data %>%
        filter(start %in% c(b - delta, b, b + delta)) %>%
        mutate(momentum = case_when(
          start == b - delta ~ "before",
          start == b         ~ "switch",
          start == b + delta ~ "after"
        ))
    })
    
    # 3) Grouper PAR PLAQUE, PAR PUITS, PAR MOMENTUM et calculer la moyenne
    df_momentum %>%
      group_by(condition_tagged, zone, plate_id, momentum) %>%
      summarise(
        condition_grouped = first(condition_grouped),
        animal            = first(animal),
        mean_val          = mean(.data[[var]], na.rm = TRUE),
        .groups           = "drop"
      )
  }
  
  # TAG: GENERATE SUMMARY LISTS
  message("🔄 Generating cumulative per-well boxplot datasets...")
  all_zone_combined_cum_boxplots  <- setNames(lapply(response_vars, summarize_cum_box), response_vars)
  message("✔️ all_zone_combined_cum_boxplots created.")

  message("🔄 Generating cumulative per-well heatmap datasets...")
  all_zone_combined_cum_heatmap       <- setNames(lapply(response_vars, summarize_cum_heat), response_vars)
  message("✔️ all_zone_combined_cum_heatmap created.")
  
  message("🔄 Generating normalized lineplot dataset...")
  all_zone_combined_lineplots       <- setNames(lapply(response_vars, summarize_line), response_vars)
  message("✔️ all_zone_combined_lineplots created.")
  
  message("🔄 Generating light/dark boxplot dataset...")
  all_zone_combined_light_dark_boxplots <- setNames(lapply(response_vars, calculate_means), response_vars)
  message("✔️ all_zone_combined_light_dark_boxplots created.")
  
  message("🔄 Generating delta boxplot dataset...")
  all_zone_combined_delta_boxplots <- setNames(lapply(response_vars, calculate_delta_means), response_vars)
  message("✔️ all_zone_combined_delta_boxplots created.")
  
  # TAG: ASSIGN GLOBAL OBJECTS
  assign("all_zone_combined_cum_boxplots",  all_zone_combined_cum_boxplots,  envir = .GlobalEnv)
  assign("all_zone_combined_cum_heatmap",   all_zone_combined_cum_heatmap, envir = .GlobalEnv)
  assign("all_zone_combined_lineplots",    all_zone_combined_lineplots, envir = .GlobalEnv)
  assign("all_zone_combined_light_dark_boxplots", all_zone_combined_light_dark_boxplots, envir = .GlobalEnv)
  assign("all_zone_combined_delta_boxplots",  all_zone_combined_delta_boxplots, envir = .GlobalEnv)
  
  message("💾 Global data objects created:")
  message("    - 'all_zone_combined_cum_boxplots': cum per well for boxplots")
  message("    - 'all_zone_combined_cum_heatmap': totals per zone for heatmap")
  message("    - 'all_zone_combined_lineplots': aggregated per-time sums per well")
  message("    - 'all_zone_combined_light_dark_boxplots': boxplot data for light/dark periods")
  message("    - 'all_zone_combined_delta_boxplots': delta boxplot data (before/switch/after)")
  message("\n✅ All summary datasets are ready!\n")
}
