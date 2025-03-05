# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light/dark mode
# Function: assign_periods_with_custom_durations
# Purpose: Assigns experimental periods based on user-defined boundaries.
#          Prompts for a universal period sequence and boundaries (not per plate),
#          computes durations, snaps boundaries to the nearest multiple of the integration period,
#          and assigns periods to each plate's enriched data.
#          Global outputs: 'data_with_periods_df_list', 'period_boundaries_list',
#          'boundary_associations_list', and 'boundary_associations'.
# -----------------------------------------------------------
assign_periods_with_custom_durations <- function(enriched_data_list) {
  message("\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process (Light/Dark Mode)!")
  message("📋 This function will help you:")
  message("   • Define your universal experimental period sequence and boundaries.")
  message("   • Assign each data row a period based on its 'start' time.")
  message("   • Create a simplified period column (without numeric tags).")
  message("   • Save the updated data globally as 'data_with_periods_df_list'.\n")
  
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Define unified input helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(pipeline_inputs[[param]])
      if (validate_fn(candidate)) {
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # ----- Universal Period Sequence -----
  period_sequence_input <- get_input_local("period_sequence",
                                           "❓ Enter the universal sequence of periods (comma-separated, e.g., acclimatation, light, dark, light, dark): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             length(periods) > 0
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Universal period sequence recorded: ", paste(periods, collapse = ", "))
  assign("all_periods", periods, envir = .GlobalEnv)
  
  # ----- Universal Period Boundaries -----
  boundaries_input <- get_input_local("period_boundaries",
                                      sprintf("❓ Enter %d time codes (comma-separated) for the universal period boundaries (in seconds): ", length(periods) - 1),
                                      validate_fn = function(x) {
                                        boundaries <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                        length(boundaries) == (length(periods) - 1) && all(!is.na(boundaries)) && all(boundaries > 0)
                                      },
                                      transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                      error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.", length(periods) - 1))
  message("✔️ Universal period boundaries recorded (in seconds): ", paste(boundaries_input, collapse = ", "))
  
  # Prepare lists to store outputs per plate.
  data_with_periods_list <- list()
  period_boundaries_list <- list()
  boundary_associations_list <- list()
  
  # Process each plate's enriched data using the universal period sequence & boundaries.
  for (i in seq_along(enriched_data_list)) {
    edata <- enriched_data_list[[i]]
    
    durations <- numeric(length(periods))
    durations[1] <- boundaries_input[1]  # Duration of the first period.
    if (length(periods) > 1 && length(boundaries_input) >= 2) {
      for (j in 2:(length(periods) - 1)) {
        durations[j] <- boundaries_input[j] - boundaries_input[j - 1]
      }
    }
    
    # Retrieve integration period from data (assumed in column 'period').
    integration_period <- as.numeric(unique(edata$period))
    if (length(integration_period) > 1) {
      message(sprintf("⚠️ Warning (plate %d): Multiple integration period values found. Using the first: %s", i, integration_period[1]))
      integration_period <- integration_period[1]
    }
    message(sprintf("\n🔍 Plate %d - Integration period (minimum duration) extracted from data: %s seconds.\n", i, integration_period))
    
    durations[1:(length(periods) - 1)] <- pmax(durations[1:(length(periods) - 1)], integration_period)
    new_boundaries <- cumsum(durations[1:(length(periods) - 1)])
    snapped_boundaries <- round(new_boundaries / integration_period) * integration_period
    plate_boundaries <- snapped_boundaries / 60  # Converting seconds to minutes.
    message(sprintf("✔️ Plate %d - Adjusted (snapped) period boundaries (in minutes): %s", i, paste(plate_boundaries, collapse = ", ")))
    
    period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
    boundary_associations <- data.frame(boundary_time = plate_boundaries, transition = period_transitions)
    
    # Assign periods based on the adjusted boundaries.
    message(sprintf("🛠️ Plate %d - Assigning periods based on 'start' time...", i))
    assign_period <- function(start_time_sec) {
      for (k in seq_along(plate_boundaries)) {
        if (start_time_sec < plate_boundaries[k] * 60) return(periods[k])
      }
      return(periods[length(periods)])
    }
    edata <- edata %>% mutate(period_with_numbers = sapply(start, assign_period))
    message(sprintf("✔️ Plate %d - Periods assigned successfully.", i))
    
    # Create simplified period column (removing numeric tags).
    message(sprintf("🛠️ Plate %d - Creating 'period_without_numbers' column...", i))
    edata <- edata %>% mutate(
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^(light)") ~ "light",
        str_detect(period_with_numbers, "^(dark)") ~ "dark",
        TRUE ~ period_with_numbers
      )
    )
    message(sprintf("✔️ Plate %d - 'period_without_numbers' created.", i))
    
    data_with_periods_list[[i]] <- edata
    period_boundaries_list[[i]] <- plate_boundaries
    boundary_associations_list[[i]] <- boundary_associations
  }
  
  # Create a universal boundary associations data frame.
  # (Assumes that boundaries are consistent across plates; otherwise, the first plate's data is used.)
  if (length(boundary_associations_list) > 0) {
    boundary_associations <- boundary_associations_list[[1]]
    assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  }
  
  message("🎉 Period assignment completed for all plates!")
  message("💾 Results saved globally as 'data_with_periods_df_list', 'period_boundaries_list', and 'boundary_associations_list'.\n")
  assign("data_with_periods_df_list", data_with_periods_list, envir = .GlobalEnv)
  assign("period_boundaries_list", period_boundaries_list, envir = .GlobalEnv)
  assign("boundary_associations_list", boundary_associations_list, envir = .GlobalEnv)
  
  return(data_with_periods_list)
}
