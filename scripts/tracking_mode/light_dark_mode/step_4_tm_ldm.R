assign_periods_with_custom_durations <- function(enriched_data_list) {
  # Step 1: Display welcome message with instructions
  message("\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process (Light/Dark Mode)!")
  message("📋 This function will help you:")
  message("   • Define your experimental period sequence and boundaries.")
  message("   • Assign each data row a period based on its 'start' time (for each plate).")
  message("   • Create a simplified period column (without numeric tags).")
  message("   • Save the updated data (a list) globally as 'data_with_periods_df'.\n")
  
  # Step 2: Retrieve pre-recorded inputs from the global pipeline_inputs
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Step 3: Define helper function to obtain and validate user inputs
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(pipeline_inputs[[param]])
      if (validate_fn(candidate)) {
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
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
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 4: Prompt user to define the experimental period sequence (no fixed starting period)
  message("📋 Define your experimental period sequence (comma-separated, e.g., baseline, treatment, recovery):")
  period_sequence_input <- get_input_local("period_sequence",
                                           "❓ Enter the sequence of periods (comma-separated): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             length(periods) > 0
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Period sequence recorded: ", paste(periods, collapse = ", "))
  
  # Save the full period sequence globally (not as a list)
  assign("all_periods", periods, envir = .GlobalEnv)
  
  # Step 5: Prompt user to define period boundaries (time codes in seconds) for each transition
  message("🛠️ Define period boundaries (time codes in seconds) for each transition.")
  boundaries_input <- get_input_local("period_boundaries",
                                      sprintf("❓ Enter %d time codes (comma-separated): ", length(periods) - 1),
                                      validate_fn = function(x) {
                                        boundaries <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                        length(boundaries) == (length(periods) - 1) && all(!is.na(boundaries)) && all(boundaries > 0)
                                      },
                                      transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                      error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.", length(periods) - 1))
  message("✔️ Period boundaries recorded (in seconds): ", paste(boundaries_input, collapse = ", "))
  
  # Step 6: Compute durations for each period (except the last, which is open-ended)
  durations <- numeric(length(periods))
  durations[1] <- boundaries_input[1]
  if (length(periods) > 1 && length(boundaries_input) >= 2) {
    for (i in 2:(length(periods) - 1)) {
      durations[i] <- boundaries_input[i] - boundaries_input[i - 1]
    }
  }
  
  # Step 7: Retrieve integration period from data (from the first enriched data frame)
  integration_period <- as.numeric(unique(enriched_data_list[[1]]$period))
  if (length(integration_period) > 1) {
    message("⚠️ Warning: Multiple integration period values found. Using the first: ", integration_period[1])
    integration_period <- integration_period[1]
  }
  message("\n🔍 Integration period (minimum duration) extracted from data: ", integration_period, " seconds.\n")
  
  # Step 8: Ensure each period (except the last) is at least the integration period long
  durations[1:(length(periods) - 1)] <- pmax(durations[1:(length(periods) - 1)], integration_period)
  
  # Step 9: Recompute boundaries from adjusted durations and snap to integration period multiples
  new_boundaries <- cumsum(durations[1:(length(periods) - 1)])  # in seconds
  snapped_boundaries <- round(new_boundaries / integration_period) * integration_period
  period_boundaries <- snapped_boundaries / 60  # convert seconds to minutes for storage
  message("✔️ Adjusted (snapped) period boundaries (in minutes): ", paste(period_boundaries, collapse = ", "))
  
  # Step 10: Create associations between period transitions and boundaries
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(boundary_time = period_boundaries, transition = period_transitions)
  
  # Save period_boundaries, boundary_associations, and all_periods globally (not as lists)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  
  # Step 11: Define function to assign a period based on a given start time (in seconds)
  assign_period <- function(start_time_sec) {
    for (i in seq_along(period_boundaries)) {
      if (start_time_sec < period_boundaries[i] * 60) return(periods[i])
    }
    return(periods[length(periods)])
  }
  
  # Step 12: Process each enriched data frame in the list and assign periods
  data_with_periods_list <- list()
  for (i in seq_along(enriched_data_list)) {
    message("\n--- Assigning periods for plate ", i, " ---")
    df <- enriched_data_list[[i]]
    # Create new column 'period_with_numbers' by applying assign_period to 'start'
    df$period_with_numbers <- sapply(df$start, assign_period)
    message("✔️ Periods assigned for plate ", i, ".")
    
    # Step 13: Create a simplified period column by removing numeric tags
    df$period_without_numbers <- dplyr::case_when(
      stringr::str_detect(df$period_with_numbers, "^(light)") ~ "light",
      stringr::str_detect(df$period_with_numbers, "^(dark)") ~ "dark",
      TRUE ~ df$period_with_numbers
    )
    message("✔️ 'period_without_numbers' created for plate ", i, ".")
    
    data_with_periods_list[[i]] <- df
  }
  
  # Save the enriched data with periods (as a list) globally
  assign("data_with_periods_df", data_with_periods_list, envir = .GlobalEnv)
  message("\n🎉 Period assignment completed!")
  message("💾 Results saved as 'data_with_periods_df', 'period_boundaries', 'boundary_associations', and 'all_periods'.\n")
  
  return(data_with_periods_list)
}
