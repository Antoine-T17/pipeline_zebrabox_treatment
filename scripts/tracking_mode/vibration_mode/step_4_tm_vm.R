# -----------------------------------------------------------
# File: assign_periods_with_custom_durations.R
# Harmonized version of assign_periods_with_custom_durations for vibration mode.
# This function assigns experimental periods based on user-defined boundaries.
# It prompts for the period sequence and boundaries, assigns periods accordingly,
# creates a simplified period column (without numeric tags), and saves the results globally.
#
# Correction: The function computes durations from the provided boundaries and
# enforces a minimum duration equal to the integration period (extracted from the 'period' column).
# This ensures that very narrow periods (e.g. vibration_3) are expanded if necessary.
# The computed boundaries are then snapped to the nearest multiple of the integration period.
# In addition, the full period sequence is stored globally to ensure that downstream
# functions (e.g., function 7) have access to all intended periods.
# -----------------------------------------------------------

assign_periods_with_custom_durations <- function(enriched_data) {
  message("\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process!\n")
  message("📋 This function will help you:")
  message("   • Define your experimental period sequence and boundaries.")
  message("   • Assign each data row a period based on its 'start' time.")
  message("   • Create a simplified period column (without numeric tags).")
  message("   • Save the updated data and associations globally.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Unified helper to retrieve inputs.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && 
        !is.na(pipeline_inputs[[param]]) && 
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
  
  # Step 1: Define the sequence of periods.
  message("📋 Define your experimental period sequence (starting with 'acclimatation').")
  period_sequence_input <- get_input_local("period_sequence",
                                           "❓ Enter the sequence of periods (comma-separated, e.g., acclimatation, vibration_1, rest_1, vibration_2, rest_2, vibration_3, rest_3): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             length(periods) > 0 && ("acclimatation" %in% periods)
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Period sequence recorded: ", paste(periods, collapse = ", "))
  
  # Save the full period sequence globally for downstream functions.
  assign("all_periods", periods, envir = .GlobalEnv)
  
  # Step 2: Define period boundaries (in seconds) for each transition.
  message("🛠️ Define period boundaries (time codes in seconds) for each transition.")
  boundaries_input <- get_input_local("period_boundaries",
                                      sprintf("❓ Enter %d time codes (comma-separated): ", length(periods) - 1),
                                      validate_fn = function(x) {
                                        boundaries <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                        length(boundaries) == (length(periods) - 1) && all(!is.na(boundaries)) && all(boundaries > 0)
                                      },
                                      transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))) ,
                                      error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.", length(periods) - 1))
  message("✔️ Period boundaries recorded (in seconds): ", paste(boundaries_input, collapse = ", "))
  
  # Compute durations (in seconds) for periods 1 to n-1:
  durations <- numeric(length(periods))
  durations[1] <- boundaries_input[1]  # Duration of first period
  if (length(periods) > 1 && length(boundaries_input) >= 2) {
    for (i in 2:(length(periods) - 1)) {
      durations[i] <- boundaries_input[i] - boundaries_input[i - 1]
    }
  }
  # Note: The last period is open-ended and not assigned a duration.
  
  # Step 3: Retrieve the integration period from the data's "period" column.
  integration_period <- as.numeric(unique(enriched_data$period))
  if (length(integration_period) > 1) {
    message("⚠️ Warning: Multiple integration period values found. Using the first: ", integration_period[1])
    integration_period <- integration_period[1]
  }
  message("\n🔍 Integration period (minimum duration) extracted from data: ", integration_period, " seconds.\n")
  
  # Enforce that each defined period (except the last) is at least as long as the integration period.
  durations[1:(length(periods) - 1)] <- pmax(durations[1:(length(periods) - 1)], integration_period)
  
  # Recompute boundaries using the adjusted durations.
  new_boundaries <- cumsum(durations[1:(length(periods) - 1)])  # in seconds
  
  # --- Snap boundaries to the nearest multiple of the integration period ---
  snapped_boundaries <- round(new_boundaries / integration_period) * integration_period
  # Convert snapped boundaries to minutes for storage/consistency.
  period_boundaries <- snapped_boundaries / 60  
  message("✔️ Adjusted (snapped) period boundaries (in minutes): ", paste(period_boundaries, collapse = ", "))
  
  # Create associations.
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(boundary_time = period_boundaries, transition = period_transitions)
  
  # Assign periods based on the adjusted boundaries.
  message("🛠️ Assigning periods based on 'start' time...")
  assign_period <- function(start_time_sec) {
    # start_time_sec is in seconds.
    for (i in seq_along(period_boundaries)) {
      # Compare against the adjusted boundary (converted back to seconds).
      if (start_time_sec < period_boundaries[i] * 60) return(periods[i])
    }
    return(periods[length(periods)])
  }
  enriched_data <- enriched_data %>% mutate(period_with_numbers = sapply(start, assign_period))
  message("✔️ Periods assigned successfully.")
  
  # Create simplified period column (mapping vibration_*/rest_* to 'vibration' and 'rest').
  message("🛠️ Creating 'period_without_numbers' column...")
  enriched_data <- enriched_data %>% mutate(
    period_without_numbers = case_when(
      str_detect(period_with_numbers, "^(vibration)") ~ "vibration",
      str_detect(period_with_numbers, "^(rest)") ~ "rest",
      TRUE ~ period_with_numbers
    )
  )
  message("✔️ 'period_without_numbers' created.")
  
  # Save results globally.
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("🎉 Period assignment completed!")
  message("💾 Results saved as 'data_with_periods_df', 'period_boundaries', 'boundary_associations', and 'all_periods'.\n")
  
  return(enriched_data)
}
