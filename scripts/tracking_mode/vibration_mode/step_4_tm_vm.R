# -----------------------------------------------------------
# File: assign_periods_with_custom_durations.R
# Harmonized version of assign_periods_with_custom_durations for vibration mode.
# This function assigns experimental periods based on user-defined boundaries.
# It prompts for the period sequence and boundaries, assigns periods accordingly,
# creates an additional column stripping numeric tags, and saves the results globally.
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
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
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
                                           "❓ Enter the sequence of periods (comma-separated, e.g., acclimatation, light_1, dark_1,...): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             length(periods) > 0 && ("acclimatation" %in% periods)
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Period sequence recorded: ", paste(periods, collapse = ", "))
  
  # Step 2: Define period boundaries.
  message("🛠️ Define period boundaries (time codes in seconds) for each transition.")
  boundaries_input <- get_input_local("period_boundaries",
                                      sprintf("❓ Enter %d time codes (comma-separated): ", length(periods) - 1),
                                      validate_fn = function(x) {
                                        boundaries <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
                                        length(boundaries) == length(periods) - 1 && all(!is.na(boundaries)) && all(boundaries > 0)
                                      },
                                      transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                      error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.", length(periods) - 1))
  period_boundaries <- boundaries_input / 60  # Convert seconds to minutes
  message("✔️ Period boundaries recorded (in minutes): ", paste(period_boundaries, collapse = ", "))
  
  # Create associations.
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(boundary_time = period_boundaries, transition = period_transitions)
  
  # Assign periods based on boundaries.
  message("🛠️ Assigning periods based on 'start' time...")
  assign_period <- function(start_time_sec) {
    for (i in seq_along(period_boundaries)) {
      if (start_time_sec < period_boundaries[i] * 60) return(periods[i])
    }
    return(periods[length(periods)])
  }
  enriched_data <- enriched_data %>% mutate(period_with_numbers = sapply(start, assign_period))
  message("✔️ Periods assigned successfully.")
  
  # Create simplified period column.
  message("🛠️ Creating 'period_without_numbers' column...")
  enriched_data <- enriched_data %>% mutate(
    period_without_numbers = case_when(
      str_detect(period_with_numbers, "^light") ~ "light",
      str_detect(period_with_numbers, "^dark") ~ "dark",
      TRUE ~ period_with_numbers
    )
  )
  message("✔️ 'period_without_numbers' created.")
  
  # Save results globally.
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("🎉 Period assignment completed!")
  message("💾 Results saved as 'data_with_periods_df', 'period_boundaries', and 'boundary_associations'.\n")
  
  return(enriched_data)
}
