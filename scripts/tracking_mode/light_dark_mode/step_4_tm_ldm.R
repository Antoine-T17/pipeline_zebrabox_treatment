# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light/dark mode
# Function: assign_periods_with_custom_durations
# Purpose: Assigns experimental periods based on user-defined boundaries.
#          It prompts for a period sequence and boundaries, computes durations
#          (ensuring a minimum equal to the integration period), snaps boundaries
#          to the nearest multiple of the integration period, and assigns periods.
#          The full period sequence and adjusted boundaries are saved globally.
# -----------------------------------------------------------
assign_periods_with_custom_durations <- function(enriched_data) {
  # Step 1: Display welcome message with bullet points
  message("\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process (Light/Dark Mode)!")
  message("📋 This function will help you:")
  message("   • Define your experimental period sequence and boundaries.")
  message("   • Assign each data row a period based on its 'start' time.")
  message("   • Create a simplified period column (without numeric tags).")
  message("   • Save the updated data and associations globally.\n")
  
  # Step 2: Retrieve pre-recorded inputs from the global pipeline_inputs
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Step 3: Define a helper function to obtain and validate user inputs
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
  
  # Step 4: Prompt user to define the experimental period sequence
  message("📋 Define your experimental period sequence (starting with 'acclimatation').")
  period_sequence_input <- get_input_local("period_sequence",
                                           "❓ Enter the sequence of periods (comma-separated, e.g., acclimatation, light, dark, light, dark, light, dark): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             length(periods) > 0 && ("acclimatation" %in% periods)
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Period sequence recorded: ", paste(periods, collapse = ", "))
  
  # Save the full period sequence globally for downstream functions.
  assign("all_periods", periods, envir = .GlobalEnv)
  
  # Step 5: Prompt user to define period boundaries (time codes in seconds)
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
  
  # Step 6: Compute durations for periods (except the last which is open-ended)
  durations <- numeric(length(periods))
  durations[1] <- boundaries_input[1]  # Duration of the first period
  if (length(periods) > 1 && length(boundaries_input) >= 2) {
    for (i in 2:(length(periods) - 1)) {
      durations[i] <- boundaries_input[i] - boundaries_input[i - 1]
    }
  }
  
  # Step 7: Retrieve integration period from data (minimum period duration)
  integration_period <- as.numeric(unique(enriched_data$period))
  if (length(integration_period) > 1) {
    message("⚠️ Warning: Multiple integration period values found. Using the first: ", integration_period[1])
    integration_period <- integration_period[1]
  }
  message("\n🔍 Integration period (minimum duration) extracted from data: ", integration_period, " seconds.\n")
  
  # Step 8: Ensure each period (except the last) is at least the integration period long
  durations[1:(length(periods) - 1)] <- pmax(durations[1:(length(periods) - 1)], integration_period)
  
  # Step 9: Recompute boundaries from the adjusted durations and snap to integration period multiples
  new_boundaries <- cumsum(durations[1:(length(periods) - 1)])  # in seconds
  snapped_boundaries <- round(new_boundaries / integration_period) * integration_period
  period_boundaries <- snapped_boundaries / 60  # convert seconds to minutes for storage
  message("✔️ Adjusted (snapped) period boundaries (in minutes): ", paste(period_boundaries, collapse = ", "))
  
  # Step 10: Create associations between period transitions and boundaries
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(boundary_time = period_boundaries, transition = period_transitions)
  
  # Step 11: Assign periods based on the adjusted boundaries
  message("🛠️ Assigning periods based on 'start' time...")
  assign_period <- function(start_time_sec) {
    for (i in seq_along(period_boundaries)) {
      if (start_time_sec < period_boundaries[i] * 60) return(periods[i])
    }
    return(periods[length(periods)])
  }
  enriched_data <- enriched_data %>% mutate(period_with_numbers = sapply(start, assign_period))
  message("✔️ Periods assigned successfully.")
  
  # Step 12: Create simplified period column by removing numeric tags
  message("🛠️ Creating 'period_without_numbers' column...")
  enriched_data <- enriched_data %>% mutate(
    period_without_numbers = case_when(
      str_detect(period_with_numbers, "^(light)") ~ "light",
      str_detect(period_with_numbers, "^(dark)") ~ "dark",
      TRUE ~ period_with_numbers
    )
  )
  message("✔️ 'period_without_numbers' created.")
  
  # Step 13: Save results globally and return the enriched data
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("🎉 Period assignment completed!")
  message("💾 Results saved as 'data_with_periods_df', 'period_boundaries', 'boundary_associations', and 'all_periods'.\n")
  
  return(enriched_data)
}
