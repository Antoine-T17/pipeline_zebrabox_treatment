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
# The computed boundaries are then snapped to the nearest multiple of the integration period,
# using ceiling and enforcing strict monotonicity to prevent collapsed intervals.
# In addition, the full period sequence is stored globally to ensure that downstream
# functions have access to all intended periods.
# -----------------------------------------------------------

assign_periods_with_custom_durations <- function(enriched_data) {
  # Ensure enriched_data is a list for flexible handling of multiple plates.
  if (!is.list(enriched_data)) {
    enriched_data <- list(enriched_data)
  }
  
  message("\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process!")
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
                                      transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
                                      error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.", length(periods) - 1))
  message("✔️ Period boundaries recorded (in seconds): ", paste(boundaries_input, collapse = ", "))
  
  # Compute durations for periods 1 to n-1 based on boundaries.
  durations <- numeric(length(periods))
  durations[1] <- boundaries_input[1]  # Duration of the first period.
  if (length(periods) > 1 && length(boundaries_input) >= 2) {
    for (i in 2:(length(periods) - 1)) {
      durations[i] <- boundaries_input[i] - boundaries_input[i - 1]
    }
  }
  # Note: The last period is open-ended.
  
  # Step 3: Retrieve the integration period from the first plate's data.
  integration_period <- as.numeric(unique(enriched_data[[1]]$period))
  if (length(integration_period) > 1) {
    message("⚠️ Warning: Multiple integration period values found in plate 1. Using the first: ", integration_period[1])
    integration_period <- integration_period[1]
  }
  message("\n🔍 Integration period (minimum duration) extracted from plate 1 data: ", integration_period, " seconds.\n")
  
  # Enforce that each defined period (except the last) is at least as long as the integration period.
  durations[1:(length(periods) - 1)] <- pmax(durations[1:(length(periods) - 1)], integration_period)
  
  # Recompute boundaries using the adjusted durations.
  new_boundaries <- cumsum(durations[1:(length(periods) - 1)])
  
  # --- Snap boundaries to the nearest multiple of the integration period using ceiling ---
  snapped_boundaries <- ceiling(new_boundaries / integration_period) * integration_period
  # Ensure strict monotonicity: if any boundary is not greater than the previous, force an increment.
  for (i in 2:length(snapped_boundaries)) {
    if (snapped_boundaries[i] <= snapped_boundaries[i - 1]) {
      snapped_boundaries[i] <- snapped_boundaries[i - 1] + integration_period
    }
  }
  # Convert snapped boundaries to minutes for storage/consistency.
  period_boundaries <- snapped_boundaries / 60  
  message("✔️ Adjusted (snapped) period boundaries (in minutes): ", paste(period_boundaries, collapse = ", "))
  
  # Create associations between boundaries and period transitions.
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(boundary_time = period_boundaries, transition = period_transitions)
  
  # Step 4: Process each plate's data.
  data_with_periods_list <- list()
  period_boundaries_list <- list()
  boundary_associations_list <- list()
  
  for (p in seq_along(enriched_data)) {
    edata <- enriched_data[[p]]
    
    # Assign periods based on the global adjusted boundaries.
    message(sprintf("🛠️ Plate %d - Assigning periods based on 'start' time...", p))
    assign_period <- function(start_time_sec) {
      for (i in seq_along(period_boundaries)) {
        if (start_time_sec < period_boundaries[i] * 60) return(periods[i])
      }
      return(periods[length(periods)])
    }
    edata <- edata %>% mutate(period_with_numbers = sapply(start, assign_period))
    message(sprintf("🔔 Plate %d - Available periods: %s", p, paste(unique(edata$period_with_numbers), collapse = ", ")))
    
    # Create simplified period column.
    message(sprintf("🛠️ Plate %d - Creating 'period_without_numbers' column...", p))
    edata <- edata %>% mutate(
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^(vibration)") ~ "vibration",
        str_detect(period_with_numbers, "^(rest)") ~ "rest",
        TRUE ~ period_with_numbers
      )
    )
    message(sprintf("🔔 Plate %d - Available periods without numbers: %s", p, paste(unique(edata$period_without_numbers), collapse = ", ")))
    message(sprintf("✔️ Plate %d - Periods assigned and columns created successfully.", p))
    
    data_with_periods_list[[p]] <- edata
    period_boundaries_list[[p]] <- period_boundaries
    boundary_associations_list[[p]] <- boundary_associations
  }
  
  # Save results globally.
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df_list", data_with_periods_list, envir = .GlobalEnv)
  assign("period_boundaries_list", period_boundaries_list, envir = .GlobalEnv)
  assign("boundary_associations_list", boundary_associations_list, envir = .GlobalEnv)
  
  message("🎉 Period assignment completed for all plates!")
  message("💾 Results saved globally as 'data_with_periods_df_list', 'period_boundaries_list', 'boundary_associations_list', and 'all_periods'.\n")
  
  # If originally a single data frame was provided, return that instead of a list.
  if (length(data_with_periods_list) == 1) {
    return(data_with_periods_list[[1]])
  } else {
    return(data_with_periods_list)
  }
}
