assign_periods_with_custom_durations <- function(enriched_data) {

  message("\n---\n---\n---\n")
  
  # Welcome message
  message("\n👋 Welcome to the Custom Period Assignment Process!\n")
  message("This function helps you:\n")
  message("🔗 Use your enriched data file to assign periods based on your experimental protocol.")
  message("🕒 Define a sequence of periods and their boundaries.")
  message("🛠️ Assign these periods to your experimental data.")
  message("🔍 Associate period boundaries with transitions between periods.")
  message("💾 Save the enriched data with newly assigned period columns.\n")
  
  # Step 1: Prompt user for period names
  message("📋 Let's define your experimental sequence.")
  repeat {
    periods_input <- readline(prompt = "Enter the sequence of periods, starting with an acclimatation phase, separated by commas (e.g., acclimatation, vibration_1, rest_1, vibration_2, rest_2,...): ")
    periods <- trimws(unlist(strsplit(periods_input, ",")))
    
    # Check if "acclimatation" is present in the input
    if (length(periods) == 0) {
      message("⚠️ The sequence of periods cannot be empty. Please try again.")
    } else if (!"acclimatation" %in% periods) {
      message("❌ The sequence must include 'acclimatation' as the first period. Please try again.")
    } else {
      message("✔️ Sequence of periods successfully recorded: ", paste(periods, collapse = ", "))
      break
    }
  }
  
  # Step 2: Prompt user for all period boundaries at once
  message("🔧 Now, let's define the period boundaries manually.")
  message("ℹ️ Period boundaries represent the exact time codes (in seconds) where a switch occurs between two consecutive periods.")
  
  # Show the full sequence of periods
  message("📋 Your full sequence of periods is: ", paste(periods, collapse = ", "))
  message("💡 Enter all boundaries in one line, separated by commas (e.g., '1800, 1802, 1920, 1922, 2040, 2042').")
  
  repeat {
    boundaries_input <- readline(prompt = "Enter the time codes (in seconds) for all switches: ")
    period_boundaries <- as.numeric(trimws(unlist(strsplit(boundaries_input, ","))))
    if (length(period_boundaries) == length(periods) - 1 && all(!is.na(period_boundaries) & period_boundaries > 0)) {
      message("✔️ Period boundaries successfully recorded: ", paste(period_boundaries, collapse = ", "))
      break
    } else {
      message("⚠️ Invalid input. Please ensure all time codes are positive numbers separated by commas and match the number of switches (", length(periods) - 1, ").")
    }
  }
  
  # Convert boundaries to minutes
  period_boundaries <- period_boundaries / 60
  
  # Create associations between boundaries and transitions
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")  # Generate transitions
  boundary_associations <- data.frame(
    boundary_time = period_boundaries,
    transition = period_transitions
  )
  
  # Step 3: Assign periods based on boundaries
  message("🛠️ Assigning periods to data based on boundaries...")
  
  # Ensure 'start' column exists
  if (!"start" %in% colnames(enriched_data)) {
    stop("❌ Error: The 'start' column is missing in the dataset.")
  }
  
  assign_period <- function(start_time_sec) {
    for (i in seq_along(period_boundaries)) {
      if (start_time_sec < period_boundaries[i] * 60) {  # Convert boundary back to seconds for comparison
        return(periods[i])
      }
    }
    return(periods[length(periods)])  # Assign the last period if time exceeds the last boundary
  }
  
  enriched_data <- enriched_data %>%
    mutate(period_with_numbers = sapply(start, assign_period))
  
  # Debugging assignment
  message("🔍 Debugging Period Assignment...")
  message("🔍 User-defined periods: ", paste(periods, collapse = ", "))
  message("🔍 Assigned periods in the dataset: ", paste(unique(enriched_data$period_with_numbers), collapse = ", "))
  
  message("✔️ Periods successfully assigned to the data.")
  
  # Step 4: Create 'period_without_numbers' column
  message("🛠️ Creating the 'period_without_numbers' column...")
  enriched_data <- enriched_data %>%
    mutate(
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^vibration") ~ "vibration",
        str_detect(period_with_numbers, "^rest") ~ "rest",
        TRUE ~ period_with_numbers
      )
    )
  message("✔️ 'period_without_numbers' column successfully created.")
  
  # Step 5: Save enriched data and associations to the global environment
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("🎉 Boundary associations created!")
  message("🎉 Period assignment completed successfully!")
  message("💾 The enriched data has been saved in the global environment as 'data_with_periods_df'.")
  message("💾 The period_boundaries has been saved in the global environment as 'period_boundaries'.")
  message("💾 The boundary-to-transition associations have been saved as 'boundary_associations'.\n")
  
  return(enriched_data)
}
