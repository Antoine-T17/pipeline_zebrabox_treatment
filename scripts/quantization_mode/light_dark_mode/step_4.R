assign_periods_with_custom_durations <- function(enriched_data) {
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  }))
  
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("👋 Welcome to the Custom Period Assignment Process!\n")
  message("This function helps you:\n")
  message("🔗 Use your enriched data file to assign periods based on your experimental protocol.")
  message("🕒 Define a sequence of periods and their durations, starting with an acclimatation phase.")
  message("🛠️ Assign these periods to your experimental data.")
  message("💾 Save the enriched data with newly assigned period columns.\n")
  
  # Step 1: Prompt user for period names
  message("📋 Let's define your experimental sequence.")
  repeat {
    periods_input <- readline(prompt = "Enter the sequence of periods, starting with an acclimatation phase, separated by commas (e.g., acclimatation, light_1, dark_1, light_2, dark_2,...): ")
    periods <- trimws(unlist(strsplit(periods_input, ",")))
    if (length(periods) > 0) {
      message("✔️ Sequence of periods successfully recorded: ", paste(periods, collapse = ", "), "\n")
      break
    } else {
      message("⚠️ The sequence of periods cannot be empty. Please try again.\n")
    }
  }
  
  # Step 2: Prompt user for durations in minutes
  durations <- numeric(length(periods))
  for (i in seq_along(periods)) {
    repeat {
      duration <- as.numeric(readline(prompt = paste0("⏳ Enter the duration of '", periods[i], "' in minutes: ")))
      if (!is.na(duration) && duration > 0) {
        durations[i] <- duration
        message("✔️ Duration for '", periods[i], "' successfully recorded: ", duration, " minutes.\n")
        break
      } else {
        message("⚠️ Duration must be a positive number. Please try again.\n")
      }
    }
  }
  
  # Step 3: Convert durations (minutes) to seconds for internal calculations
  durations_in_seconds <- durations * 60
  cumulative_durations <- cumsum(durations_in_seconds)
  
  message("\n🛠️ Assigning periods based on user-defined durations...")
  
  # Helper function to determine which period applies to a given start_time (in seconds)
  determine_period <- function(start_time_sec) {
    for (i in seq_along(periods)) {
      lower_bound <- ifelse(i == 1, 0, cumulative_durations[i - 1])
      if (start_time_sec >= lower_bound && start_time_sec < cumulative_durations[i]) {
        return(periods[i])
      }
    }
    return(NA)
  }
  
  # Map each 'start' to the correct period based on durations_in_seconds
  enriched_data <- enriched_data %>%
    mutate(period_with_numbers = sapply(start, determine_period))
  
  message("✔️ Periods successfully assigned to the data.\n")
  
  # Step 4: Create 'period_without_numbers' column (generalized light/dark periods)
  message("🛠️ Creating the 'period_without_numbers' column...")
  enriched_data <- enriched_data %>%
    mutate(
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^light") ~ "light",
        str_detect(period_with_numbers, "^dark") ~ "dark",
        TRUE ~ period_with_numbers
      )
    )
  message("✔️ 'period_without_numbers' column successfully created.\n")
  
  # Step 5: Store period boundaries for light and dark periods only
  message("🛠️ Extracting period boundaries for light and dark periods...")
  light_dark_indices <- which(!str_detect(periods, "^acclimatation"))
  period_boundaries <- c(0, cumsum(durations)[-length(durations)]) # Exclude final cumulative duration
  period_boundaries <- period_boundaries[light_dark_indices]      # Include only light/dark periods
  
  # Save period boundaries as an attribute and in the global environment
  attr(enriched_data, "period_boundaries") <- period_boundaries
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("✔️ Period boundaries successfully saved in the global environment as 'period_boundaries'.\n")
  
  # Step 6: Save enriched data with periods to the global environment
  message("🎉 Period assignment completed successfully!")
  message("💾 The enriched data has been saved in the global environment as 'data_with_periods_df'.\n")
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  
  return(enriched_data)
}
