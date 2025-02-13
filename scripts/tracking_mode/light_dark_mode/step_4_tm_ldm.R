# -----------------------------------------------------------
# assign_periods_with_custom_durations.R
# -----------------------------------------------------------
# This function assigns experimental periods to the enriched data 
# based on a custom experimental protocol. It performs the following:
#
#   1. Prompts for a sequence of periods (starting with "acclimatation").
#   2. Prompts for the period boundaries (time codes in seconds) that 
#      indicate transitions between consecutive periods.
#   3. Converts the boundaries to minutes.
#   4. Creates associations between boundaries and period transitions.
#   5. Assigns a period to each data row based on its "start" time.
#   6. Creates an additional column with period names stripped of numeric tags.
#   7. Saves the updated data and associations in the global environment.
#
# Note: This function records its used inputs in the global list 
#       'input_record_list'. Ensure that this global variable is 
#       initialized before running the pipeline.
# -----------------------------------------------------------

assign_periods_with_custom_durations <- function(enriched_data) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Custom Period Assignment Process!\n")
  message("This function helps you:")
  message("  • Use your enriched data to assign experimental periods.")
  message("  • Define a sequence of periods and their durations (starting with 'acclimatation').")
  message("  • Assign these periods to your experimental data.")
  message("  • Associate period boundaries with transitions.")
  message("  • Save the updated data and associations globally.\n")
  
  # Load pre-recorded inputs for period assignment.
  pipeline_inputs <- list()
  inputs_path <- "inputs/tracking_mode/light_dark_mode/inputs_values"
  inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
  inputs_file_csv <- file.path(inputs_path, "pipeline_inputs.csv")
  
  if (file.exists(inputs_file_xlsx)) {
    df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.xlsx file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  } else if (file.exists(inputs_file_csv)) {
    df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.csv file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  }
  
  # Local helper to get input and record it in the global input_record_list.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(pipeline_inputs[[param]])
      if (validate_fn(candidate)) {
        message("Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message("Pre-recorded input for '", param, "' is invalid. Falling back to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 1: Prompt for the sequence of periods.
  message("📋 Let's define your experimental sequence.")
  period_sequence_input <- get_input_local("period_sequence",
                                           "Enter the sequence of periods (starting with 'acclimatation'), separated by commas (e.g., acclimatation, light_1, dark_1, light_2, dark_2): ",
                                           validate_fn = function(x) {
                                             periods <- trimws(unlist(strsplit(as.character(x), ",")))
                                             (length(periods) > 0) && ("acclimatation" %in% periods)
                                           },
                                           transform_fn = function(x) trimws(as.character(x)))
  # Split the sequence into a vector.
  periods <- trimws(unlist(strsplit(period_sequence_input, ",")))
  message("✔️ Sequence of periods successfully recorded: ", paste(periods, collapse = ", "))
  
  # Step 2: Prompt for period boundaries.
  message("🔧 Now, let's define the period boundaries manually.")
  message("ℹ️ Period boundaries represent the exact time codes (in seconds) where a switch occurs between consecutive periods.")
  message("📋 Your full sequence of periods is: ", paste(periods, collapse = ", "))
  message("💡 Enter all boundaries in one line, separated by commas (e.g., 1800, 1802, 1920, 1922, 2040, 2042).")
  
  boundaries_input <- get_input_local("period_boundaries",
                                      paste0("Enter the time codes (in seconds) for ", length(periods) - 1, " switches: "),
                                      validate_fn = function(x) {
                                        # Force x to be a string
                                        x_str <- as.character(x)
                                        boundaries <- as.numeric(trimws(unlist(strsplit(x_str, ","))))
                                        (length(boundaries) == length(periods) - 1) && all(!is.na(boundaries)) && all(boundaries > 0)
                                      },
                                      transform_fn = function(x) {
                                        x_str <- as.character(x)
                                        as.numeric(trimws(unlist(strsplit(x_str, ","))))
                                      },
                                      error_msg = paste("⚠️ Invalid input. Please ensure all time codes are positive numbers separated by commas and match the number of switches (", length(periods) - 1, ").")
  )
  period_boundaries <- boundaries_input
  message("✔️ Period boundaries successfully recorded: ", paste(period_boundaries, collapse = ", "))
  
  # Step 3: Convert boundaries to minutes.
  period_boundaries <- period_boundaries / 60
  
  # Step 4: Create associations between boundaries and period transitions.
  period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
  boundary_associations <- data.frame(
    boundary_time = period_boundaries,
    transition = period_transitions
  )
  
  # Step 5: Assign periods based on boundaries.
  message("🛠️ Assigning periods to data based on boundaries...")
  assign_period <- function(start_time_sec) {
    for (i in seq_along(period_boundaries)) {
      # Compare start time (in seconds) with boundary converted back to seconds.
      if (start_time_sec < period_boundaries[i] * 60) {
        return(periods[i])
      }
    }
    return(periods[length(periods)])
  }
  
  enriched_data <- enriched_data %>%
    mutate(period_with_numbers = sapply(start, assign_period))
  message("✔️ Periods successfully assigned to the data.")
  
  # Step 6: Create 'period_without_numbers' column by stripping numeric tags.
  message("🛠️ Creating the 'period_without_numbers' column...")
  enriched_data <- enriched_data %>%
    mutate(
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^light") ~ "light",
        str_detect(period_with_numbers, "^dark") ~ "dark",
        TRUE ~ period_with_numbers
      )
    )
  message("✔️ 'period_without_numbers' column successfully created.")
  
  # Step 7: Save enriched data and associations globally.
  assign("boundary_associations", boundary_associations, envir = .GlobalEnv)
  assign("data_with_periods_df", enriched_data, envir = .GlobalEnv)
  assign("period_boundaries", period_boundaries, envir = .GlobalEnv)
  message("🎉 Boundary associations created!")
  message("🎉 Period assignment completed successfully!")
  message("💾 The enriched data has been saved as 'data_with_periods_df'.")
  message("💾 The period boundaries have been saved as 'period_boundaries'.")
  message("💾 The boundary-to-transition associations have been saved as 'boundary_associations'.\n")
  
  return(enriched_data)
}
