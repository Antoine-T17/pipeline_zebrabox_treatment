# -----------------------------------------------------------
# File: extract_data.R
# -----------------------------------------------------------
# Harmonized version of the extract_data function for vibration_mode.
# This function locates, reads, and processes raw experimental data (supporting CSV and Excel).
# It also standardizes numeric column conversion and saves the data globally as 'extracted_data_df'.
# -----------------------------------------------------------

extract_data <- function() {
  
  message("\n---\n")
  message("👋 Welcome to the Data Extraction Process!\n")
  message("📋 This function will help you:")
  message("   • Locate and read your raw data file from the designated directory.")
  message("   • Convert numeric columns to proper formats for processing.")
  message("   • Save the extracted data globally as 'extracted_data_df'.\n")
  message("ℹ️ Please ensure that:")
  message("   • Your raw data file is in 'inputs/tracking_mode/vibration_mode/raw_data'.")
  message("   • The file format is either '.csv' or '.xlsx'.")
  message("   • All Excel files are closed prior to extraction.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Unified helper to retrieve inputs.
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
  
  # Prompt for the raw data file name.
  raw_data_file_name <- get_input_local("raw_data_file_name",
                                        "❓ Enter the raw data file name (including extension '.csv' or '.xlsx'): ",
                                        validate_fn = function(x) x != "" && grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE),
                                        transform_fn = function(x) trimws(x),
                                        error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
  
  full_path <- file.path("inputs/tracking_mode/vibration_mode/raw_data", raw_data_file_name)
  while (!file.exists(full_path)) {
    message("❌ The file '", full_path, "' does not exist. Please try again.")
    raw_data_file_name <- get_input_local("raw_data_file_name",
                                          "❓ Enter the raw data file name (including extension '.csv' or '.xlsx'): ",
                                          validate_fn = function(x) x != "" && grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE),
                                          transform_fn = function(x) trimws(x),
                                          error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
    full_path <- file.path("inputs/tracking_mode/vibration_mode/raw_data", raw_data_file_name)
  }
  
  message("✔️ File detected: ", full_path)
  
  # Read the file based on its extension.
  data <- tryCatch({
    if (grepl("\\.csv$", raw_data_file_name, ignore.case = TRUE)) {
      message("🔍 Detected CSV format. Reading file...")
      readr::read_csv(full_path, show_col_types = FALSE)
    } else if (grepl("\\.xlsx$", raw_data_file_name, ignore.case = TRUE)) {
      message("🔍 Detected Excel format. Reading file...")
      readxl::read_excel(full_path)
    } else {
      stop("❌ Unsupported file format. Use '.csv' or '.xlsx'.")
    }
  }, error = function(e) {
    stop("❌ Error while reading the file: ", e$message)
  })
  
  message("✔️ Data successfully read.")
  
  # Process potential numeric columns.
  message("🛠️ Converting potential numeric columns...")
  potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                              "smlct", "smldist", "smldur",
                              "larct", "lardur", "lardist",
                              "emptyct", "emptydur", 
                              "totaldist", "totaldur", "totalct")
  
  for (colname in intersect(names(data), potential_numeric_cols)) {
    data[[colname]] <- as.numeric(gsub(",", ".", as.character(data[[colname]])))
  }
  
  message("✔️ Numeric columns processed.")
  message("🎉 Data extraction completed!")
  message("💾 Extracted data is saved globally as 'extracted_data_df'.\n")
  
  assign("extracted_data_df", data, envir = .GlobalEnv)
  return(data)
}
