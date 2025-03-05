# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light dark mode
# Function: extract_data
# Purpose: Locates, reads, and processes raw experimental data (CSV or Excel).
#          Supports one or more raw data files (separated by ';').
#          It standardizes numeric column conversion and saves the data globally as 'extracted_data_df_list'.
#          Additionally, if a global plate plan list exists and contains multiple plates,
#          the function forces manual input if only one raw data file was provided,
#          ensuring that the number of raw data files matches the number of plate plan files.
# -----------------------------------------------------------
extract_data <- function() {
  # Step 1: Display the welcome message with bullet points and instructions
  message("\n---\n")
  message("👋 Welcome to the Data Extraction Process!")
  message("📋 This function will help you:")
  message("   • Locate and read your raw data file(s) from the designated directory.")
  message("   • Convert numeric columns to proper formats for processing.")
  message("   • Save the extracted data globally as 'extracted_data_df_list'.")
  message("ℹ️ Please ensure that:")
  message("   • Your raw data file(s) are in 'inputs/tracking_mode/light_dark_mode/raw_data'.")
  message("   • The file format is either '.csv' or '.xlsx'.")
  message("   • All Excel files are closed prior to extraction.\n")
  
  # Step 2: Retrieve pre-recorded pipeline inputs from the global environment
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Step 3: Define a helper function to obtain and validate user inputs
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
  
  # Step 4: Prompt for the raw data file name(s); allow multiple names separated by ';'
  raw_data_files_input <- get_input_local("raw_data_file_name",
                                          "❓ Enter the raw data file name(s) (including extension '.csv' or '.xlsx'), separated by ';': ",
                                          validate_fn = function(x) {
                                            files <- trimws(unlist(strsplit(x, ";")))
                                            all(sapply(files, function(f) f != "" && grepl("\\.(csv|xlsx)$", f, ignore.case = TRUE)))
                                          },
                                          transform_fn = function(x) trimws(x),
                                          error_msg = "❌ Invalid file name(s). Ensure each ends with '.csv' or '.xlsx'.")
  file_names <- trimws(unlist(strsplit(raw_data_files_input, ";")))
  
  # --- NEW LOGIC: Check against number of plate plan files (if available) ---
  if (exists("plate_plan_df_list", envir = .GlobalEnv)) {
    plate_plans <- get("plate_plan_df_list", envir = .GlobalEnv)
    n_plate_plans <- length(plate_plans)
    n_raw_files <- length(file_names)
    
    # If there are multiple plate plans but only one raw data file, force manual re-entry.
    if (n_plate_plans > 1 && n_raw_files == 1) {
      message("⚠️ Multiple plate plan files detected (", n_plate_plans, 
              ") but only one raw data file provided. Switching to manual prompt mode.")
      raw_data_files_input <- readline(prompt = "❓ Please re-enter the raw data file names (separated by ';') to match the number of plate plan files: ")
      file_names <- trimws(unlist(strsplit(raw_data_files_input, ";")))
      n_raw_files <- length(file_names)
    }
    # Ensure the number of raw data files matches the number of plate plan files.
    if (n_raw_files != n_plate_plans) {
      stop("❌ The number of raw data files (", n_raw_files, 
           ") must match the number of plate plan files (", n_plate_plans, ").")
    }
  }
  
  extracted_data_list <- list()
  for (f in file_names) {
    full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", f)
    while (!file.exists(full_path)) {
      message("❌ The file '", full_path, "' does not exist. Please try again.")
      f <- get_input_local("raw_data_file_name",
                           "❓ Enter the raw data file name (including extension '.csv' or '.xlsx'): ",
                           validate_fn = function(x) x != "" && grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE),
                           transform_fn = function(x) trimws(x),
                           error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
      full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", f)
    }
    message("✔️ File detected: ", full_path)
    
    # Step 6: Read the file based on its extension using a tryCatch block
    data <- tryCatch({
      if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        message("🔍 Detected CSV format. Reading file...")
        readr::read_csv(full_path, show_col_types = FALSE)
      } else if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {
        message("🔍 Detected Excel format. Reading file...")
        readxl::read_excel(full_path)
      } else {
        stop("❌ Unsupported file format. Use '.csv' or '.xlsx'.")
      }
    }, error = function(e) {
      stop("❌ Error while reading the file: ", e$message)
    })
    message("✔️ Data successfully read from ", f, ".")
    
    # Step 7: Process and convert potential numeric columns
    message("🛠️ Converting potential numeric columns in ", f, "...")
    potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                                "smlct", "smldist", "smldur",
                                "larct", "lardur", "lardist",
                                "emptyct", "emptydur", 
                                "totaldist", "totaldur", "totalct")
    for (colname in intersect(names(data), potential_numeric_cols)) {
      data[[colname]] <- as.numeric(gsub(",", ".", as.character(data[[colname]])))
    }
    message("✔️ Numeric columns processed for ", f, ".")
    extracted_data_list[[length(extracted_data_list) + 1]] <- data
  }
  
  # Step 8: Finalize extraction and assign data globally
  message("🎉 Data extraction completed!")
  message("💾 Extracted data saved globally as 'extracted_data_df_list'.\n")
  assign("extracted_data_df_list", extracted_data_list, envir = .GlobalEnv)
  return(extracted_data_list)
}
