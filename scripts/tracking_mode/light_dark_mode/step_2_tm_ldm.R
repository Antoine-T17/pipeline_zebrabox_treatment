extract_data <- function() {
  # Inline helper function for user input retrieval
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
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
  
  # Step 1: Display welcome message with instructions
  message("\n---\n")
  message("👋 Welcome to the Data Extraction Process!")
  message("📋 This function will help you:")
  message("   • Locate and read your raw data file(s) from the designated directory.")
  message("   • Convert numeric columns to proper formats for processing.")
  message("   • Save each extracted data frame in a list globally as 'extracted_data_df'.")
  message("ℹ️ Please ensure that:")
  message("   • Your raw data file(s) are in 'inputs/tracking_mode/light_dark_mode/raw_data'.")
  message("   • The file format is either '.csv' or '.xlsx'.")
  message("   • All Excel files are closed prior to extraction.\n")
  
  # Step 2: Prompt for the raw data file names (comma-separated)
  raw_data_files <- get_input_local("raw_data_file_name",
                                    "❓ Enter the raw data file name(s) (comma-separated, e.g., data1.csv, data2.xlsx): ",
                                    validate_fn = function(x) {
                                      files <- trimws(unlist(strsplit(x, ",")))
                                      length(files) >= 1 && all(grepl("\\.(csv|xlsx)$", files, ignore.case = TRUE))
                                    },
                                    transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
                                    error_msg = "❌ Invalid input. Ensure each file name ends with '.csv' or '.xlsx'.")
  
  raw_data_list <- list()
  
  # Step 3: Process each raw data file individually
  for (i in seq_along(raw_data_files)) {
    file_name <- raw_data_files[i]
    full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", file_name)
    
    # Validate file existence; if not found, prompt until a valid file is provided
    while (!file.exists(full_path)) {
      message("❌ The file '", full_path, "' does not exist. Please try again for file ", i, ".")
      file_name <- get_input_local(paste0("raw_data_file_name_", i),
                                   paste0("❓ Enter the raw data file name for plate ", i, " (including extension '.csv' or '.xlsx'): "),
                                   validate_fn = function(x) x != "" && grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE),
                                   transform_fn = function(x) trimws(x),
                                   error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
      full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", file_name)
    }
    message("✔️ File detected: ", full_path)
    
    # Read the file based on its extension using tryCatch
    data_i <- tryCatch({
      if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        message("🔍 Detected CSV format. Reading file...")
        read.csv2(full_path, header = TRUE, stringsAsFactors = FALSE)
      } else if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
        message("🔍 Detected Excel format. Reading file...")
        read_excel(full_path)
      } else {
        stop("❌ Unsupported file format. Use '.csv' or '.xlsx'.")
      }
    }, error = function(e) {
      stop("❌ Error while reading the file: ", e$message)
    })
    message("✔️ Data from file ", file_name, " successfully read.")
    
    # Process and convert potential numeric columns for this data
    message("🛠️ Converting potential numeric columns for file ", file_name, "...")
    potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                                "smlct", "smldist", "smldur",
                                "larct", "lardur", "lardist",
                                "emptyct", "emptydur", 
                                "totaldist", "totaldur", "totalct")
    for (colname in intersect(names(data_i), potential_numeric_cols)) {
      data_i[[colname]] <- as.numeric(gsub(",", ".", as.character(data_i[[colname]])))
    }
    message("✔️ Numeric columns processed for file ", file_name, ".")
    
    # Link this raw data file with the corresponding plate by assigning a plate_id
    data_i$plate_id <- paste0("plate_", i)
    
    # Save this processed data into the list
    raw_data_list[[i]] <- data_i
  }
  
  # Finalize: Save the list of raw data frames globally and return it
  message("🎉 Data extraction completed!")
  message("💾 Extracted data is saved globally as 'extracted_data_df'.\n")
  assign("extracted_data_df", raw_data_list, envir = .GlobalEnv)
  return(raw_data_list)
}
