# -----------------------------------------------------------
# extract_data.R
# -----------------------------------------------------------
# This function assists in extracting raw data.
# It:
#   • Displays instructions for data extraction.
#   • Prompts the user (or uses pre‐recorded inputs) to specify
#     the raw data file name (stored as "raw_data_file_name").
#   • Reads the file (supporting both '.csv' and '.xlsx').
#   • Processes potential numeric columns (converting "," to ".")
#   • Saves the extracted data globally as "extracted_data_df"
#   • Records the user input in the global list "input_record_list"
#      (to be merged later into a data frame).
# -----------------------------------------------------------

extract_data <- function() {
  message("\n---\n---\n---\n")  
  message("\n👋 Welcome to the Data Extraction Process!\n")
  message("This function assists you with:")
  message("  📂 Locating and reading your raw data file.")
  message("  🔢 Converting potential numeric columns for proper processing.")
  message("  💾 Saving the extracted data in the global environment for future steps.\n")
  
  message("💡 Please ensure the following before proceeding:")
  message("  - 📁 Your raw data file is saved in the 'inputs/tracking_mode/light_dark_mode/raw_data' directory.")
  message("  - 🗂️ The file can be in either '.csv' or '.xlsx' format.")
  message("  - 🛑 All Excel files are closed to avoid errors during processing.\n")
  
  # Load pre-recorded inputs for data extraction.
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
  
  # Local helper to get an input for extraction and record it globally.
  # The parameter here is "raw_data_file_name".
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
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
  
  # Step 2: Prompt for the raw data file name.
  raw_data_file_name <- get_input_local("raw_data_file_name",
                                        "📄 Enter the raw data file name (including the extension, '.csv' or '.xlsx'): ",
                                        validate_fn = function(x) {
                                          if (x == "") return(FALSE)
                                          if (!grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE)) return(FALSE)
                                          TRUE
                                        },
                                        transform_fn = function(x) trimws(x),
                                        error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
  
  # Construct the full path.
  full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", raw_data_file_name)
  
  # If the file does not exist, keep prompting until a valid file is provided.
  while (!file.exists(full_path)) {
    message("❌ The file '", full_path, "' does not exist. Please try again.")
    raw_data_file_name <- get_input_local("raw_data_file_name",
                                          "📄 Enter the raw data file name (including the extension, '.csv' or '.xlsx'): ",
                                          validate_fn = function(x) {
                                            if (x == "") return(FALSE)
                                            if (!grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE)) return(FALSE)
                                            TRUE
                                          },
                                          transform_fn = function(x) trimws(x),
                                          error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'.")
    full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", raw_data_file_name)
  }
  
  message("✔️ File detected: ", full_path)
  
  # Step 3: Read the file based on its extension.
  data <- tryCatch({
    if (grepl("\\.csv$", raw_data_file_name, ignore.case = TRUE)) {
      message("🔍 Detected a CSV file. Reading the file... 📖")
      readr::read_csv(full_path, show_col_types = FALSE)
    } else if (grepl("\\.xlsx$", raw_data_file_name, ignore.case = TRUE)) {
      message("🔍 Detected an Excel file. Reading the file... 📖")
      readxl::read_excel(full_path)
    } else {
      stop("❌ Unsupported file format. Please use '.csv' or '.xlsx'.")
    }
  }, error = function(e) {
    stop("❌ Error while reading the file: ", e$message)
  })
  
  message("✔️ Data successfully read from the file.")
  
  # Step 4: Process potential numeric columns.
  message("🔄 Processing potential numeric columns...")
  potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                              "smlct", "smldist", "smldur",
                              "larct", "lardur", "lardist",
                              "emptyct", "emptydur", 
                              "totaldist", "totaldur", "totalct")
  
  for (colname in intersect(names(data), potential_numeric_cols)) {
    data[[colname]] <- gsub(",", ".", as.character(data[[colname]]))
    data[[colname]] <- as.numeric(data[[colname]])
  }
  
  message("✔️ Numeric columns processed successfully.")
  
  # Step 5: Save the processed data globally.
  message("🎉 Data extraction completed successfully!")
  message("💾 The extracted data has been saved globally as 'extracted_data_df'.\n")
  
  assign("extracted_data_df", data, envir = .GlobalEnv)
  return(data)
}
