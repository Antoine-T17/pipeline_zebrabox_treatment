extract_data <- function() {
  suppressWarnings(suppressPackageStartupMessages({
    library(readxl)
    library(readr)
  }))
  
  message("\n---\n---\n---\n")  
  
  # Step 1: Welcome message with instructions
  message("👋 Welcome to the Data Extraction Process!\n")
  message("This function assists you with:\n")
  message("📂 Locating and reading your raw data file.")
  message("🔢 Converting potential numeric columns for proper processing.")
  message("💾 Saving the extracted data in the global environment for future steps.\n")
  
  message("💡 Please ensure the following before proceeding:\n")
  message("- 📁 Your raw data file is saved in the 'inputs/raw_data' directory.")
  message("- 🗂️ The file can be in either '.csv' or '.xlsx' format.")
  message("- 🛑 All Excel files are closed to avoid errors during processing.\n")
  
  # Step 2: Prompt the user for the file name
  repeat {
    file_name <- readline(prompt = "❓ Enter the file name (including the extension, '.csv' or '.xlsx') saved in 'inputs/quantization_mode/light_dark_mode/raw_data': ")
    full_path <- file.path("inputs/quantization_mode/light_dark_mode/raw_data", file_name)
    
    if (!grepl("\\.(csv|xlsx)$", file_name, ignore.case = TRUE)) {
      message("\n❌ The file name must include either '.csv' or '.xlsx' as the extension. Please try again.")
      next
    }
    
    if (!file.exists(full_path)) {
      message("\n❌ The file '", full_path, "' does not exist. Please try again.")
      next
    } else {
      message("\n✔️ File detected: ", full_path)
      break
    }
  }
  
  # Step 3: Read the file based on its extension
  message("\n📂 Reading the file...")
  data <- tryCatch({
    if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
      message("🔍 Detected a CSV file. Reading the file... 📖")
      readr::read_csv(full_path, show_col_types = FALSE)
    } else if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
      message("🔍 Detected an Excel file. Reading the file... 📖")
      readxl::read_excel(full_path)
    } else {
      stop("❌ Unsupported file format. Please use '.csv' or '.xlsx'.")
    }
  }, error = function(e) {
    stop("❌ Error while reading the file: ", e$message)
  })
  
  message("\n✔️ Data successfully read from the file.")
  
  # Step 4: Convert potential numeric columns from "," to "." and then to numeric
  message("\n🔄 Processing potential numeric columns...")
  potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                              "smlct", "smldist", "smldur",
                              "larct", "lardur", "lardist",
                              "emptyct", "emptydur", 
                              "totaldist", "totaldur", "totalct")
  
  for (colname in intersect(names(data), potential_numeric_cols)) {
    data[[colname]] <- gsub(",", ".", as.character(data[[colname]]))
    data[[colname]] <- as.numeric(data[[colname]])
  }
  
  message("\n✔️ Numeric columns processed successfully.")
  
  # Step 5: Save the processed data in the global environment
  message("\n🎉 Data extraction completed successfully!")
  message("💾 The extracted data has been saved in the global environment as 'extracted_data_df'.\n")
  
  assign("extracted_data_df", data, envir = .GlobalEnv)
  return(data)
}
