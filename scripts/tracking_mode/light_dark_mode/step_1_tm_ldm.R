# -----------------------------------------------------------
# generate_plate_plan.R
# -----------------------------------------------------------
# This script defines the generate_plate_plan function.
#
# The function assists in creating a new plate plan or loading an existing one.
# It uses a local helper (get_input_local) to retrieve inputs (from a pre-recorded
# file or interactively) and records every input directly into the global list
# 'input_record_list'. (Ensure that input_record_list is defined globally.)
#
# When called from the main pipeline (run_full_pipeline), the generated plate plan
# is saved globally as 'plate_plan_df'.
# -----------------------------------------------------------

generate_plate_plan <- function(plan_dir = "inputs/tracking_mode/light_dark_mode/plate_plan") {
  
  # Ensure the plate plan directory exists.
  if (!dir.exists(plan_dir)) {
    dir.create(plan_dir, recursive = TRUE)
    message("Directory created: ", plan_dir)
  }
  
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Multi-Well Plate Plan Generator!")
  message("This function assists you with:")
  message("  - Creating a new plate plan or loading an existing one.")
  message("  - Randomizing well assignments if a new plan is created.")
  message("  - Supporting both '.csv' and '.xlsx' file formats.")
  message("  - Saving the plate plan for later use.\n")
  
  # Load pre-recorded inputs for the plate plan.
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
  
  # --- Begin Plate Plan Setup ---
  
  # Ask whether to create a new plate plan.
  create_plan <- get_input_local("create_plate_plan",
                                 "❓ Do you want to create a new plate plan? (yes/no): ",
                                 validate_fn = function(x) x %in% c("yes", "y", "no", "n"),
                                 transform_fn = function(x) tolower(trimws(x)),
                                 error_msg = "⚠️ Invalid response. Please enter 'yes' or 'no'.")
  
  save_file_base <- NULL
  plate_plan <- NULL
  plate_plan_name <- NULL  # This will include the file extension.
  
  if (create_plan %in% c("yes", "y")) {
    # New plate plan creation branch.
    plate_type <- get_input_local("plate_type",
                                  "🔢 Enter the type of plate (24, 48, or 96 wells): ",
                                  validate_fn = function(x) !is.na(x) && x %in% c(24, 48, 96),
                                  transform_fn = function(x) as.integer(x),
                                  error_msg = "⚠️ Invalid plate type. Please enter 24, 48, or 96.")
    message("✔️ Plate type selected: ", plate_type, " wells.")
    
    total_wells <- plate_type
    rows <- LETTERS[1:(ifelse(plate_type == 24, 4, ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:(ifelse(plate_type == 24, 6, ifelse(plate_type == 48, 8, 12)))
    
    conditions_number <- get_input_local("conditions_number",
                                         "🔢 Enter the number of conditions (e.g., 3, 5, 10): ",
                                         validate_fn = function(x) !is.na(x) && x > 0,
                                         transform_fn = function(x) as.integer(x),
                                         error_msg = "⚠️ Invalid number. Please enter a positive integer.")
    message("✔️ Number of conditions: ", conditions_number)
    
    conditions_name_input <- get_input_local("conditions_name",
                                             paste0("📄 Enter the names of your conditions (", conditions_number, 
                                                    " expected, separated by commas): "),
                                             validate_fn = function(x) {
                                               conds <- trimws(unlist(strsplit(x, ",")))
                                               length(conds) == conditions_number && length(unique(conds)) == length(conds)
                                             },
                                             transform_fn = function(x) x,
                                             error_msg = paste("❌ The conditions must be unique and exactly", 
                                                               conditions_number, "in number.")
    )
    conditions <- trimws(unlist(strsplit(conditions_name_input, ",")))
    message("✔️ Conditions recorded: ", paste(conditions, collapse = ", "))
    
    replicates_number <- get_input_local("replicates_number",
                                         "🔢 Enter the number of replicates per condition (e.g., 3, 5): ",
                                         validate_fn = function(x) !is.na(x) && x > 0,
                                         transform_fn = function(x) as.integer(x),
                                         error_msg = "⚠️ Invalid number. Please enter a positive integer.")
    message("✔️ Number of replicates: ", replicates_number)
    
    units_per_replicate <- get_input_local("units_per_replicate",
                                           "🔢 Enter the number of units per replicate (e.g., 10): ",
                                           validate_fn = function(x) {
                                             x <- as.integer(x)
                                             !is.na(x) && x > 0 && ((conditions_number * replicates_number * x) <= total_wells)
                                           },
                                           transform_fn = function(x) as.integer(x),
                                           error_msg = "⚠️ Invalid number. Total units exceed available wells."
    )
    total_units <- conditions_number * replicates_number * units_per_replicate
    message("✔️ Total units: ", total_units, " (within ", total_wells, " wells).")
    
    seed_value <- get_input_local("seed_value",
                                  "🎲 Enter a seed value for randomization (e.g., between 1 and 1000): ",
                                  validate_fn = function(x) !is.na(x),
                                  transform_fn = function(x) as.integer(x),
                                  error_msg = "⚠️ Invalid seed value. Enter a valid integer.")
    set.seed(seed_value)
    message("✔️ Seed value: ", seed_value)
    message("💡 Remember this seed for reproducibility.")
    
    plate_plan_name <- get_input_local("plate_plan_name",
                                       "💾 Enter a file name for the new plate plan (with .csv or .xlsx extension): ",
                                       validate_fn = function(x) {
                                         if (x == "") return(FALSE)
                                         grepl("\\.csv$|\\.xlsx$", x, ignore.case = TRUE)
                                       },
                                       transform_fn = function(x) trimws(x),
                                       error_msg = "⚠️ Invalid file name. Must end with .csv or .xlsx.")
    save_file_base <- sub("\\.csv$|\\.xlsx$", "", plate_plan_name, ignore.case = TRUE)
    file_extension <- tolower(tools::file_ext(plate_plan_name))
    
    wells <- with(expand.grid(Row = rows, Column = cols), 
                  paste0(Row, sprintf("%02d", Column)))
    
    assignments <- unlist(lapply(conditions, function(cond) {
      rep(paste0(cond, "_", seq_len(replicates_number)), each = units_per_replicate)
    }))
    assignments <- c(assignments, rep("X", total_wells - length(assignments)))
    assignments <- sample(assignments)
    
    plate_matrix <- matrix(assignments, nrow = length(rows), ncol = length(cols),
                           byrow = TRUE, dimnames = list(rows, cols))
    
    plate_plan <- data.frame(
      animal = wells,
      condition = as.vector(plate_matrix),
      stringsAsFactors = FALSE
    )
    
  } else {
    # Existing plate plan branch.
    plate_plan_name <- get_input_local("plate_plan_name",
                                       "📄 Enter the name of your existing plate plan file (with .csv or .xlsx extension): ",
                                       validate_fn = function(x) {
                                         if (x == "") return(FALSE)
                                         if (!grepl("\\.csv$|\\.xlsx$", x, ignore.case = TRUE)) return(FALSE)
                                         selected_plan_path <- file.path(plan_dir, x)
                                         file.exists(selected_plan_path)
                                       },
                                       transform_fn = function(x) trimws(x),
                                       error_msg = "❌ Invalid file name or file does not exist in the directory."
    )
    save_file_base <- sub("\\.csv$|\\.xlsx$", "", plate_plan_name, ignore.case = TRUE)
    file_extension <- tolower(tools::file_ext(plate_plan_name))
    selected_plan_path <- file.path(plan_dir, plate_plan_name)
    
    plate_plan <- tryCatch({
      if (file_extension == "csv") {
        message("📖 Reading plate plan from CSV...")
        read.csv2(selected_plan_path, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
      } else if (file_extension == "xlsx") {
        message("📖 Reading plate plan from Excel...")
        readxl::read_excel(selected_plan_path, sheet = 1, col_names = TRUE)
      } else {
        stop("❌ Unsupported file format. Use .csv or .xlsx.")
      }
    }, error = function(e) {
      stop("❌ Error loading the plate plan file: ", e$message)
    })
    
    required_columns <- c("animal", "condition")
    missing_columns <- setdiff(required_columns, colnames(plate_plan))
    if (length(missing_columns) > 0) {
      stop("❌ Missing required columns in the plate plan: ", paste(missing_columns, collapse = ", "))
    }
    message("✔️ Plate plan file validated successfully.")
  }
  
  # Save the plate plan using the chosen file extension.
  if (file_extension == "csv") {
    csv_path <- file.path(plan_dir, plate_plan_name)
    write.csv2(plate_plan, file = csv_path, row.names = FALSE)
    message("✔️ Plate plan saved as CSV: ", csv_path)
  } else if (file_extension == "xlsx") {
    xlsx_path <- file.path(plan_dir, plate_plan_name)
    openxlsx::write.xlsx(plate_plan, file = xlsx_path, rowNames = FALSE)
    message("✔️ Plate plan saved as Excel file: ", xlsx_path)
  } else {
    stop("Unsupported file extension for plate_plan_name.")
  }
  
  message("🎉 Plate plan generation completed!")
  message("💾 Plate plan is saved globally as 'plate_plan_df'.")
  
  # Save the plate plan in the global environment.
  assign("plate_plan_df", plate_plan, envir = .GlobalEnv)
  
  return(plate_plan)
}
