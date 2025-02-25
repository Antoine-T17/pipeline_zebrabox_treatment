generate_plate_plan <- function(plan_dir = "inputs/tracking_mode/light_dark_mode/plate_plan") {
  # Helper function to get input from pipeline_inputs or interactively
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
  
  # Step 1: Ensure the plate plan directory exists
  if (!dir.exists(plan_dir)) {
    dir.create(plan_dir, recursive = TRUE)
    message("✔️ Directory created: ", plan_dir)
  }
  
  # Step 2: Display welcome message with instructions
  message("\n---\n")
  message("👋 Welcome to the Multi-Plate Plan Generator!")
  message("📋 This function will help you:")
  message("   • Create new plate plan(s) OR")
  message("   • Load one or more existing plate plan files (CSV or Excel).")
  message("   • Merge them into a combined plate plan with plate identifiers.\n")
  
  combined_plate_plan <- NULL
  
  # Step 3: Ask the user whether to create a new plate plan or load existing ones
  create_plan <- get_input_local("create_plate_plan",
                                 "❓ Do you want to create a new plate plan? (yes/no): ",
                                 validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                 transform_fn = function(x) tolower(trimws(x)),
                                 error_msg = "❌ Please enter 'yes' or 'no'.")
  
  if (create_plan %in% c("yes", "y")) {
    # ---- New Plate Plan Creation (for one or more plates) ----
    # Prompt for the number of plates to create
    plate_count <- get_input_local("plate_count",
                                   "❓ Enter the number of plates to generate: ",
                                   validate_fn = function(x) !is.na(x) && as.integer(x) > 0,
                                   transform_fn = function(x) as.integer(trimws(x)),
                                   error_msg = "❌ Please enter a positive integer.")
    message("✔️ Number of plates to generate: ", plate_count)
    
    # Get common parameters that will be used for all plates.
    plate_type <- get_input_local("plate_type",
                                  "❓ Enter the type of plate (24, 48, or 96 wells): ",
                                  validate_fn = function(x) !is.na(x) && as.integer(x) %in% c(24, 48, 96),
                                  transform_fn = function(x) as.integer(trimws(x)),
                                  error_msg = "❌ Invalid plate type. Please enter 24, 48, or 96.")
    message("✔️ Plate type selected: ", plate_type, " wells.")
    
    total_wells <- plate_type
    rows <- LETTERS[1:(ifelse(plate_type == 24, 4, ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:(ifelse(plate_type == 24, 6, ifelse(plate_type == 48, 8, 12)))
    
    conditions_number <- get_input_local("conditions_number",
                                         "❓ Enter the number of conditions: ",
                                         validate_fn = function(x) !is.na(x) && as.integer(x) > 0,
                                         transform_fn = function(x) as.integer(trimws(x)),
                                         error_msg = "❌ Please enter a positive integer.")
    message("✔️ Number of conditions: ", conditions_number)
    
    conditions_name_input <- get_input_local("conditions_name",
                                             sprintf("❓ Enter the names of your %d conditions (comma-separated): ", conditions_number),
                                             validate_fn = function(x) {
                                               conds <- trimws(unlist(strsplit(x, ",")))
                                               length(conds) == conditions_number && length(unique(conds)) == length(conds)
                                             },
                                             transform_fn = function(x) x,
                                             error_msg = sprintf("❌ Conditions must be unique and exactly %d in number.", conditions_number))
    conditions <- trimws(unlist(strsplit(conditions_name_input, ",")))
    message("✔️ Conditions recorded: ", paste(conditions, collapse = ", "))
    
    replicates_number <- get_input_local("replicates_number",
                                         "❓ Enter the number of replicates per condition: ",
                                         validate_fn = function(x) !is.na(x) && as.integer(x) > 0,
                                         transform_fn = function(x) as.integer(trimws(x)),
                                         error_msg = "❌ Please enter a positive integer.")
    message("✔️ Number of replicates: ", replicates_number)
    
    units_per_replicate <- get_input_local("units_per_replicate",
                                           "❓ Enter the number of units per replicate: ",
                                           validate_fn = function(x) {
                                             x <- as.integer(trimws(x))
                                             !is.na(x) && x > 0 && ((conditions_number * replicates_number * x) <= total_wells)
                                           },
                                           transform_fn = function(x) as.integer(trimws(x)),
                                           error_msg = "❌ Invalid number. Total units exceed available wells.")
    total_units <- conditions_number * replicates_number * units_per_replicate
    message("✔️ Total units: ", total_units, " (within ", total_wells, " wells).")
    
    seed_value <- get_input_local("seed_value",
                                  "❓ Enter a seed value for randomization: ",
                                  validate_fn = function(x) !is.na(x),
                                  transform_fn = function(x) as.integer(trimws(x)),
                                  error_msg = "❌ Invalid seed value. Please enter an integer.")
    set.seed(seed_value)
    message("✔️ Seed value set to: ", seed_value)
    
    # For file saving, get a base name to use for each plate
    base_plate_name_csv <- get_input_local("base_plate_plan_name_csv",
                                           "❓ Enter a base file name for the plate plan CSV (e.g., plate_): ",
                                           validate_fn = function(x) x != "",
                                           transform_fn = function(x) trimws(x),
                                           error_msg = "❌ Invalid base file name.")
    
    base_plate_name_xlsx <- get_input_local("base_plate_plan_name_xlsx",
                                            "❓ Enter a base file name for the plate plan Excel (e.g., plate_): ",
                                            validate_fn = function(x) x != "",
                                            transform_fn = function(x) trimws(x),
                                            error_msg = "❌ Invalid base file name.")
    
    # Loop to generate plate_count new plate plans
    for (i in 1:plate_count) {
      message("\n--- Generating Plate Plan for Plate ", i, " ---")
      # Generate plate assignments
      wells <- with(expand.grid(Row = rows, Column = cols),
                    paste0(Row, sprintf("%02d", Column)))
      assignments <- unlist(lapply(conditions, function(cond) {
        rep(paste0(cond, "_", seq_len(replicates_number)), each = units_per_replicate)
      }))
      assignments <- c(assignments, rep("X", total_wells - length(assignments)))
      assignments <- sample(assignments)
      
      plate_matrix <- matrix(assignments, nrow = length(rows), ncol = length(cols),
                             byrow = TRUE, dimnames = list(rows, cols))
      plate_plan <- data.frame(animal = wells, condition = as.vector(plate_matrix), stringsAsFactors = FALSE)
      message("🎉 Plate plan for Plate ", i, " created successfully!")
      
      # File names for saving this plate plan
      plate_name_csv <- paste0(base_plate_name_csv, i, ".csv")
      plate_name_xlsx <- paste0(base_plate_name_xlsx, i, ".xlsx")
      
      csv_path <- file.path(plan_dir, plate_name_csv)
      write.csv2(plate_plan, file = csv_path, row.names = FALSE)
      message("💾 Plate plan saved as CSV: ", csv_path)
      
      xlsx_path <- file.path(plan_dir, plate_name_xlsx)
      openxlsx::write.xlsx(plate_plan, file = xlsx_path, rowNames = FALSE)
      message("💾 Plate plan saved as Excel file: ", xlsx_path)
      
      # Tag the plate plan with a unique plate identifier
      plate_plan$plate_id <- paste0("plate_", i)
      
      # Combine into the overall plate plan
      if (is.null(combined_plate_plan)) {
        combined_plate_plan <- plate_plan
      } else {
        combined_plate_plan <- rbind(combined_plate_plan, plate_plan)
      }
    }
    
  } else {
    # ---- Loading Existing Plate Plans (supports multiple files) ----
    plate_plan_name <- get_input_local("plate_plan_name",
                                        "❓ Enter the existing plate plan file name(s) (comma-separated, e.g., plate_1.xlsx, plate_2.csv): ",
                                        validate_fn = function(x) {
                                          names <- trimws(unlist(strsplit(x, ",")))
                                          all(grepl("\\.(csv|xlsx)$", names, ignore.case = TRUE)) &&
                                            all(file.exists(file.path(plan_dir, names)))
                                        },
                                        transform_fn = function(x) trimws(unlist(strsplit(x, ","))),
                                        error_msg = "❌ Invalid input. Ensure each file name ends with .csv or .xlsx and exists in the directory.")
    
    for (file_name in plate_plan_name) {
      file_path <- file.path(plan_dir, file_name)
      file_extension <- tolower(tools::file_ext(file_name))
      message("🔍 Reading plate plan from file: ", file_path)
      
      plate_plan <- tryCatch({
        if (file_extension == "csv") {
          read.csv2(file_path, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
        } else if (file_extension == "xlsx") {
          readxl::read_excel(file_path, sheet = 1, col_names = TRUE)
        } else {
          stop("❌ Unsupported file format.")
        }
      }, error = function(e) {
        stop("❌ Error loading the plate plan file: ", file_name, " - ", e$message)
      })
      
      if (!all(c("animal", "condition") %in% colnames(plate_plan))) {
        stop("❌ Plate plan in file ", file_name, " is missing required columns: 'animal' and 'condition'.")
      }
      # Add a plate identifier derived from the file name (without extension)
      plate_plan$plate_id <- tools::file_path_sans_ext(file_name)
      message("✔️ Plate plan from ", file_name, " loaded with plate_id: ", plate_plan$plate_id[1])
      
      # Merge into the combined plate plan
      if (is.null(combined_plate_plan)) {
        combined_plate_plan <- plate_plan
      } else {
        combined_plate_plan <- rbind(combined_plate_plan, plate_plan)
      }
    }
  }
  
  # Finalize: Save and return the combined plate plan
  message("🎉 Plate plan generation completed!")
  message("💾 Combined plate plan is now available globally as 'plate_plan_df'.\n")
  assign("plate_plan_df", combined_plate_plan, envir = .GlobalEnv)
  return(combined_plate_plan)
}
