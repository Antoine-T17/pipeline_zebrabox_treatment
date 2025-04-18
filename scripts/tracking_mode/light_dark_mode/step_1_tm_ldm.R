# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light dark mode
# Function: generate_plate_plan
# Purpose: Creates or loads one or more multi-well plate plans for light/dark mode experiments.
#          If in creation mode, a new plate plan is generated (for one plate; see note).
#          In load mode, multiple plate plan file names may be provided separated by ";".
#          The resulting plate plan(s) are stored globally as 'plate_plan_df_list'.
# -----------------------------------------------------------
generate_plate_plan <- function(plan_dir = "inputs/tracking_mode/light_dark_mode/plate_plan") {
  # Step 1: Ensure the plate plan directory exists
  if (!dir.exists(plan_dir)) {
    dir.create(plan_dir, recursive = TRUE)
    message("✔️ Directory created: ", plan_dir)
  }
  
  # Step 2: Display the welcome message
  message("\n---\n")
  message("👋 Welcome to the Plate Plan Generator!")
  message("📋 This function will help you:")
  message("   • Create a new plate plan or load existing one(s).")
  message("   • Randomize well assignments for new plans (with or without border wells).")
  message("   • Work with both '.csv' and '.xlsx' file formats.")
  message("   • Save the plate plan(s) for later use.\n")
  
  # Step 3: Retrieve pre-recorded pipeline inputs from the global environment
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Step 4: Helper function for obtaining and validating user inputs
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
  
  # Step 5: Ask whether the user wants to create a new plan or load an existing plan
  create_plan <- get_input_local("create_plate_plan",
                                 "❓ Do you want to create a new plate plan? (yes/no): ",
                                 validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                 transform_fn = function(x) tolower(trimws(x)),
                                 error_msg = "❌ Please enter 'yes' or 'no'.")
  plate_plan_list <- list()  # This list will hold one or more plate plan dataframes
  
  if (create_plan %in% c("yes", "y")) {
    # Step 6.1: Get the plate type (24, 48, or 96 wells)
    plate_type <- get_input_local("plate_type",
                                  "❓ Enter the type of plate (24, 48, or 96 wells): ",
                                  validate_fn = function(x) !is.na(x) && x %in% c(24, 48, 96),
                                  transform_fn = function(x) as.integer(x),
                                  error_msg = "❌ Invalid plate type. Please enter 24, 48, or 96.")
    message("✔️ Plate type selected: ", plate_type, " wells.")
    
    total_wells <- plate_type
    rows <- LETTERS[1:(ifelse(plate_type == 24, 4, ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:(ifelse(plate_type == 24, 6, ifelse(plate_type == 48, 8, 12)))
    
    # Step 6.2: Ask for the number of conditions and their names
    conditions_number <- get_input_local("conditions_number",
                                         "❓ Enter the number of conditions: ",
                                         validate_fn = function(x) !is.na(x) && x > 0,
                                         transform_fn = function(x) as.integer(x),
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
    
    # Step 6.3: Ask for the number of replicates per condition and number of units per replicate
    replicates_number <- get_input_local("replicates_number",
                                         "❓ Enter the number of replicates per condition: ",
                                         validate_fn = function(x) !is.na(x) && x > 0,
                                         transform_fn = function(x) as.integer(x),
                                         error_msg = "❌ Please enter a positive integer.")
    message("✔️ Number of replicates: ", replicates_number)
    
    units_per_replicate <- get_input_local("units_per_replicate",
                                           "❓ Enter the number of units per replicate: ",
                                           validate_fn = function(x) {
                                             x <- as.integer(x)
                                             !is.na(x) && x > 0
                                           },
                                           transform_fn = function(x) as.integer(x),
                                           error_msg = "❌ Please enter a valid positive integer.")
    total_units <- conditions_number * replicates_number * units_per_replicate
    message("✔️ Total units to assign: ", total_units)
    
    # Step 6.4: Ask for the number of plates to generate
    plates_number <- get_input_local("plate_number",
                                     "❓ Enter the number of plates to generate: ",
                                     validate_fn = function(x) {
                                       x <- as.integer(x)
                                       !is.na(x) && x > 0
                                     },
                                     transform_fn = function(x) as.integer(x),
                                     error_msg = "❌ Please enter a positive integer for the number of plates.")
    message("✔️ Number of plates: ", plates_number)
    
    # Step 6.5: Ask whether to include border wells in the randomization
    keep_border <- get_input_local("keep_border_wells",
                                   "❓ Do you want to include border wells in the randomization? (yes/no): ",
                                   validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                   transform_fn = function(x) tolower(trimws(x)),
                                   error_msg = "❌ Please enter 'yes' or 'no'.")
    
    # Create the standard grid for each plate (without suffixes)
    wells_template <- with(expand.grid(Row = rows, Column = cols),
                           paste0(Row, sprintf("%02d", Column)))
    # Identify indices of border wells
    border_indices <- which(outer(seq_along(rows), seq_along(cols), 
                                  function(i, j) { i == 1 | i == length(rows) | j == 1 | j == length(cols) }))
    if(tolower(keep_border) %in% c("yes", "y")) {
      available_per_plate <- total_wells
      message("✔️ All wells (including borders) will be randomized.")
    } else {
      non_border_indices <- setdiff(seq_len(total_wells), border_indices)
      available_per_plate <- length(non_border_indices)
      message("✔️ Only non-border wells will be randomized. Border wells will remain empty ('X').")
    }
    
    available_total <- plates_number * available_per_plate
    if(total_units > available_total) {
      stop("❌ Total units (", total_units, ") exceed available randomizable wells (", available_total,
           ") across ", plates_number, " plate(s) when excluding borders.")
    } else {
      message("✔️ Total available randomizable wells: ", available_total)
    }
    
    # Step 6.6: Ask for the seed and base file names for saving the plate plans
    seed_value <- get_input_local("seed_value",
                                  "❓ Enter a seed value for randomization: ",
                                  validate_fn = function(x) !is.na(x),
                                  transform_fn = function(x) as.integer(x),
                                  error_msg = "❌ Invalid seed value. Please enter an integer.")
    set.seed(seed_value)
    message("✔️ Seed value set to: ", seed_value)
    
    # Base names for saving files; a suffix will be added for each plate generated
    plate_plan_name_csv <- get_input_local("plate_plan_name_csv",
                                           "❓ Enter a base file name for the plate plan CSV (without extension): ",
                                           validate_fn = function(x) x != "",
                                           transform_fn = function(x) trimws(x),
                                           error_msg = "❌ Invalid CSV file name.")
    
    plate_plan_name_xlsx <- get_input_local("plate_plan_name_xlsx",
                                            "❓ Enter a base file name for the plate plan Excel (without extension): ",
                                            validate_fn = function(x) x != "",
                                            transform_fn = function(x) trimws(x),
                                            error_msg = "❌ Invalid Excel file name.")
    
    # Step 6.7: Generate the global assignment vector for the randomizable wells only
    assignments_available <- unlist(lapply(conditions, function(cond) {
      rep(paste0(cond, "_", seq_len(replicates_number)), each = units_per_replicate)
    }))
    assignments_available <- c(assignments_available, rep("X", available_total - length(assignments_available)))
    assignments_available <- sample(assignments_available)
    
    # Step 6.8: Loop through to create each plate plan
    plate_plan_list <- list()
    for (i in seq_len(plates_number)) {
      # Create plate wells names with a plate-specific suffix
      plate_wells <- paste0(wells_template, "_plate_", i)
      # Initialize the plate assignment vector with "X" (empty)
      plate_assignments <- rep("X", total_wells)
      
      if(tolower(keep_border) %in% c("yes", "y")) {
        # If including all wells, assign for all wells
        idx_start <- (i - 1) * available_per_plate + 1
        idx_end <- i * available_per_plate
        plate_assignments <- assignments_available[idx_start:idx_end]
      } else {
        # Excluding border wells: only assign to non-border indices
        idx_start <- (i - 1) * available_per_plate + 1
        idx_end <- i * available_per_plate
        plate_assignments[non_border_indices] <- assignments_available[idx_start:idx_end]
        # Border wells remain as "X"
      }
      
      # Reshape assignment vector into a matrix based on the grid
      plate_matrix <- matrix(plate_assignments, nrow = length(rows), ncol = length(cols),
                             byrow = TRUE, dimnames = list(rows, cols))
      plate_plan <- data.frame(animal = plate_wells,
                               condition = as.vector(plate_matrix),
                               stringsAsFactors = FALSE)
      plate_plan_list[[i]] <- plate_plan
      
      # Save the plate plan as a CSV file
      csv_path <- file.path(plan_dir, paste0(plate_plan_name_csv, "_plate_", i, ".csv"))
      write.csv2(plate_plan, file = csv_path, row.names = FALSE)
      message("💾 Plate plan saved as CSV: ", csv_path)
      
      # Save the plate plan as an Excel file
      xlsx_path <- file.path(plan_dir, paste0(plate_plan_name_xlsx, "_plate_", i, ".xlsx"))
      openxlsx::write.xlsx(plate_plan, file = xlsx_path, rowNames = FALSE)
      message("💾 Plate plan saved as Excel file: ", xlsx_path)
    }
    
    message("🎉 New plate plan(s) created successfully!")
    
  } else {
    # Step 7: Load one or more existing plate plan file(s)
    plate_plan_names <- get_input_local("plate_plan_name",
                                        "❓ Enter the existing plate plan file name(s) separated by ';' (with .csv or .xlsx extension): ",
                                        validate_fn = function(x) {
                                          files <- trimws(unlist(strsplit(x, ";")))
                                          all(sapply(files, function(f) f != "" && grepl("\\.csv$|\\.xlsx$", f, ignore.case = TRUE) &&
                                                       file.exists(file.path(plan_dir, f))))
                                        },
                                        transform_fn = function(x) trimws(x),
                                        error_msg = "❌ One or more file names do not exist or are invalid.")
    file_names <- trimws(unlist(strsplit(plate_plan_names, ";")))
    for (f in file_names) {
      file_extension <- tolower(tools::file_ext(f))
      selected_plan_path <- file.path(plan_dir, f)
      plan_df <- tryCatch({
        if (file_extension == "csv") {
          message("🔍 Reading plate plan from CSV: ", f)
          read.csv2(selected_plan_path, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
        } else if (file_extension == "xlsx") {
          message("🔍 Reading plate plan from Excel: ", f)
          readxl::read_excel(selected_plan_path, sheet = 1, col_names = TRUE)
        } else {
          stop("❌ Unsupported file format.")
        }
      }, error = function(e) {
        stop("❌ Error loading the plate plan file ", f, ": ", e$message)
      })
      if (!all(c("animal", "condition") %in% colnames(plan_df))) {
        stop("❌ Plate plan ", f, " is missing required columns: 'animal' and 'condition'.")
      }
      message("✔️ Plate plan ", f, " loaded and validated successfully.")
      # Save back in the original format
      if (file_extension == "csv") {
        write.csv2(plan_df, file = selected_plan_path, row.names = FALSE)
      } else {
        openxlsx::write.xlsx(plan_df, file = selected_plan_path, rowNames = FALSE)
      }
      plate_plan_list[[length(plate_plan_list) + 1]] <- plan_df
    }
  }
  
  # Step 8: Finalize and assign the plate plan list globally, then return it.
  message("🎉 Plate plan generation completed!")
  message("💾 Plate plan(s) are now available globally as 'plate_plan_df_list'.\n")
  assign("plate_plan_df_list", plate_plan_list, envir = .GlobalEnv)
  return(plate_plan_list)
}
