generate_plate_plan <- function(plan_dir = "inputs/tracking_mode/vibration_mode/plate_plan") {
  # Ensure the output directory exists; if not, create it.
  if (!dir.exists(plan_dir)) {
    dir.create(plan_dir, recursive = TRUE)
    message("Directory created: ", plan_dir)
  }
  
  message("\n---\n---\n---\n")
  message("\n👋 Welcome to the Multi-Well Plate Plan Generator!\n")
  message("This function assists you with:")
  message("🔢 Providing an existing plate plan or generating a new one.")
  message("🎲 Randomizing well assignments if no plan is provided.")
  message("📁 Supporting plate plan files in both '.csv' and '.xlsx' formats.")
  message("💾 Saving the generated plan in the chosen formats.\n")
  
  # Initialize a variable to hold the base file name (without extension)
  save_file_base <- NULL
  
  # Step 1: Check if the user has an existing plate plan.
  repeat {
    use_existing_plan <- readline(prompt = "❓ Do you already have a plate plan? (yes/no): ")
    if (tolower(use_existing_plan) %in% c("yes", "y", "no", "n")) {
      break
    } else {
      message("⚠️ Invalid response. Please enter 'yes' or 'no'.\n")
    }
  }
  
  if (tolower(use_existing_plan) %in% c("yes", "y")) {
    message("\n💡 Please ensure the following before proceeding:\n")
    message("📂 The plate plan is saved in the directory: '", plan_dir, "'.")
    message("🗂️ The file format can be either '.csv' or '.xlsx'.")
    message("📄 The file should contain these required columns:")
    message("  - `animal`: List of wells corresponding to your plate type (e.g., A01, B01).")
    message("  - `condition`: Experimental conditions with replicate numbers (e.g., 'control_1', '5 mg/L_1').\n")
    
    repeat {
      plan_file_name <- readline(prompt = "📄 Enter the name of your plate plan file (must end with '.csv' or '.xlsx'): ")
      
      # Check if the input is empty.
      if (plan_file_name == "") {
        message("❌ No file name entered. Please provide a valid file name.")
        next
      }
      
      # Check if the file has the correct extension.
      if (!grepl("\\.csv$|\\.xlsx$", plan_file_name, ignore.case = TRUE)) {
        message("❌ The file must end with '.csv' or '.xlsx'. Please try again.")
        next
      }
      
      # Check if the file exists in the directory.
      selected_plan_path <- file.path(plan_dir, plan_file_name)
      if (!file.exists(selected_plan_path)) {
        message("❌ The file '", plan_file_name, "' does not exist in the directory '", plan_dir, "'.")
        message("💡 Ensure the file is correctly named and try again.")
      } else {
        message("✔️ Using plate plan file: ", selected_plan_path)
        break
      }
    }
    
    # Load and validate the plate plan.
    plate_plan <- tryCatch({
      if (grepl("\\.csv$", plan_file_name, ignore.case = TRUE)) {
        message("📖 Reading plate plan from CSV file...")
        read.csv2(selected_plan_path, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
      } else if (grepl("\\.xlsx$", plan_file_name, ignore.case = TRUE)) {
        message("📖 Reading plate plan from Excel file...")
        readxl::read_excel(selected_plan_path, sheet = 1, col_names = TRUE)
      } else {
        stop("❌ Unsupported file format. Please use '.csv' or '.xlsx'.")
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
    
    # Remove extension from the loaded file name to reuse as base for saving.
    save_file_base <- sub("\\.csv$|\\.xlsx$", "", plan_file_name, ignore.case = TRUE)
    
  } else {
    # Step 2: Generate a new plate plan.
    repeat {
      plate_type <- as.integer(readline(prompt = "🔢 Enter the type of plate you are using (24, 48, or 96 wells): "))
      if (!is.na(plate_type) && plate_type %in% c(24, 48, 96)) {
        message("✔️ Plate type selected: ", plate_type, " wells.")
        break
      } else {
        message("⚠️ Invalid plate type. Please enter 24, 48, or 96.")
      }
    }
    
    rows <- LETTERS[1:(ifelse(plate_type == 24, 4, ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:(ifelse(plate_type == 24, 6, ifelse(plate_type == 48, 8, 12)))
    total_wells <- plate_type
    
    repeat {
      num_conditions <- as.integer(readline(prompt = "🔢 Enter the number of conditions (e.g., 3, 5, 10): "))
      if (!is.na(num_conditions) && num_conditions > 0) {
        message("✔️ Number of conditions: ", num_conditions)
        break
      } else {
        message("⚠️ Invalid number. Please enter a positive integer.")
      }
    }
    
    repeat {
      conditions_input <- readline(prompt = "📄 Enter the names of your conditions separated by commas (e.g., Control, 5 mg/L): ")
      conditions <- trimws(unlist(strsplit(conditions_input, ",")))
      
      if (length(conditions) != num_conditions) {
        message("❌ The number of entered conditions (", length(conditions), 
                ") does not match the expected number (", num_conditions, "). Please try again.")
      } else if (length(unique(conditions)) != length(conditions)) {
        message("❌ Duplicate condition names detected: ", paste(conditions[duplicated(conditions)], collapse = ", "))
        message("💡 Please ensure all condition names are unique and try again.")
      } else {
        message("✔️ Conditions recorded: ", paste(conditions, collapse = ", "))
        break
      }
    }
    
    repeat {
      num_replicates <- as.integer(readline(prompt = "🔢 Enter the number of replicates per condition (e.g., 3, 5): "))
      if (!is.na(num_replicates) && num_replicates > 0) {
        message("✔️ Number of replicates per condition: ", num_replicates)
        break
      } else {
        message("⚠️ Invalid number. Please enter a positive integer.")
      }
    }
    
    repeat {
      daphnia_per_replicate <- as.integer(readline(prompt = "🔢 Enter the number of daphnia per replicate (e.g., 10): "))
      if (!is.na(daphnia_per_replicate) && daphnia_per_replicate > 0) {
        total_daphnia <- num_conditions * num_replicates * daphnia_per_replicate
        if (total_daphnia <= total_wells) {
          message("✔️ Total daphnia: ", total_daphnia, " (fits within ", total_wells, " wells).")
          break
        } else {
          message("⚠️ The total number of daphnia exceeds the available wells on the plate. Please enter a smaller number.")
        }
      } else {
        message("⚠️ Invalid number. Please enter a positive integer.")
      }
    }
    
    repeat {
      seed_value <- as.integer(readline(prompt = "🎲 Enter a seed value for randomization (e.g., between 1 and 1000): "))
      if (!is.na(seed_value)) {
        set.seed(seed_value)
        message("✔️ Seed value recorded: ", seed_value)
        message("💡 Note down this seed value to reproduce the same randomization.")
        break
      } else {
        message("⚠️ Invalid seed value. Please enter a valid integer.")
      }
    }
    
    # Generate well identifiers.
    wells <- with(expand.grid(Row = rows, Column = cols), 
                  paste0(Row, sprintf("%02d", Column)))
    
    assignments <- unlist(lapply(conditions, function(cond) {
      rep(paste0(cond, "_", seq_len(num_replicates)), each = daphnia_per_replicate)
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
    
    # For a new plan, prompt for a file name (without extension) for saving.
    repeat {
      new_file_name <- readline(prompt = "💾 Enter a name for the plate plan file (without extension): ")
      if (new_file_name != "") {
        save_file_base <- new_file_name
        break
      } else {
        message("⚠️ Invalid file name. Please enter a valid name.")
      }
    }
  }
  
  # Save the plate plan as both CSV and XLSX files using the determined base name.
  csv_path <- file.path(plan_dir, paste0(save_file_base, ".csv"))
  write.csv2(plate_plan, file = csv_path, row.names = FALSE)
  message("✔️ Plate plan saved successfully as CSV: ", csv_path)
  
  xlsx_path <- file.path(plan_dir, paste0(save_file_base, ".xlsx"))
  openxlsx::write.xlsx(plate_plan, file = xlsx_path, rowNames = FALSE)
  message("✔️ Plate plan saved successfully as Excel file: ", xlsx_path)
  
  message("🎉 Plate plan generation completed successfully!")
  message("💾 Plate plan has been saved in the global environment as 'plate_plan_df'.\n")
  
  # Assign the plate plan to the global environment.
  assign("plate_plan_df", plate_plan, envir = .GlobalEnv)
  
  return(plate_plan)
}
