# -----------------------------------------------------------
# final_step_run_full_pipeline.R
# -----------------------------------------------------------
# This script runs the full Zebrabox Experiment Pipeline.
#
# It does the following:
#   1. Loads pre-recorded inputs (if available) from a pipeline inputs file.
#   2. Prompts the user for settings (primary mode, secondary mode, clear outputs, etc.)
#      using pre-recorded inputs when available.
#   3. Records all used inputs in a global data frame (input_record_df).
#   4. Loads step scripts based on the selected modes and then runs the full pipeline.
#
# The pipeline steps (e.g., generate_plate_plan, extract_data, etc.) are assumed to be
# defined in the appropriate step_1, step_2, …, step_12 scripts.
# -----------------------------------------------------------

# Global list to record all input values (will be converted to a data frame later)
input_record_list <<- list()

# Function to load pre-recorded inputs from the pipeline inputs file.
load_pipeline_inputs <- function() {
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
  return(pipeline_inputs)
}

# Generic helper function to get input (either from pre-recorded values or interactively)
# and record the used input.
get_input <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                      transform_fn = function(x) x,
                      error_msg = "Invalid input. Please try again.", pipeline_inputs) {
  if (!is.null(pipeline_inputs[[param]]) &&
      !is.na(pipeline_inputs[[param]]) &&
      pipeline_inputs[[param]] != "") {
    candidate <- transform_fn(pipeline_inputs[[param]])
    if (validate_fn(candidate)) {
      message("Using pre-recorded input for '", param, "': ", candidate)
      input_record_list[[param]] <<- candidate
      return(candidate)
    } else {
      message("Pre-recorded input for '", param, "' is invalid. Falling back to interactive prompt.")
    }
  }
  repeat {
    user_input <- readline(prompt = prompt_msg)
    candidate <- transform_fn(user_input)
    if (validate_fn(candidate)) {
      input_record_list[[param]] <<- candidate
      return(candidate)
    } else {
      message(error_msg)
    }
  }
}

# Function to check and install required packages.
check_and_install_packages <- function(packages) {
  suppressWarnings(suppressPackageStartupMessages({
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, quiet = TRUE) # Install silently if not found
      }
      library(pkg, character.only = TRUE, quietly = TRUE) # Load silently
    }
  }))
}

# Function to ask the user whether to empty the outputs directory.
prompt_clear_outputs <- function(output_base_dir, pipeline_inputs) {
  clean_output <- get_input("clean_output_directory",
                            "❓ Do you want to empty the outputs directory before proceeding? (yes/y or no/n): ",
                            validate_fn = function(x) x %in% c("yes", "y", "no", "n"),
                            transform_fn = function(x) tolower(trimws(x)),
                            error_msg = "⚠️ Invalid response. Please enter 'yes/y' or 'no/n'.",
                            pipeline_inputs = pipeline_inputs)
  
  if (clean_output %in% c("yes", "y")) {
    message("🧹 Clearing outputs directory...")
    files_to_delete <- list.files(output_base_dir, recursive = TRUE, full.names = TRUE)
    if (length(files_to_delete) > 0) {
      file.remove(files_to_delete)
      message("✅ Outputs directory cleared.")
    } else {
      message("⚠️ Outputs directory was already empty.")
    }
  } else {
    message("🚀 Proceeding without clearing outputs.")
  }
}

# Function to display pipeline guidelines and prompt for modes.
prompt_guidelines <- function(pipeline_inputs) {
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Zebrabox Experiment Pipeline!\n")
  message("This pipeline guides you through the following steps:")
  message("  🔢 Generate a fully randomized plate layout for your Zebrabox experiments.")
  message("  📂 Extract raw data obtained from the Zebrabox.")
  message("  🔄 Format and preprocess the extracted data for visualization.")
  message("  📊 Create figures and tables to visualize your experimental results.\n")
  
  primary_mode_map <- list(
    tm = "tracking_mode",
    tracking_mode = "tracking_mode",
    qm = "quantization_mode",
    quantization_mode = "quantization_mode"
  )
  secondary_mode_map <- list(
    ldm = "light_dark_mode",
    light_dark_mode = "light_dark_mode",
    vm = "vibration_mode",
    vibration_mode = "vibration_mode"
  )
  
  primary_input <- get_input("primary_mode",
                             "❓ Select the primary mode you want to use (tracking_mode/tm or quantization_mode/qm): ",
                             validate_fn = function(x) x %in% names(primary_mode_map),
                             transform_fn = function(x) tolower(trimws(x)),
                             error_msg = "⚠️ Invalid input. Please type 'tracking_mode/tm' or 'quantization_mode/qm'.",
                             pipeline_inputs = pipeline_inputs)
  primary_mode <- primary_mode_map[[primary_input]]
  if (primary_mode == "quantization_mode") {
    message("⚠️ Quantization mode is still under development ('encore en travaux l'équipe'). Exiting the function.")
    return(NULL)
  }
  
  secondary_input <- get_input("secondary_mode",
                               "❓ Select the secondary mode you want to use (light_dark_mode/ldm or vibration_mode/vm): ",
                               validate_fn = function(x) x %in% names(secondary_mode_map),
                               transform_fn = function(x) tolower(trimws(x)),
                               error_msg = "⚠️ Invalid input. Please type 'light_dark_mode/ldm' or 'vibration_mode/vm'.",
                               pipeline_inputs = pipeline_inputs)
  secondary_mode <- secondary_mode_map[[secondary_input]]
  
  required_packages <- c(
    "dplyr", "ggplot2", "plotly", "htmlwidgets", "colorspace", "webshot2",
    "tidyr", "purrr", "openxlsx", "grid", "jpeg", "stringr", "readxl", "readr",
    "ggpattern", "remotes", "reprex"
  )
  check_and_install_packages(required_packages)
  
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    remotes::install_github("rstudio/webshot2")
  }
  if (!requireNamespace("chromote", quietly = TRUE)) {
    remotes::install_github("rstudio/chromote")
  }
  
  message("\n📦 All necessary packages are installed and loaded. You are ready to proceed!")
  message("🎯 You're all set! The pipeline will run in ", primary_mode, " > ", secondary_mode, ".")
  message("\n---\n---\n---\n")
  
  return(list(primary_mode = primary_mode, secondary_mode = secondary_mode))
}

# Function to load scripts dynamically based on primary and secondary modes.
load_mode_scripts <- function(primary_mode, secondary_mode) {
  primary_abbrev <- switch(
    primary_mode,
    "tracking_mode" = "tm",
    "quantization_mode" = "qm",
    stop("Invalid primary mode provided. Check your input.")
  )
  secondary_abbrev <- switch(
    secondary_mode,
    "light_dark_mode" = "ldm",
    "vibration_mode" = "vm",
    stop("Invalid secondary mode provided. Check your input.")
  )
  
  base_path <- file.path("scripts", primary_mode, secondary_mode)
  script_names <- paste0("step_", 1:12, "_", primary_abbrev, "_", secondary_abbrev, ".R")
  script_paths <- file.path(base_path, script_names)
  
  lapply(script_paths, function(scr) {
    if (file.exists(scr)) {
      source(scr)
    } else {
      warning(paste("Script file not found:", scr))
    }
  })
}

# Function to run the full pipeline.
run_full_pipeline <- function() {
  # Load pre-recorded inputs.
  pipeline_inputs <- load_pipeline_inputs()
  
  # Prompt for primary and secondary modes.
  modes <- prompt_guidelines(pipeline_inputs)
  if (is.null(modes)) return(NULL)
  primary_mode <- modes$primary_mode
  secondary_mode <- modes$secondary_mode
  
  # Set up the outputs directory and prompt whether to clear it.
  output_base_dir <- file.path("outputs", primary_mode, secondary_mode)
  prompt_clear_outputs(output_base_dir, pipeline_inputs)
  
  dir.create(file.path(output_base_dir, "figures/lineplots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_base_dir, "figures/boxplots"), recursive = TRUE, showWarnings = FALSE)
  
  # Load mode-specific scripts (e.g., step_1, step_2, …, step_12).
  load_mode_scripts(primary_mode, secondary_mode)
  
  # Run the pipeline steps.
  plate_plan <- generate_plate_plan()                             
  raw_data <- extract_data()                                      
  enriched_data <- import_and_process_data(data = raw_data, plate_plan = plate_plan)
  data_with_periods <- assign_periods_with_custom_durations(enriched_data)
  zone_data_list <- process_zones(data_with_periods)              
  zone_processed <- calculate_and_clean_zone_data(zone_data_list) 
  pretreated_data <- pre_visualization_data_treatment(zone_processed$zone_combined)
  
  generate_and_save_lineplots()                                   
  generate_and_save_boxplots_with_excel_files()                   
  prepare_delta_data_for_analysis()
  generate_and_save_boxplots_delta_with_excel_files()
  
  end_pipeline_message()
  
  # Create a global data frame of all recorded inputs.
  input_record_df <<- data.frame(parameter = names(input_record_list),
                                 input = unlist(input_record_list),
                                 stringsAsFactors = FALSE)
  
  return(pretreated_data)
}

