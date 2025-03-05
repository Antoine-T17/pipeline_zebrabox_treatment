# -----------------------------------------------------------
# File: final_step_run_full_pipeline.R
# -----------------------------------------------------------
# This script runs the full Zebrabox Experiment Pipeline.
#
# It does the following:
#   1. Loads pre-recorded inputs from a multi-sheet pipeline_inputs.xlsx file.
#      The first sheet ("select_the_path") contains mode selections.
#      Based on these, the corresponding sheet (e.g. "tm_ldm") is loaded.
#   2. Prompts the user for additional settings using pre-recorded inputs when available.
#   3. Records all used inputs in a global data frame (input_record_df).
#   4. Loads step scripts based on the selected modes and then runs the full pipeline.
# -----------------------------------------------------------

# Global list to record all input values (will be converted to a data frame later)
input_record_list <<- list()

# Function to load pipeline inputs from a multi-sheet Excel file.
load_pipeline_inputs <- function() {
  inputs_path <- "inputs/inputs_values"
  pipeline_file <- file.path(inputs_path, "pipeline_inputs.xlsx")
  
  if (!file.exists(pipeline_file)) {
    stop("❌ Pipeline inputs file not found at: ", pipeline_file)
  }
  
  # Load the "select_the_path" sheet.
  select_df <- readxl::read_excel(pipeline_file, sheet = "select_the_path")
  if (!all(c("parameters", "input") %in% colnames(select_df))) {
    stop("❌ The 'select_the_path' sheet must contain columns 'parameters' and 'input'.")
  }
  select_inputs <- setNames(as.list(select_df$input), select_df$parameters)
  
  # Extract mode selections.
  primary_mode <- tolower(trimws(select_inputs[["primary_mode"]]))
  secondary_mode <- tolower(trimws(select_inputs[["secondary_mode"]]))
  
  # Map to abbreviated sheet names.
  primary_abbrev <- switch(primary_mode,
                           "tracking_mode" = "tm",
                           "tm" = "tm",
                           "quantization_mode" = "qm",
                           "qm" = "qm",
                           stop("Invalid primary_mode in 'select_the_path'."))
  secondary_abbrev <- switch(secondary_mode,
                             "light_dark_mode" = "ldm",
                             "ldm" = "ldm",
                             "vibration_mode" = "vm",
                             "vm" = "vm",
                             stop("Invalid secondary_mode in 'select_the_path'."))
  
  target_sheet <- paste0(primary_abbrev, "_", secondary_abbrev)
  message("🔎 Loading pipeline inputs from sheet: ", target_sheet)
  
  df <- readxl::read_excel(pipeline_file, sheet = target_sheet)
  if (!all(c("parameters", "input") %in% colnames(df))) {
    stop("❌ The ", target_sheet, " sheet must contain columns 'parameters' and 'input'.")
  }
  pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  
  # Merge in the basic selections.
  pipeline_inputs[["primary_mode"]] <- primary_mode
  pipeline_inputs[["secondary_mode"]] <- secondary_mode
  pipeline_inputs[["clean_output_directory"]] <- tolower(trimws(select_inputs[["clean_output_directory"]]))
  
  return(pipeline_inputs)
}

# Generic helper function to get input (from pipeline_inputs or interactively)
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
        install.packages(pkg, quiet = TRUE)
      }
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }))
}

# Function to ask the user whether to empty the outputs directory.
prompt_clear_outputs <- function(output_base_dir, pipeline_inputs) {
  clean_output <- get_input("clean_output_directory",
                            "❓ Do you want to empty the outputs directory before proceeding? (yes/y or no/n): ",
                            validate_fn = function(x) x %in% c("yes", "y", "no", "n"),
                            transform_fn = function(x) tolower(trimws(x)),
                            error_msg = "⚠️ Please enter 'yes/y' or 'no/n'.",
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

# Function to display pipeline guidelines and prompt for mode selections.
prompt_guidelines <- function(pipeline_inputs) {
  message("\n---\n")
  message("👋 Welcome to the Zebrabox Experiment Pipeline!\n")
  message("📋 This pipeline guides you through the following steps:")
  message("  • Generate a randomized plate layout for your Zebrabox experiments.")
  message("  • Extract raw data from the Zebrabox.")
  message("  • Preprocess and format the data for visualization.")
  message("  • Create figures and tables to display experimental results.\n")
  
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
                             "❓ Select the primary mode (tracking_mode/tm or quantization_mode/qm): ",
                             validate_fn = function(x) x %in% names(primary_mode_map),
                             transform_fn = function(x) tolower(trimws(x)),
                             error_msg = "⚠️ Please enter 'tracking_mode/tm' or 'quantization_mode/qm'.",
                             pipeline_inputs = pipeline_inputs)
  primary_mode <- primary_mode_map[[primary_input]]
  if (primary_mode == "quantization_mode") {
    message("⚠️ Quantization mode is under development. Exiting.")
    return(NULL)
  }
  
  secondary_input <- get_input("secondary_mode",
                               "❓ Select the secondary mode (light_dark_mode/ldm or vibration_mode/vm): ",
                               validate_fn = function(x) x %in% names(secondary_mode_map),
                               transform_fn = function(x) tolower(trimws(x)),
                               error_msg = "⚠️ Please enter 'light_dark_mode/ldm' or 'vibration_mode/vm'.",
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
  message("🎯 The pipeline will run in ", primary_mode, " > ", secondary_mode, ".")
  message("\n---\n")
  
  return(list(primary_mode = primary_mode, secondary_mode = secondary_mode))
}

# Function to load scripts dynamically based on primary and secondary modes.
load_mode_scripts <- function(primary_mode, secondary_mode) {
  primary_abbrev <- switch(
    primary_mode,
    "tracking_mode" = "tm",
    "quantization_mode" = "qm",
    stop("Invalid primary mode.")
  )
  secondary_abbrev <- switch(
    secondary_mode,
    "light_dark_mode" = "ldm",
    "vibration_mode" = "vm",
    stop("Invalid secondary mode.")
  )
  
  base_path <- file.path("scripts", primary_mode, secondary_mode)
  script_names <- paste0("step_", 1:11, "_", primary_abbrev, "_", secondary_abbrev, ".R")
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
  # Load pipeline inputs from the multi-sheet Excel file.
  pipeline_inputs <<- load_pipeline_inputs()
  
  # Prompt for primary and secondary modes.
  modes <- prompt_guidelines(pipeline_inputs)
  if (is.null(modes)) return(NULL)
  primary_mode <- modes$primary_mode
  secondary_mode <- modes$secondary_mode
  
  # Set up outputs directory.
  output_base_dir <- file.path("outputs", primary_mode, secondary_mode)
  prompt_clear_outputs(output_base_dir, pipeline_inputs)
  dir.create(file.path(output_base_dir, "figures/lineplots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_base_dir, "figures/boxplots"), recursive = TRUE, showWarnings = FALSE)
  
  # Load mode-specific scripts.
  load_mode_scripts(primary_mode, secondary_mode)
  
  # Run the pipeline steps.
  plate_plan <- generate_plate_plan()                             
  raw_data <- extract_data()                                      
  enriched_data <- import_and_process_data(data = raw_data, plate_plan = plate_plan)
  data_with_periods <- assign_periods_with_custom_durations(enriched_data)
  zone_data_list <- process_zones(data_with_periods)              
  zone_processed <- calculate_and_clean_zone_data(zone_data_list) 
  pretreated_data <- pre_visualization_data_treatment(zone_calculated_list)

  generate_and_save_lineplots()                                   
  generate_and_save_boxplots_with_excel_files()                   
  generate_and_save_boxplots_delta_with_excel_files()
  
  end_pipeline_message()
  
  # Create a global data frame of all recorded inputs.
  input_record_df <<- data.frame(parameter = names(input_record_list),
                                 input = unlist(input_record_list),
                                 stringsAsFactors = FALSE)
  
  return(pretreated_data)
}

# Run the full pipeline.
run_full_pipeline()
