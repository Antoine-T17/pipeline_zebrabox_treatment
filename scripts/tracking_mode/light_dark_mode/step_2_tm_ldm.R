# Refactored extract_and_enrich_data function
extract_and_enrich_data <- function() {
  # TAG: HELPER FUNCTIONS
  convert_numeric_cols <- function(df, cols) {
    for (col in intersect(names(df), cols)) {
      df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
    }
    df
  }
  
  fmt <- function(x) {
    if (!is.list(x)) {
      paste(x, collapse = ", ")
    } else {
      # each sub-vector → comma list, then glue plates with " ; "
      paste(vapply(x, function(z) paste(z, collapse = ", "), character(1)),
            collapse = " ; ")
    }
  }
  
  get_input_local <- function(param, prompt_msg,
                              validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(input_record_list[[param]])) {
      message("💾 Using cached input for '", param, "'.")
      return(input_record_list[[param]])
    }
    raw <- pipeline_inputs[[param]]
    if (!is.null(raw) && !is.na(raw) && raw != "") {
      candidate <- transform_fn(raw)
      if (validate_fn(raw)) {
        message("💾 Using pre-recorded input for '", param, "': ", fmt(candidate))
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate  <- transform_fn(user_input)
      if (validate_fn(user_input)) {
        message("✔️ Input for '", param, "' recorded: ", fmt(candidate))
        input_record_list[[param]] <<- candidate
        return(candidate)
      }
      message(error_msg)
    }
  }
  
  read_and_clean <- function(f) {
    full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", f)
    while (!file.exists(full_path)) {
      message("❌ The file '", full_path, "' does not exist. Please try again.")
      f <- get_input_local(
        "raw_data_file_name",
        "❓ Enter the raw data file name (including extension '.csv' or '.xlsx'): ",
        validate_fn = function(x) x != "" && grepl("\\.(csv|xlsx)$", x, ignore.case = TRUE),
        transform_fn = function(x) trimws(as.character(x)),
        error_msg = "❌ Invalid file name. Ensure it ends with '.csv' or '.xlsx'."
      )
      full_path <- file.path("inputs/tracking_mode/light_dark_mode/raw_data", f)
    }
    message("✔️ File detected: ", full_path)
    data <- tryCatch({
      if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        message("🔍 Detected CSV format. Reading file...")
        readr::read_csv(full_path, show_col_types = FALSE)
      } else {
        message("🔍 Detected Excel format. Reading file...")
        readxl::read_excel(full_path)
      }
    }, error = function(e) stop("❌ Error while reading the file: ", e$message))
    message("✔️ Data successfully read from ", f, ".")
    message("🛠️ Converting potential numeric columns in ", f, "...")
    convert_numeric_cols(data, potential_numeric_cols)
  }
  
  # TAG: WELCOME MESSAGES
  message("\n---\n")
  message("👋 Welcome to the Data Extraction, Enrichment, Period Assignment, and Zone Processing! \n")
  message("📋 This function will perform the following tasks:")
  message("   1. Read raw experimental data file(s) from 'inputs/tracking_mode/light_dark_mode/raw_data'.")
  message("   2. Convert numeric columns to proper numeric formats.")
  message("   3. Match raw data (using the 'animal' column, e.g., A01, A02, etc.) with the corresponding")
  message("      plate plan conditions from 'plate_plan_df_list'.")
  message("   4. Generate grouping/tagging columns ('condition_grouped' and 'condition_tagged') and add 'plate_id'.")
  message("   5. For each plate, assign experimental periods based on a universal period sequence")
  message("      and user-defined boundaries, creating a simplified period column.")
  message("   6. For each plate, process zones:")
  message("        - Filter data by specified zones;")
  message("        - Calculate new numeric variables and convert the 'start' column to minutes")
  message("          (using a universal unit);")
  message("        - Remove rows with 'start' equal to specified removal times;")
  message("        - Reorder columns and combine all zones for the plate.")
  message("   7. Save global objects: 'enriched_data_df_list', 'data_with_periods_df_list',")
  message("      'period_boundaries_list', 'boundary_associations_list', and 'zone_calculated_list'.\n")
  
  # TAG: RETRIEVE PRE-RECORDED PIPELINE INPUTS
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  input_record_list <<- list()
  
  # TAG: CENTRALIZE INPUTS
  raw_files_input <- get_input_local(
    "raw_data_file_name",
    "❓ Enter the raw data file name(s) (with extension '.csv' or '.xlsx'), separated by ';': ",
    validate_fn = function(x) {
      files <- trimws(unlist(strsplit(as.character(x), ";")))
      all(sapply(files, function(f) f != "" && grepl("\\.(csv|xlsx)$", f, ignore.case = TRUE)))
    },
    transform_fn = function(x) trimws(as.character(x)),
    error_msg = "❌ Invalid file name(s). Ensure each ends with '.csv' or '.xlsx'."
  )
  
  period_sequence_input <- get_input_local(
    "period_sequence",
    "❓ Enter the universal period sequence (comma-separated, e.g., acclimatation, light, dark, light, dark): ",
    validate_fn = function(x) length(trimws(unlist(strsplit(as.character(x), ",")))) > 0,
    transform_fn = function(x) trimws(as.character(x))
  )
  
  boundaries_input <- get_input_local(
    "period_boundaries",
    sprintf("❓ Enter %d numeric time codes (comma-separated, in seconds) for the period boundaries: ",
            length(trimws(unlist(strsplit(period_sequence_input, ",")))) - 1),
    validate_fn = function(x) {
      b <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
      length(b) == (length(trimws(unlist(strsplit(period_sequence_input, ",")))) - 1) && all(!is.na(b)) && all(b > 0)
    },
    transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
    error_msg = sprintf("❌ Please enter %d positive numeric time codes separated by commas.",
                        length(trimws(unlist(strsplit(period_sequence_input, ",")))) - 1)
  )
  
  start_unit_input <- get_input_local(
    "start_column_unit",
    "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ",
    validate_fn = function(x) tolower(trimws(as.character(x))) %in% c("h", "m", "s"),
    transform_fn = function(x) tolower(trimws(as.character(x))),
    error_msg = "❌ Please enter 'h', 'm', or 's'."
  )
  
  remove_time_input <- get_input_local(
    "remove_time",
    sprintf("❓ Enter the remove_time threshold(s) (in minutes) for %d plate(s), separated by ';' (e.g., 1,6,27 ; no): ",
            if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1),
    validate_fn = function(x) {
      tokens <- trimws(unlist(strsplit(as.character(x), ";")))
      if (length(tokens) != if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1) return(FALSE)
      for (tok in tokens) {
        if (tolower(tok) == "no") next
        stoks <- trimws(unlist(strsplit(tok, ",")))
        if (any(is.na(as.numeric(stoks))) || any(as.numeric(stoks) <= 0)) return(FALSE)
      }
      TRUE
    },
    transform_fn = function(x) {
      tokens <- trimws(unlist(strsplit(as.character(x), ";")))
      lapply(tokens, function(tok) if (tolower(tok) == "no") NA else as.numeric(trimws(unlist(strsplit(tok, ",")))))
    },
    error_msg = "❌ Invalid remove_time input."
  )
  
  suspect_wells_input <- get_input_local(
    "remove_suspect_wells",
    sprintf("❓ Suspect wells to remove for %d plates, sep. ';' (e.g. A01,A02 ; no): ",
            if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1),
    validate_fn = function(x) length(strsplit(x, ";")[[1]]) == if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1,
    transform_fn = function(x) {
      tokens <- strsplit(as.character(x), ";")[[1]]
      lapply(tokens, function(y) if (tolower(y) == "no") character(0) else trimws(strsplit(y, ",")[[1]]))
    }
  )
  
  remove_conditions_input <- get_input_local(
    "remove_conditions",
    sprintf("❓ Conditions to remove for %d plates, sep. ';' (e.g. C1,C2 ; no): ",
            if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1),
    validate_fn = function(x) length(strsplit(x, ";")[[1]]) == if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1,
    transform_fn = function(x) {
      tokens <- strsplit(as.character(x), ";")[[1]]
      lapply(tokens, function(y) if (tolower(y) == "no") character(0) else trimws(strsplit(y, ",")[[1]]))
    }
  )
  
  remove_periods_input <- get_input_local(
    "remove_periods",
    sprintf("❓ Period names to remove for %d plates, sep. ';' (e.g. acclimatation,light ; no): ",
            if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1),
    validate_fn = function(x) length(strsplit(x, ";")[[1]]) == if (exists("plate_plan_df_list", envir = .GlobalEnv)) length(get("plate_plan_df_list", envir = .GlobalEnv)) else 1,
    transform_fn = function(x) {
      tokens <- strsplit(as.character(x), ";")[[1]]
      lapply(tokens, function(y) if (tolower(y) == "no") character(0) else trimws(strsplit(y, ",")[[1]]))
    }
  )
  
  zones_input <- get_input_local(
    "zones_number",
    "❓ Enter the zone numbers (comma-separated, e.g., 0,1,2): ",
    validate_fn = function(x) {
      z <- as.numeric(trimws(unlist(strsplit(as.character(x), ","))))
      length(z) > 0 && !any(is.na(z)) && all(z %in% c(0, 1, 2))
    },
    transform_fn = function(x) as.numeric(trimws(unlist(strsplit(as.character(x), ",")))),
    error_msg = "❌ Please enter valid zone numbers (only 0, 1, and 2 allowed)."
  )
  
  input_list <- list(
    raw_data_file_name   = raw_files_input,
    period_sequence      = period_sequence_input,
    period_boundaries    = boundaries_input,
    start_column_unit    = start_unit_input,
    remove_time          = remove_time_input,
    remove_suspect_wells = suspect_wells_input,
    remove_conditions    = remove_conditions_input,
    remove_periods       = remove_periods_input,
    zones_number         = zones_input
  )
  
  # TAG: PREPARE RAW FILES
  file_names <- trimws(unlist(strsplit(input_list$raw_data_file_name, ";")))
  
  # TAG: HANDLE RAW VS PLATE PLAN COUNTS
  if (exists("plate_plan_df_list", envir = .GlobalEnv)) {
    plate_plans <- get("plate_plan_df_list", envir = .GlobalEnv)
    n_plate_plans <- length(plate_plans)
    n_raw_files   <- length(file_names)
    if (n_plate_plans > 1 && n_raw_files == 1) {
      message("⚠️ Multiple plate plan files detected (", n_plate_plans,
              ") but only one raw data file provided. Switching to manual prompt mode.")
      raw_data_files_input <- readline(prompt = sprintf("❓ Please re-enter the raw data file names (separated by ';') to match the number of plate plan files (%d): ", n_plate_plans))
      file_names <- trimws(unlist(strsplit(raw_data_files_input, ";")))
      n_raw_files <- length(file_names)
    }
    if (length(file_names) != n_plate_plans) {
      stop("❌ The number of raw data files (", length(file_names),
           ") must match the number of plate plan files (", n_plate_plans, ").")
    }
  }
  
  # TAG: READ & INITIAL PROCESSING OF RAW DATA FILES
  potential_numeric_cols <- c("start", "an", "inact", "inadist", "inadur",
                              "smlct", "smldist", "smldur",
                              "larct", "lardur", "lardist",
                              "emptyct", "emptydur",
                              "totaldist", "totaldur", "totalct")
  extracted_data_list <- lapply(file_names, read_and_clean)
  
  # TAG: VALIDATE PLATE PLAN AVAILABILITY
  if (!exists("plate_plan_df_list", envir = .GlobalEnv)) {
    stop("❌ No plate plan data found in global environment ('plate_plan_df_list').")
  }
  plate_plans <- get("plate_plan_df_list", envir = .GlobalEnv)
  nplates <- length(plate_plans)
  if (length(extracted_data_list) != nplates) {
    stop("❌ The number of raw data files (", length(extracted_data_list),
         ") must match the number of plate plan files (", nplates, ").")
  }
  
  # TAG: INITIALIZE STORAGE LISTS
  enriched_data_list         <- list()
  data_with_periods_list     <- list()
  period_boundaries_list     <- list()
  boundary_associations_list <- list()
  zone_data_list             <- list()
  zone_calculated_list       <- list()
  
  # TAG: PREPARE COMMON PARAMETERS
  periods          <- trimws(unlist(strsplit(input_list$period_sequence, ",")))
  boundaries_input <- input_list$period_boundaries
  start_unit       <- input_list$start_column_unit
  remove_time_list          <- input_list$remove_time
  remove_suspect_wells_list <- input_list$remove_suspect_wells
  remove_conditions_list    <- input_list$remove_conditions
  remove_periods_list       <- input_list$remove_periods
  zones                     <- input_list$zones_number
  
  # TAG: MAIN LOOP - PROCESS EACH PLATE
  for (i in seq_len(nplates)) {
    message("-----")
    message(sprintf("process plate %d", i))
    message("-----")
    current_plan <- plate_plans[[i]]
    current_data <- extracted_data_list[[i]]
    
    # --- Step 1: Validate plate plan structure ---
    required_columns <- c("animal", "condition", "plate_id")
    missing_cols      <- setdiff(required_columns, colnames(current_plan))
    if (length(missing_cols) > 0) {
      stop(sprintf("❌ Plate plan %d is missing required columns: %s",
                   i, paste(missing_cols, collapse = ", ")))
    }
    message(sprintf("✔️ Plate plan %d validated.", i))
    clean_id <- function(x) sub("_.*$", "", x)
    
    # --- Step 2: Assign 'condition' based on cleaned animal IDs ---
    current_data$condition <- sapply(current_data$animal, function(a) {
      vals <- current_plan$condition[ clean_id(current_plan$animal) == a ]
      if (length(vals) == 0) NA else vals
    })
    message(sprintf("✔️ Conditions assigned for raw data file #%d.", i))
    
    # --- Step 3: Generate or retrieve 'condition_grouped' ---
    if (!"condition_grouped" %in% names(current_plan)) {
      message(sprintf("🔄️ Generating 'condition_grouped' for Plate plan #%d...", i))
      current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
        if (is.na(cond)) NA else sub("_.*$", "", cond)
      })
      message(sprintf("✔️ 'condition_grouped' generated for Plate plan #%d.", i))
    }
    current_data$condition_grouped <- sapply(current_data$animal, function(a) {
      vals <- current_plan$condition_grouped[ clean_id(current_plan$animal) == a ]
      if (length(vals) == 0) NA else vals
    })
    
    # --- Step 4: Append 'plate_id' ---
    current_data$plate_id <- current_plan$plate_id[1]
    
    # --- Step 5: Generate or retrieve 'condition_tagged' ---
    if (!"condition_tagged" %in% names(current_plan)) {
      message(sprintf("🔄️ Generating 'condition_tagged' for Plate plan #%d...", i))
      current_plan <- current_plan %>%
        group_by(condition_grouped) %>%
        mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", row_number()))) %>%
        ungroup()
      message(sprintf("✔️ 'condition_tagged' generated for Plate plan #%d.", i))
    }
    current_data$condition_tagged <- sapply(current_data$animal, function(a) {
      vals <- current_plan$condition_tagged[ clean_id(current_plan$animal) == a ]
      if (length(vals) == 0) NA else vals
    })
    message(sprintf("✔️ Grouping, tagging, and plate ID columns appended for raw data file #%d.", i))
    enriched_data_list[[i]] <- current_data
    
    # TAG: PERIOD ASSIGNMENT FOR CURRENT PLATE
    message(sprintf("🔄 Assigning periods for Plate %d...", i))
    message("✔️ Universal period sequence: ", paste(periods, collapse = ", "))
    message("✔️ Universal period boundaries (seconds): ", paste(boundaries_input, collapse = ", "))
    
    # Calculate durations and adjust to integration period
    durations <- numeric(length(periods))
    durations[1] <- boundaries_input[1]
    if (length(boundaries_input) > 1) {
      for (j in 2:length(boundaries_input)) {
        durations[j] <- boundaries_input[j] - boundaries_input[j - 1]
      }
    }
    integration_period <- as.numeric(unique(current_data$period))
    if (length(integration_period) > 1) integration_period <- integration_period[1]
    durations[1:length(boundaries_input)] <- pmax(durations[1:length(boundaries_input)], integration_period)
    snapped <- round(cumsum(durations[1:length(boundaries_input)]) / integration_period) * integration_period
    plate_boundaries <- snapped / 60
    message(sprintf("✔️ Plate %d - Adjusted period boundaries (minutes): %s", i, paste(plate_boundaries, collapse = ", ")))
    
    # Build boundary associations
    period_transitions <- paste(periods[-length(periods)], periods[-1], sep = "-")
    boundary_associations <- data.frame(boundary_time = plate_boundaries, transition = period_transitions)
    
    assign_period <- function(start_sec) {
      for (k in seq_along(plate_boundaries)) {
        if (start_sec < plate_boundaries[k] * 60) return(periods[k])
      }
      periods[length(periods)]
    }
    current_data <- current_data %>% mutate(
      period_with_numbers   = sapply(start, assign_period),
      period_without_numbers = case_when(
        str_detect(period_with_numbers, "^light") ~ "light",
        str_detect(period_with_numbers, "^dark")  ~ "dark",
        TRUE ~ period_with_numbers
      )
    )
    message(sprintf("✔️ Plate %d - Periods assigned and simplified.", i))
    data_with_periods_list[[i]]     <- current_data
    period_boundaries_list[[i]]     <- plate_boundaries
    boundary_associations_list[[i]] <- boundary_associations
    
    # --- Apply removal filters ---
    times_to_remove    <- remove_time_list[[i]]
    if (!all(is.na(times_to_remove))) {
      n0 <- nrow(current_data)
      current_data <- current_data %>% filter(! start %in% times_to_remove)
      message(sprintf("✔️ Plate %d – %d row(s) removed by remove_time_list", i, n0 - nrow(current_data)))
    }
    wells_to_remove   <- remove_suspect_wells_list[[i]]
    if (length(wells_to_remove)>0) {
      n0 <- nrow(current_data)
      current_data <- current_data %>% filter(! animal %in% wells_to_remove)
      message(sprintf("✔️ Plate %d – %d well(s) removed by remove_suspect_wells_list", i, n0 - nrow(current_data)))
    }
    conds_to_remove   <- remove_conditions_list[[i]]
    if (length(conds_to_remove)>0) {
      n0 <- nrow(current_data)
      current_data <- current_data %>% filter(! condition %in% conds_to_remove)
      message(sprintf("✔️ Plate %d – %d row(s) removed by remove_conditions_list", i, n0 - nrow(current_data)))
    }
    periods_to_remove <- remove_periods_list[[i]]
    if (length(periods_to_remove)>0) {
      n0 <- nrow(current_data)
      current_data <- current_data %>% filter(! period_with_numbers %in% periods_to_remove)
      message(sprintf("✔️ Plate %d – %d row(s) removed by remove_periods_list", i, n0 - nrow(current_data)))
    }
    
    # TAG: ZONE PROCESSING FOR CURRENT PLATE
    message(sprintf("🔄 Processing zones for Plate %d...", i))
    message("✔️ Zones recorded: ", paste(zones, collapse = ", "))
    
    # Split and calculate zones
    zone_data  <- list()
    for (z in zones) zone_data[[as.character(z)]] <- current_data %>% filter(an == z)
    if (all(c(0,2) %in% zones)) {
      zone_data[["1"]] <- zone_data[["0"]]
      num_cols <- c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur")
      for (col in num_cols) {
        zone_data[["1"]][[col]] <- zone_data[["0"]][[col]] - zone_data[["2"]][[col]]
      }
      message(sprintf("✔️ Zone 1 calculated for Plate %d.", i))
    }
    
    # Clean & combine
    processed_zones <- list()
    for (zn in names(zone_data)) {
      zd <- zone_data[[zn]]
      calc_cols <- c("smldist","lardist","smldur","lardur","smlct","larct")
      zd <- convert_numeric_cols(zd, calc_cols)
      zd <- zd %>% mutate(
        totaldist = ifelse(!is.na(smldist) & !is.na(lardist), smldist + lardist, NA),
        totaldur  = ifelse(!is.na(smldur)  & !is.na(lardur),  smldur  + lardur,  NA),
        totalct   = ifelse(!is.na(smlct)   & !is.na(larct),   smlct   + larct,   NA)
      )
      if (start_unit == "h") zd <- zd %>% mutate(start = as.numeric(start) * 60)
      if (start_unit == "s") zd <- zd %>% mutate(start = as.numeric(start) / 60)
      if (!all(is.na(remove_time_list[[i]]))) zd <- zd %>% filter(!start %in% remove_time_list[[i]])
      zd <- zd %>% mutate(zone = zn)
      desired_cols <- c("plate_id","animal","condition","condition_grouped","condition_tagged",
                        "period","period_with_numbers","period_without_numbers",
                        "zone","start","inact","inadur","inadist","emptyct","emptydur",
                        "smlct","larct","totalct","smldur","lardur","totaldur",
                        "smldist","lardist","totaldist")
      processed_zones[[zn]] <- zd %>% select(any_of(desired_cols))
      message(sprintf("✔️ Zone %s processed for Plate %d.", zn, i))
    }
    zone_combined <- bind_rows(processed_zones)
    zone_calculated_list[[i]] <- list(processed_zones = processed_zones, zone_combined = zone_combined)
    zone_data_list[[i]]       <- zone_data
    message(sprintf("✅ Pairing %d complete.", i))
    message("-----")
  }
  
  message("\n🎉 Data extraction, enrichment, period assignment, and zone processing completed for all plates!")
  assign("enriched_data_df_list",          enriched_data_list,         envir = .GlobalEnv)
  assign("data_with_periods_df_list",     data_with_periods_list,     envir = .GlobalEnv)
  assign("period_boundaries_list",        period_boundaries_list,     envir = .GlobalEnv)
  assign("boundary_associations_list",    boundary_associations_list, envir = .GlobalEnv)
  assign("zone_calculated_list",          zone_calculated_list,       envir = .GlobalEnv)
  
  message("💾 Global data objects created:")
  message("    - 'enriched_data_df_list'")
  message("    - 'data_with_periods_df_list'")
  message("    - 'period_boundaries_list'")
  message("    - 'boundary_associations_list'")
  message("    - 'zone_calculated_list'")
  
  return(enriched_data_list)
}

                  