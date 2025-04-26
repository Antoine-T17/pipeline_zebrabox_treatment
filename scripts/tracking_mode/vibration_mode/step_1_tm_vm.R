generate_plate_plan <- function(plan_dir = "inputs/tracking_mode/vibration_mode/plate_plan") {
  # STEP 1 — ENSURE DIRECTORY
  if (!dir.exists(plan_dir)) {
    dir.create(plan_dir, recursive = TRUE)
    message("✔️ Directory created: ", plan_dir)
  }
  
  # STEP 2 — WELCOME MESSAGE
  message("\n---\n")
  message("👋 Welcome to the Plate Plan Generator! \n")
  message("📋 This function will help you:")
  message("   • Create a new plate plan or load existing one(s).")
  message("   • Randomize well assignments for new plans (with or without border wells).")
  message("   • Work with both '.csv' and '.xlsx' file formats.")
  message("   • Save the plate plan(s) for later use.\n")
  
  # STEP 3 — RETRIEVE PIPELINE INPUTS
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  input_record_list <<- list()
  
  # HELPER: INPUT HANDLER
  get_input_local <- function(param, prompt_msg,
                              validate_fn = function(x) TRUE,
                              transform_fn  = function(x) x,
                              error_msg    = "❌ Invalid input. Please try again.") {
    if (!is.null(input_record_list[[param]])) {
      message("💾 Using cached input for '", param, "'.")
      return(input_record_list[[param]])
    }
    if (!is.null(pipeline_inputs[[param]]) &&
        !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      raw       <- pipeline_inputs[[param]]
      candidate <- transform_fn(raw)
      if (validate_fn(raw)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("💾 Using pre-recorded input for '", param, "': ",
                  paste(candidate, collapse = ", "))
        } else {
          message("💾 Using pre-recorded input for '", param, "': ", candidate)
        }
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param,
                "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate  <- transform_fn(user_input)
      if (validate_fn(user_input)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("✔️ Input for '", param, "' recorded: ",
                  paste(candidate, collapse = ", "))
        } else {
          message("✔️ Input for '", param, "' recorded: ", candidate)
        }
        return(candidate)
      }
      message(error_msg)
    }
  }
  
  # INPUT DEFS
  input_defs <- list(
    create_plate_plan = list(
      prompt = "❓ Create new plate plan? (yes/no): ",
      validate = function(x) tolower(x) %in% c("yes","y","no","n"),
      transform = function(x) tolower(trimws(x)),
      error = "❌ Please enter 'yes' or 'no'."
    ),
    plate_number        = list(prompt="❓ Number of plates to generate: ", validate=function(x)!is.na(as.integer(x))&&as.integer(x)>0, transform=as.integer, error="❌ Enter a positive integer."),
    plate_type          = list(prompt="❓ Plate type (12,24,48,96 wells): ", validate=function(x)!is.na(as.integer(x))&&as.integer(x)%in%c(12,24,48,96), transform=as.integer, error="❌ Enter 12,24,48, or 96."),
    conditions_number   = list(prompt="❓ Number of conditions: ", validate=function(x)!is.na(as.integer(x))&&as.integer(x)>0, transform=as.integer, error="❌ Enter a positive integer."),
    conditions_name     = list(prompt=NULL, validate=function(x)TRUE, transform=function(x)x, error=""),
    replicates_number   = list(prompt="❓ Replicates per condition: ", validate=function(x)!is.na(as.integer(x))&&as.integer(x)>0, transform=as.integer, error="❌ Enter a positive integer."),
    units_per_replicate = list(prompt="❓ Units per replicate: ", validate=function(x)!is.na(as.integer(x))&&as.integer(x)>0, transform=as.integer, error="❌ Enter a positive integer."),
    keep_border_wells   = list(prompt="❓ Include border wells? (yes/no): ", validate=function(x)tolower(x)%in%c("yes","y","no","n"), transform=function(x)tolower(trimws(x)), error="❌ Enter 'yes' or 'no'."),
    seed_value          = list(prompt="❓ Seed for randomization: ", validate=function(x)!is.na(as.integer(x)), transform=as.integer, error="❌ Enter an integer."),
    plate_plan_name_csv = list(prompt="❓ Base CSV file name: ", validate=function(x)nzchar(trimws(x)), transform=trimws, error="❌ Invalid name."),
    plate_plan_name_xlsx= list(prompt="❓ Base Excel file name: ", validate=function(x)nzchar(trimws(x)), transform=trimws, error="❌ Invalid name."),
    plate_plan_name     = list(prompt="❓ Existing file name(s) separated by ';': ", validate=function(x)TRUE, transform=trimws, error="❌ Invalid file name(s).")
  )
  
  # Collect initial choice
  mode_create <- get_input_local("create_plate_plan", input_defs$create_plate_plan$prompt,
                                 input_defs$create_plate_plan$validate,
                                 input_defs$create_plate_plan$transform,
                                 input_defs$create_plate_plan$error)
  plate_plan_list <- list()
  
  if (mode_create %in% c("yes","y")) {
    # Gather inputs
    plate_type        <- get_input_local("plate_type", input_defs$plate_type$prompt,
                                         input_defs$plate_type$validate,
                                         input_defs$plate_type$transform,
                                         input_defs$plate_type$error)
    cond_n            <- get_input_local("conditions_number", input_defs$conditions_number$prompt,
                                         input_defs$conditions_number$validate,
                                         input_defs$conditions_number$transform,
                                         input_defs$conditions_number$error)
    input_defs$conditions_name$prompt <- sprintf("❓ Names of %d conditions (comma-separated): ", cond_n)
    input_defs$conditions_name$validate <- function(x) {
      conds <- trimws(strsplit(x, ",")[[1]])
      length(conds)==cond_n && length(unique(conds))==cond_n
    }
    input_defs$conditions_name$error <- sprintf("❌ Provide %d unique names.", cond_n)
    cond_names        <- strsplit(get_input_local("conditions_name", input_defs$conditions_name$prompt,
                                                  input_defs$conditions_name$validate,
                                                  input_defs$conditions_name$transform,
                                                  input_defs$conditions_name$error), ",")[[1]] |> trimws()
    repl_n            <- get_input_local("replicates_number", input_defs$replicates_number$prompt,
                                         input_defs$replicates_number$validate,
                                         input_defs$replicates_number$transform,
                                         input_defs$replicates_number$error)
    units_n           <- get_input_local("units_per_replicate", input_defs$units_per_replicate$prompt,
                                         input_defs$units_per_replicate$validate,
                                         input_defs$units_per_replicate$transform,
                                         input_defs$units_per_replicate$error)
    plate_number      <- get_input_local("plate_number", input_defs$plate_number$prompt,
                                         input_defs$plate_number$validate,
                                         input_defs$plate_number$transform,
                                         input_defs$plate_number$error)
    border_pref       <- get_input_local("keep_border_wells", input_defs$keep_border_wells$prompt,
                                         input_defs$keep_border_wells$validate,
                                         input_defs$keep_border_wells$transform,
                                         input_defs$keep_border_wells$error)
    
    # Define plate grid (supports 12, 24, 48, and 96 wells)
    rows <- LETTERS[1:ifelse(plate_type == 12, 4,
                             ifelse(plate_type == 24, 4,
                                    ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:ifelse(plate_type == 12, 3,
                     ifelse(plate_type == 24, 6,
                            ifelse(plate_type == 48, 8, 12)))
    wells_template <- with(expand.grid(Row = rows, Column = cols),
                           paste0(Row, sprintf("%02d", Column)))
    total_wells <- length(wells_template)
    
    # Explicit border wells for each plate format
    if (plate_type == 12) {
      explicit_border <- c(
        "A01","A02","A03",
        "B01","B03",
        "C01","C03",
        "D01","D02","D03"
      )
    } else if (plate_type == 24) {
      explicit_border <- c(
        "A01","A02","A03","A04","A05","A06",
        "B01","B06",
        "C01","C06",
        "D01","D02","D03","D04","D05","D06"
      )
    } else if (plate_type == 48) {
      explicit_border <- c(
        "A01","A02","A03","A04","A05","A06","A07","A08",
        "B01","B08",
        "C01","C08",
        "D01","D08",
        "E01","E08",
        "F01","F02","F03","F04","F05","F06","F07","F08"
      )
    } else if (plate_type == 96) {
      explicit_border <- c(
        paste0("A", sprintf("%02d", 1:12)),
        paste0(rep(c("B","C","D","E","F","G"), each = 2),
               sprintf("%02d", c(1,12))),
        paste0("H", sprintf("%02d", 1:12))
      )
    }
    
    # Determine available wells based on keep_border_wells preference
    avail_idx <- if (border_pref %in% c("no", "n")) {
      seq_along(wells_template)[!wells_template %in% explicit_border]
    } else {
      seq_along(wells_template)
    }
    available_per_plate <- length(avail_idx)
    available_total     <- available_per_plate * plate_number
    
    # Validate capacity
    total_units <- cond_n * repl_n * units_n
    if (total_units > available_total) {
      stop(sprintf("❌ Total units (%d) exceed available wells (%d)",
                   total_units, available_total))
    }
    
    # Seed
    seed_val <- get_input_local("seed_value", input_defs$seed_value$prompt,
                                input_defs$seed_value$validate,
                                input_defs$seed_value$transform,
                                input_defs$seed_value$error)
    set.seed(seed_val)
    
    # File names
    base_csv  <- get_input_local("plate_plan_name_csv", input_defs$plate_plan_name_csv$prompt,
                                 input_defs$plate_plan_name_csv$validate,
                                 input_defs$plate_plan_name_csv$transform,
                                 input_defs$plate_plan_name_csv$error)
    base_xlsx <- get_input_local("plate_plan_name_xlsx", input_defs$plate_plan_name_xlsx$prompt,
                                 input_defs$plate_plan_name_xlsx$validate,
                                 input_defs$plate_plan_name_xlsx$transform,
                                 input_defs$plate_plan_name_xlsx$error)
    
    # Distribute conditions uniformly across plates
    cond_counts <- rep(repl_n * units_n, length(cond_names))
    names(cond_counts) <- cond_names
    cond_by_plate <- lapply(cond_names, function(cond) {
      total_c <- cond_counts[cond]
      base <- floor(total_c/plate_number)
      rem  <- total_c %% plate_number
      counts <- rep(base, plate_number)
      if (rem>0) counts[seq_len(rem)] <- counts[seq_len(rem)] + 1
      counts
    })
    names(cond_by_plate) <- cond_names
    
    # Build and save each plate
    for (i in seq_len(plate_number)) {
      message(sprintf("----- PROCESS PLATE %d -----", i))
      # Prepare assignments for this plate
      # Generate labels with replicate suffixes
      this_labels <- unlist(lapply(names(cond_by_plate), function(cond) {
        # total units of this condition on plate i
        total_u    <- cond_by_plate[[cond]][i]
        # base count per replicate and remainder
        base       <- floor(total_u / repl_n)
        rem        <- total_u %% repl_n
        rep_counts <- rep(base, repl_n)
        if (rem > 0) rep_counts[seq_len(rem)] <- rep_counts[seq_len(rem)] + 1
        # for each replicate r, repeat "cond_r" rep_counts[r] times
        unlist(mapply(function(r, cnt) 
          rep(paste0(cond, "_", r), cnt),
          seq_len(repl_n), rep_counts,
          SIMPLIFY = FALSE))
      }))
      
      # Fill plate vector
      plate_assign <- rep("X", total_wells)
      # Randomly assign into available wells
      choice_idx <- sample(avail_idx, length(this_labels))
      plate_assign[choice_idx] <- this_labels
      
      # Assemble df
      df <- data.frame(
        animal    = paste0(wells_template, "_plate_", i),
        condition = plate_assign,
        plate_id  = i,
        stringsAsFactors = FALSE
      )
      
      # Save files
      csv_path  <- file.path(plan_dir, sprintf("%s_plate_%d.csv", base_csv, i))
      write.csv2(df, file=csv_path, row.names=FALSE)
      message("💾 Saved CSV: ", csv_path)
      xlsx_path <- file.path(plan_dir, sprintf("%s_plate_%d.xlsx", base_xlsx, i))
      openxlsx::write.xlsx(df, file=xlsx_path, rowNames=FALSE)
      message("💾 Saved XLSX: ", xlsx_path)
      
      plate_plan_list[[i]] <- df
    }
    message("🎉 New plate plan(s) created successfully!")
    
  } else {
    # LOAD MODE
    files <- strsplit(get_input_local("plate_plan_name", input_defs$plate_plan_name$prompt,
                                      input_defs$plate_plan_name$validate,
                                      input_defs$plate_plan_name$transform,
                                      input_defs$plate_plan_name$error), ";")[[1]] |> trimws()
    for (f in files) {
      message(sprintf("----- LOADING FILE: %s -----", f))
      ext  <- tolower(tools::file_ext(f))
      path <- file.path(plan_dir, f)
      df   <- tryCatch({
        if (ext=="csv") read.csv2(path, sep=";", dec=".", stringsAsFactors=FALSE)
        else readxl::read_excel(path)
      }, error=function(e) stop(sprintf("❌ Error loading %s: %s", f, e$message)))
      if (!all(c("animal","condition") %in% names(df))) stop(
        sprintf("❌ File %s missing required columns.", f)
      )
      message(sprintf("✔️ Plate plan %s loaded.", f))
      # overwrite canonical format
      if (ext=="csv") write.csv2(df, path, row.names=FALSE)
      else openxlsx::write.xlsx(df, path, rowNames=FALSE)
      plate_plan_list[[length(plate_plan_list)+1]] <- df
    }
  }
  
  # FINALIZE
  assign("plate_plan_df_list", plate_plan_list, envir=.GlobalEnv)
  message("🎉 Plate plan generation completed! Outputs in 'plate_plan_df_list'.\n")
  return(plate_plan_list)
}
