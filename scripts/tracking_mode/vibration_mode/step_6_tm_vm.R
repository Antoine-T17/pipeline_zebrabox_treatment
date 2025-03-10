# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : vibration mode
# Function: calculate_and_clean_zone_data
# Purpose: Processes each zone’s data by computing new variables,
#          converting the 'start' column to minutes (using a universal unit),
#          removing rows whose 'start' time equals any specified removal times (per plate),
#          reordering columns, and combining all zones into a single dataset for each plate.
#          The final result is saved globally as 'zone_calculated_list'.
#          (Adapted for vibration mode from the light-dark version:
#           - No automatic removal of the last minute.
#           - Combination of zones and nomenclature identical to the light-dark version.)
# -----------------------------------------------------------
calculate_and_clean_zone_data <- function(zone_data_list) {
  message("\n---\n")
  message("👋 Welcome to the Zone Calculation and Cleaning Process! (Vibration Mode)")
  message("📋 This function will help you:")
  message("   • Process and clean zone-specific data.")
  message("   • Calculate new numeric variables and convert 'start' time to minutes.")
  message("   • Remove rows whose 'start' time equals any specified removal thresholds (per plate).")
  message("   • Reorder columns and combine all zones into a single dataset for each plate.")
  message("   • Save the final results globally as 'zone_calculated_list'.\n")
  
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Helper function: forces manual prompt mode if input is empty.
  get_input_local <- function(param, prompt_msg, validate_fn, transform_fn,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(as.character(pipeline_inputs[[param]]))) {
        message("💾 Using pre-recorded input for '", param, "': ", as.character(pipeline_inputs[[param]]))
        input_record_list[[param]] <<- as.character(pipeline_inputs[[param]])
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      if (user_input == "") {
        message("⚠️ Empty input detected. Please enter a valid value.")
        next
      }
      if (validate_fn(user_input)) {
        candidate <- transform_fn(user_input)
        message("✔️ Input for '", param, "' recorded: ", user_input)
        input_record_list[[param]] <<- user_input
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  # Step 1: Get universal start unit.
  start_unit <- get_input_local("start_column_unit",
                                "❓ What is the unit of the 'start' column? (h for hours, m for minutes, s for seconds): ",
                                validate_fn = function(x) {
                                  x <- trimws(x)
                                  tolower(x) %in% c("h", "m", "s")
                                },
                                transform_fn = function(x) tolower(trimws(x)),
                                error_msg = "❌ Please enter 'h', 'm', or 's'.")
  
  # Step 2: Prompt for remove_time thresholds per plate.
  n_plates <- length(zone_data_list)
  remove_time_input <- get_input_local("remove_time",
                                       sprintf("❓ Enter the remove_time threshold(s) (in minutes) for %d plate(s), separated by ';' (e.g., 1,6,27 ; no): ", n_plates),
                                       validate_fn = function(x) {
                                         tokens <- trimws(unlist(strsplit(x, ";")))
                                         if (length(tokens) != n_plates) return(FALSE)
                                         for (tok in tokens) {
                                           if (tolower(tok) == "no") {
                                             next
                                           } else {
                                             sub_tokens <- trimws(unlist(strsplit(tok, ",")))
                                             if (length(sub_tokens) < 1) return(FALSE)
                                             for (st in sub_tokens) {
                                               if (is.na(as.numeric(st)) || as.numeric(st) <= 0) {
                                                 return(FALSE)
                                               }
                                             }
                                           }
                                         }
                                         return(TRUE)
                                       },
                                       transform_fn = function(x) {
                                         tokens <- trimws(unlist(strsplit(x, ";")))
                                         lapply(tokens, function(tok) {
                                           if (tolower(tok) == "no") {
                                             NA  # NA means no removal for that plate.
                                           } else {
                                             as.numeric(trimws(unlist(strsplit(tok, ","))))
                                           }
                                         })
                                       },
                                       error_msg = sprintf("❌ Please enter exactly %d value(s) (a positive number or a comma-separated list of positive numbers, or 'no') separated by ';'.", n_plates)
  )
  
  for (i in seq_along(remove_time_input)) {
    if (all(is.na(remove_time_input[[i]]))) {
      message(sprintf("ℹ️ For plate %d, no removal threshold is set.", i))
    } else {
      message(sprintf("✔️ For plate %d, remove_time thresholds set to: %s minutes.",
                      i, paste(remove_time_input[[i]], collapse = ", ")))
    }
  }
  
  zone_calculated_list <- list()
  
  # Process each plate's zone data.
  for (i in seq_along(zone_data_list)) {
    message("\n🛠️ Processing zone data for plate ", i)
    plate_zone_data <- zone_data_list[[i]]
    processed_zones <- list()
    
    for (zone_name in names(plate_zone_data)) {
      message("🛠️ Processing Zone: ", zone_name, " for plate ", i)
      zone_data <- plate_zone_data[[zone_name]]
      message("✔️ Zone data loaded.")
      
      message("🛠️ Calculating new variables...")
      numeric_columns <- c("smldist", "lardist", "smldur", "lardur", "smlct", "larct")
      for (col in numeric_columns) {
        if (col %in% colnames(zone_data)) {
          zone_data[[col]] <- as.numeric(gsub(",", ".", as.character(zone_data[[col]])))
        }
      }
      zone_data <- zone_data %>% 
        mutate(
          totaldist = ifelse(!is.na(smldist) & !is.na(lardist), smldist + lardist, NA),
          totaldur  = ifelse(!is.na(smldur) & !is.na(lardur), smldur + lardur, NA),
          totalct   = ifelse(!is.na(smlct) & !is.na(larct), smlct + larct, NA)
        )
      message("✔️ New variables calculated.")
      
      message("🛠️ Converting 'start' column to minutes...")
      if ("start" %in% colnames(zone_data)) {
        zone_data$start <- as.numeric(gsub(",", ".", as.character(zone_data$start)))
        if (start_unit == "h") {
          zone_data <- zone_data %>% mutate(start = start * 60)
          message("✔️ 'start' converted from hours to minutes.")
        } else if (start_unit == "s") {
          zone_data <- zone_data %>% mutate(start = start / 60)
          message("✔️ 'start' converted from seconds to minutes.")
        } else {
          message("✔️ 'start' assumed to be in minutes; no conversion done.")
        }
      } else {
        message("⚠️ 'start' column not found; skipping conversion.")
      }
      
      # Remove rows based on remove_time thresholds.
      current_remove_times <- remove_time_input[[i]]
      if (all(is.na(current_remove_times))) {
        message("ℹ️ No removal thresholds set for plate ", i, "; no rows will be removed.")
      } else {
        message("🛠️ Removing rows for plate ", i, " where 'start' equals any of: ", 
                paste(current_remove_times, collapse = ", "), " minutes...")
        if ("start" %in% colnames(zone_data)) {
          zone_data <- zone_data %>% filter(!(start %in% current_remove_times))
          message("✔️ Specified rows removed for plate ", i, ".")
        } else {
          message("⚠️ 'start' column missing; skipping row removal.")
        }
      }
      
      message("🛠️ Adding 'zone' column...")
      zone_data <- zone_data %>% mutate(zone = zone_name) %>% relocate(zone, .after = period_without_numbers)
      message("✔️ 'zone' column added.")
      
      message("🛠️ Filtering and reordering columns...")
      desired_columns <- c("animal", "condition", "condition_grouped", "condition_tagged",
                           "period", "period_with_numbers", "period_without_numbers",
                           "zone", "start", 
                           "inact", "inadur", "inadist", "emptyct", "emptydur",
                           "smlct", "larct", "totalct",
                           "smldur", "lardur", "totaldur",
                           "smldist", "lardist", "totaldist")
      zone_data <- zone_data %>% select(any_of(desired_columns))
      message("✔️ Columns filtered and reordered.")
      
      processed_zones[[zone_name]] <- zone_data
    }
    
    message("🛠️ Combining processed zone data for plate ", i, "...")
    zone_combined <- dplyr::bind_rows(processed_zones)
    message("✔️ Zones combined successfully for plate ", i, ".")
    
    zone_calculated_list[[i]] <- list(processed_zones = processed_zones, zone_combined = zone_combined)
  }
  
  message("🎉 Zone calculation and cleaning completed!")
  message("💾 Processed zone data saved globally as 'zone_calculated_list'.\n")
  assign("zone_calculated_list", zone_calculated_list, envir = .GlobalEnv)
  return(zone_calculated_list)
}
