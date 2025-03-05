# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : light dark mode
# Function: import_and_process_data
# Purpose: Enriches experimental data using a plate plan by matching wells,
#          and, if needed, generating additional grouping/tagging columns.
#          Assumes that the two inputs are lists of data frames where the i-th
#          plate plan corresponds to the i-th raw data file.
#          The enriched data is saved globally as 'enriched_data_df_list'.
# -----------------------------------------------------------
import_and_process_data <- function(data_list, plate_plan_list) {
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  }))
  
  message("\n---\n")
  message("👋 Welcome to the Data Enrichment Process!")
  message("📋 This function will help you:")
  message("   • Match experimental wells (from the 'animal' column) with their conditions.")
  message("   • Generate 'condition_grouped' and 'condition_tagged' columns if missing.")
  message("   • Save the enriched data globally as 'enriched_data_df_list'.\n")
  
  # Ensure that the number of raw data files matches the number of plate plan files
  nplates <- length(plate_plan_list)
  if (length(data_list) != nplates) {
    stop("❌ The number of raw data files (", length(data_list), 
         ") must match the number of plate plan files (", nplates, ").")
  }
  
  enriched_data_list <- list()
  
  # Process each pairing, with detailed messaging
  for (i in seq_len(nplates)) {
    message("-----------------------------------------------------------")
    message(sprintf("🔄 Processing pairing %d:", i))
    message(sprintf("   • Raw data file #%d is being paired with Plate plan #%d.", i, i))
    
    current_plan <- plate_plan_list[[i]]
    current_data <- data_list[[i]]
    
    # Validate that the current plate plan has the required columns
    required_columns <- c("animal", "condition")
    missing_cols <- setdiff(required_columns, colnames(current_plan))
    if (length(missing_cols) > 0) {
      stop(sprintf("❌ Plate plan %d is missing required columns: %s", 
                   i, paste(missing_cols, collapse = ", ")))
    }
    message(sprintf("✔️ Plate plan %d validated; required columns are present.", i))
    
    # Match experimental wells with plate plan conditions
    message(sprintf("🛠️ Assigning conditions to raw data file #%d based on Plate plan #%d...", i, i))
    current_data$condition <- sapply(current_data$animal, function(animal_id) {
      value <- current_plan$condition[current_plan$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    message(sprintf("✔️ Conditions assigned for raw data file #%d.", i))
    
    # Generate 'condition_grouped' if missing
    if (!"condition_grouped" %in% colnames(current_plan)) {
      message(sprintf("🛠️ Generating 'condition_grouped' from 'condition' for Plate plan #%d...", i))
      current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
        if (is.na(cond)) NA else strsplit(cond, "_")[[1]][1]
      })
      message(sprintf("✔️ 'condition_grouped' generated for Plate plan #%d.", i))
    } else {
      message(sprintf("ℹ️ 'condition_grouped' already exists in Plate plan #%d.", i))
    }
    
    # Generate 'condition_tagged' if missing
    if (!"condition_tagged" %in% colnames(current_plan)) {
      message(sprintf("🛠️ Generating 'condition_tagged' based on 'condition_grouped' for Plate plan #%d...", i))
      current_plan <- current_plan %>% 
        group_by(condition_grouped) %>% 
        mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", row_number()))) %>% 
        ungroup()
      message(sprintf("✔️ 'condition_tagged' generated for Plate plan #%d.", i))
    } else {
      message(sprintf("ℹ️ 'condition_tagged' already exists in Plate plan #%d.", i))
    }
    
    # Append new columns to experimental data
    message(sprintf("🛠️ Appending grouping and tagging columns to raw data file #%d...", i))
    current_data$condition_grouped <- sapply(current_data$animal, function(animal_id) {
      value <- current_plan$condition_grouped[current_plan$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    current_data$condition_tagged <- sapply(current_data$animal, function(animal_id) {
      value <- current_plan$condition_tagged[current_plan$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    message(sprintf("✔️ Grouping and tagging columns appended for raw data file #%d.", i))
    
    enriched_data_list[[i]] <- current_data
    message(sprintf("✅ Pairing %d complete: Plate plan #%d has been successfully assigned to raw data file #%d.", i, i, i))
  }
  
  message("\n🎉 Data enrichment completed for all plate pairings!")
  message("💾 Enriched data saved globally as 'enriched_data_df_list'.\n")
  assign("enriched_data_df_list", enriched_data_list, envir = .GlobalEnv)
  return(enriched_data_list)
}
