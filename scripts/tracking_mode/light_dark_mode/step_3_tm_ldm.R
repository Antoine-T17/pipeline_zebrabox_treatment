import_and_process_data <- function(data_list, plate_plan) {
  # Load necessary libraries quietly
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  }))
  
  message("\n---\n")
  message("👋 Welcome to the Data Enrichment Process!")
  message("📋 This function will help you:")
  message("   • Match experimental wells (from the 'animal' column) with their conditions.")
  message("   • Generate 'condition_grouped' and 'condition_tagged' columns if missing.")
  message("   • Save each enriched data frame in a list globally as 'enriched_data_df'.\n")
  
  # Validate that the plate plan contains the required columns
  message("🔍 Validating the plate plan...")
  required_columns <- c("animal", "condition")
  missing_cols <- setdiff(required_columns, colnames(plate_plan))
  if (length(missing_cols) > 0) {
    stop("❌ Plate plan missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  message("✔️ Plate plan validation successful.")
  
  enriched_data_list <- list()
  
  # Get the unique plate IDs from the plate plan in the order they were loaded.
  unique_plate_ids <- unique(plate_plan$plate_id)
  
  # Process each data frame in the list using the corresponding plate plan
  for (i in seq_along(data_list)) {
    message("\n--- Processing Raw Data for Plate ", i, " ---")
    df <- data_list[[i]]
    
    # Determine the actual plate_id from the plate plan using the order.
    if(i > length(unique_plate_ids)) {
      stop("❌ There are more raw data files than available plate plan IDs.")
    }
    actual_plate_id <- unique_plate_ids[i]
    message("🔍 Linking raw data to plate_id: ", actual_plate_id)
    
    # Update the raw data frame's plate_id to match the plate plan's id.
    df$plate_id <- actual_plate_id
    
    # Filter the plate plan for this specific plate
    plate_plan_i <- plate_plan[plate_plan$plate_id == actual_plate_id, ]
    if (nrow(plate_plan_i) == 0) {
      stop("❌ No plate plan rows found for plate_id: ", actual_plate_id)
    }
    
    # Step 4: Match experimental wells with plate plan conditions
    message("🛠️ Matching experimental wells with plate plan conditions...")
    df$condition <- sapply(df$animal, function(animal_id) {
      value <- plate_plan_i$condition[plate_plan_i$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    message("✔️ Conditions successfully matched.")
    
    # Step 5: Generate 'condition_grouped' in the plate plan if missing
    if (!"condition_grouped" %in% colnames(plate_plan_i)) {
      message("🛠️ Generating 'condition_grouped' from 'condition' for plate_id ", actual_plate_id, "...")
      plate_plan_i$condition_grouped <- sapply(plate_plan_i$condition, function(cond) {
        if (is.na(cond)) NA else strsplit(cond, "_")[[1]][1]
      })
      message("✔️ 'condition_grouped' generated.")
    }
    
    # Step 6: Generate 'condition_tagged' in the plate plan if missing
    if (!"condition_tagged" %in% colnames(plate_plan_i)) {
      message("🛠️ Generating 'condition_tagged' based on 'condition_grouped' for plate_id ", actual_plate_id, "...")
      plate_plan_i <- plate_plan_i %>%
        group_by(condition_grouped) %>%
        mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", row_number()))) %>%
        ungroup()
      message("✔️ 'condition_tagged' generated.")
    }
    
    # Step 7: Append new columns to experimental data from the plate plan
    message("🛠️ Appending 'condition_grouped' and 'condition_tagged' to experimental data for plate_id ", actual_plate_id, "...")
    df$condition_grouped <- sapply(df$animal, function(animal_id) {
      value <- plate_plan_i$condition_grouped[plate_plan_i$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    df$condition_tagged <- sapply(df$animal, function(animal_id) {
      value <- plate_plan_i$condition_tagged[plate_plan_i$animal == animal_id]
      if (length(value) == 0) NA else value
    })
    
    # Save the enriched data frame into the list
    enriched_data_list[[i]] <- df
    message("✔️ Enrichment for plate_id ", actual_plate_id, " completed.")
  }
  
  message("\n🎉 Data enrichment completed for all plates!")
  message("💾 Enriched data saved globally as 'enriched_data_df'.\n")
  assign("enriched_data_df", enriched_data_list, envir = .GlobalEnv)
  return(enriched_data_list)
}
