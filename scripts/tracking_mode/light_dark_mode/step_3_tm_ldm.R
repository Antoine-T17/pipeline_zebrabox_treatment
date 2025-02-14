# -----------------------------------------------------------
# File: import_and_process_data.R
# -----------------------------------------------------------
# Harmonized version of the import_and_process_data function for light dark mode.
# This function enriches experimental data using a plate plan by matching wells,
# and if needed, generating additional grouping/tagging columns.
# The enriched data is saved globally as 'enriched_data_df'.
# -----------------------------------------------------------

import_and_process_data <- function(data, plate_plan) {
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  }))
  
  message("\n---\n")
  message("👋 Welcome to the Data Enrichment Process!\n")
  message("📋 This function will help you:")
  message("   • Match experimental wells (from the 'animal' column) with their conditions.")
  message("   • Generate 'condition_grouped' and 'condition_tagged' columns if missing.")
  message("   • Save the enriched data globally as 'enriched_data_df'.\n")
  
  # Validate plate plan.
  message("🔍 Validating the plate plan...")
  required_columns <- c("animal", "condition")
  missing_cols <- setdiff(required_columns, colnames(plate_plan))
  if (length(missing_cols) > 0) {
    stop("❌ Plate plan missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  message("✔️ Plate plan validation successful.")
  
  # Enrich experimental data.
  message("🛠️ Matching experimental wells with plate plan conditions...")
  data$condition <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition[plate_plan$animal == animal_id]
    if (length(value) == 0) NA else value
  })
  message("✔️ Conditions successfully matched.")
  
  # Generate condition_grouped if missing.
  if (!"condition_grouped" %in% colnames(plate_plan)) {
    message("🛠️ Generating 'condition_grouped' from 'condition'...")
    plate_plan$condition_grouped <- sapply(plate_plan$condition, function(cond) {
      if (is.na(cond)) NA else strsplit(cond, "_")[[1]][1]
    })
    message("✔️ 'condition_grouped' generated.")
  }
  
  # Generate condition_tagged if missing.
  if (!"condition_tagged" %in% colnames(plate_plan)) {
    message("🛠️ Generating 'condition_tagged' based on 'condition_grouped'...")
    plate_plan <- plate_plan %>%
      group_by(condition_grouped) %>%
      mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", row_number()))) %>%
      ungroup()
    message("✔️ 'condition_tagged' generated.")
  }
  
  # Append the new columns to data.
  message("🛠️ Appending 'condition_grouped' and 'condition_tagged' to experimental data...")
  data$condition_grouped <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_grouped[plate_plan$animal == animal_id]
    if (length(value) == 0) NA else value
  })
  data$condition_tagged <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_tagged[plate_plan$animal == animal_id]
    if (length(value) == 0) NA else value
  })
  
  message("🎉 Data enrichment completed!")
  message("💾 Enriched data saved globally as 'enriched_data_df'.\n")
  assign("enriched_data_df", data, envir = .GlobalEnv)
  return(data)
}
