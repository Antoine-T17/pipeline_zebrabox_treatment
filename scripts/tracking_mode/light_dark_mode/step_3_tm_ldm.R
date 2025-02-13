# -----------------------------------------------------------
# import_and_process_data.R
# -----------------------------------------------------------
# This function enriches the raw experimental data with
# additional information from the plate plan. It performs the following tasks:
#
#   1. Validates the plate plan to ensure that the required columns 
#      ("animal" and "condition") are present.
#   2. Matches each experimental well (from the 'animal' column) to 
#      its assigned condition from the plate plan.
#   3. Dynamically generates 'condition_grouped' (if missing) by 
#      splitting the 'condition' string.
#   4. Dynamically generates 'condition_tagged' (if missing) by grouping 
#      by 'condition_grouped' and appending a row number.
#   5. Appends 'condition_grouped' and 'condition_tagged' to the experimental data.
#   6. Saves the enriched data globally as 'enriched_data_df'.
#
# Usage:
#   enriched_data <- import_and_process_data(data, plate_plan)
#
# -----------------------------------------------------------

import_and_process_data <- function(data, plate_plan) {
  # Suppress warnings and package startup messages for clarity.
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  }))
  
  message("\n---\n---\n---\n")
  message("👋 Welcome to the Data Enrichment Process!\n")
  message("This function assists you with:")
  message("  🔗 Matching experimental wells (from the 'animal' column) to their assigned conditions.")
  message("  🛠️ Generating 'condition_grouped' and 'condition_tagged' if missing in the plate plan.\n")
  
  # Step 1: Validate the plate plan.
  message("🔍 Validating the provided plate plan...")
  required_columns <- c("animal", "condition")
  missing_cols <- setdiff(required_columns, colnames(plate_plan))
  if (length(missing_cols) > 0) {
    stop("❌ The following required columns are missing in the plate plan: ",
         paste(missing_cols, collapse = ", "))
  }
  message("✔️ Plate plan validation successful.\n")
  
  # Step 2: Enrich the experimental data with plate plan information.
  message("📂 Enriching experimental data with plate plan conditions...")
  data$condition <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  message("✔️ Conditions successfully matched to experimental data.\n")
  
  # Step 3: Dynamically generate 'condition_grouped' if missing.
  if (!"condition_grouped" %in% colnames(plate_plan)) {
    message("🔧 Generating 'condition_grouped' dynamically from 'condition'...")
    plate_plan$condition_grouped <- sapply(plate_plan$condition, function(cond) {
      if (is.na(cond)) return(NA)
      # Split the condition string by "_" and take the first part.
      str_split(cond, "_")[[1]][1]
    })
    message("✔️ 'condition_grouped' generated successfully.\n")
  }
  
  # Step 4: Dynamically generate 'condition_tagged' if missing.
  if (!"condition_tagged" %in% colnames(plate_plan)) {
    message("🔧 Generating 'condition_tagged' dynamically based on 'condition_grouped'...")
    plate_plan <- plate_plan %>%
      group_by(condition_grouped) %>%
      mutate(condition_tagged = ifelse(
        condition == "X", 
        "X",
        paste0(condition_grouped, "_", row_number())
      )) %>%
      ungroup()
    message("✔️ 'condition_tagged' generated successfully.\n")
  }
  
  # Step 5: Append 'condition_grouped' and 'condition_tagged' to the experimental data.
  message("🔄 Adding 'condition_grouped' and 'condition_tagged' to the experimental data...")
  data$condition_grouped <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_grouped[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  
  data$condition_tagged <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_tagged[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  
  # Step 6: Save the enriched data globally.
  message("\n🎉 Data enrichment completed successfully!")
  message("💾 The enriched data has been saved globally as 'enriched_data_df'.\n")
  assign("enriched_data_df", data, envir = .GlobalEnv)
  
  return(data)
}
