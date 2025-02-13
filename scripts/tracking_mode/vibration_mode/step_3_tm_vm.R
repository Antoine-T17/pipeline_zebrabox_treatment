import_and_process_data <- function(data, plate_plan) {
  
  message("\n---\n---\n---\n")
  
  # Welcome message
  message("\n👋 Welcome to the Data Enrichment Process!\n")
  message("This function assists you with:\n")
  message("🔗 Matching experimental wells (from the 'animal' column) to their assigned conditions.")
  message("🛠️ Dynamically generating 'condition_grouped' and 'condition_tagged' if missing in the plate plan.\n")
  
  # Step 1: Validate the plate plan
  message("🔍 Validating the provided plate plan...")
  required_columns <- c("animal", "condition")
  missing_cols <- setdiff(required_columns, colnames(plate_plan))
  if (length(missing_cols) > 0) {
    stop("❌ The following required columns are missing in the plate plan: ",
         paste(missing_cols, collapse = ", "))
  }
  message("✔️ Plate plan validation successful.")
  
  # Step 2: Enrich the experimental data with plate plan information
  message("📂 Enriching experimental data with plate plan information...")
  data$condition <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  message("✔️ Conditions successfully matched to the experimental data.")
  
  # Step 3: Dynamically generate 'condition_grouped' if missing
  if (!"condition_grouped" %in% colnames(plate_plan)) {
    message("🔧 Generating 'condition_grouped' dynamically from 'condition'...")
    plate_plan$condition_grouped <- sapply(plate_plan$condition, function(cond) {
      if (is.na(cond)) return(NA)
      str_split(cond, "_")[[1]][1]
    })
    message("✔️ 'Condition_grouped' successfully generated.")
  }
  
  # Step 4: Dynamically generate 'condition_tagged' if missing
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
    message("✔️ 'Condition_tagged' successfully generated.")
  }
  
  # Step 5: Add 'condition_grouped' and 'condition_tagged' to the experimental data
  data$condition_grouped <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_grouped[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  
  data$condition_tagged <- sapply(data$animal, function(animal_id) {
    value <- plate_plan$condition_tagged[plate_plan$animal == animal_id]
    if (length(value) == 0) return(NA) else return(value)
  })
  
  message("🎉 Data enrichment completed successfully!")
  message("💾 The enriched data has been saved in the global environment as 'enriched_data_df'.\n")
  
  # Step 6: Save enriched data to the global environment
  assign("enriched_data_df", data, envir = .GlobalEnv)
  
  return(data)
}
