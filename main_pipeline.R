# Load the pipeline script which, in turn, loads all other scripts including step 13.
source("scripts/final_step_run_full_pipeline.R")

# Now run the pipeline
results_final <- run_full_pipeline()    

test = 100