# -----------------------------------------------------------
# generate_and_save_boxplots_delta_with_excel_files.R
# -----------------------------------------------------------
# This function generates and saves delta boxplots for your experimental data,
# as well as writes pairwise percentage differences to an Excel file.
# It performs the following tasks:
#
#   1. Prompts whether to generate delta boxplots.
#   2. Validates the input data structure.
#   3. Orders conditions based on generated_condition_grouped_order.
#   4. Defines output directories for PNG, HTML, and Excel files.
#   5. Manages colors by prompting for custom colors.
#   6. Defines light and dark themes.
#   7. Prompts for the desired output formats (HTML and PNG).
#   8. Generates delta boxplots (if requested) for each response variable, zone, and theme.
#   9. Calculates pairwise percentage differences and writes the results to an Excel file.
#
# All used inputs are recorded in the global list 'input_record_list'
# (which should be initialized in your main script).
# -----------------------------------------------------------

generate_and_save_boxplots_delta_with_excel_files <- function(
    input_data = get("pretreated_delta_data_for_boxplots_df", envir = .GlobalEnv),
    output_dir = "outputs/tracking_mode/light_dark_mode/figures/boxplots",
    excel_output_dir = "outputs/tracking_mode/light_dark_mode/tables"
) {
  message("\n---\n---\n---\n")
  message("\n👋 Welcome to the Delta Boxplot Generation Process!\n")
  message("This function helps you:")
  message("  📊 Generate high-quality delta boxplots to visualize your experimental data.")
  message("  🔧 Customize visualization by selecting colors and plot themes.")
  message("  💾 Save plots in PNG and HTML formats.")
  message("  💾 Save pairwise percentage differences as an Excel file.\n")
  
  # ---------------------------
  # Load pre-recorded inputs.
  pipeline_inputs <- list()
  inputs_path <- "inputs/tracking_mode/light_dark_mode/inputs_values"
  inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
  inputs_file_csv <- file.path(inputs_path, "pipeline_inputs.csv")
  if (file.exists(inputs_file_xlsx)) {
    df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.xlsx file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  } else if (file.exists(inputs_file_csv)) {
    df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
    if (!all(c("parameters", "input") %in% colnames(df))) {
      stop("The pipeline_inputs.csv file must contain the columns 'parameters' and 'input'.")
    }
    pipeline_inputs <- setNames(as.list(df$input), df$parameters)
  }
  
  # ---------------------------
  # Local helper: get input and record it.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
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
  
  # Helper to split comma-separated strings.
  split_and_trim <- function(x) {
    result <- unlist(strsplit(x, ","))
    trimws(result)
  }
  
  # ---------------------------
  # Step 1: Ask whether to generate delta boxplots.
  generate_delta_boxplots <- get_input_local(
    "generate_delta_boxplots",
    prompt_msg = "❓ Do you want to generate delta boxplots? (yes/no): ",
    validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
    transform_fn = function(x) tolower(trimws(x)),
    error_msg = "⚠️ Please enter 'yes' or 'no'."
  )
  generate_delta_boxplots <- generate_delta_boxplots %in% c("yes", "y")
  if (generate_delta_boxplots) {
    message("✔️ Proceeding with delta boxplot generation.")
  } else {
    message("❌ Delta boxplot generation skipped. The Excel file will still be generated.")
  }
  
  # ---------------------------
  # Step 2: Validate input data structure.
  message("🔍 Checking data structure...")
  if (!"data.frame" %in% class(input_data)) {
    stop("Error: input_data must be a data frame!")
  }
  required_cols <- c("start", "zone", "condition_grouped", "period_without_numbers", "momentum")
  if (!all(required_cols %in% colnames(input_data))) {
    stop("Error: input_data must contain 'start', 'zone', 'condition_grouped', 'period_without_numbers', and 'momentum' columns.")
  }
  
  boxplot_data <- input_data
  
  # ---------------------------
  # Step 3: Order conditions.
  if (exists("generated_condition_grouped_order", envir = .GlobalEnv)) {
    message("✔️ Using 'generated_condition_grouped_order' to order condition groups.")
    boxplot_data$condition_grouped <- factor(
      boxplot_data$condition_grouped,
      levels = get("generated_condition_grouped_order", envir = .GlobalEnv)
    )
  } else {
    stop("❌ 'generated_condition_grouped_order' does not exist in the global environment. Please define it before running this function.")
  }
  
  # ---------------------------
  # Step 4: Define output directories.
  message("📁 Creating output directories...")
  html_path <- file.path(output_dir, "html")
  png_path <- file.path(output_dir, "png")
  jpg_path <- file.path(output_dir, "jpg")
  excel_path <- file.path(excel_output_dir)
  
  dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(jpg_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(excel_path, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Output directories created.")
  
  # ---------------------------
  # Step 5: Manage colors.
  switch_colors <- get_input_local(
    "switch_colors",
    prompt_msg = "🎨 Enter custom colors for delta boxplots for 'before, switch, after' (comma-separated, e.g., #1f77b4, #2ca02c, #ff7f0e), or press Enter to use defaults: ",
    validate_fn = function(x) TRUE,
    transform_fn = function(x) {
      trimmed <- trimws(x)
      if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
    }
  )
  default_switch_colors <- c("before" = "#1f77b4", "switch" = "#2ca02c", "after" = "#ff7f0e")
  if (length(switch_colors) == 0) {
    colors <- default_switch_colors
    message("✔️ No custom colors provided; using default colors.")
  } else if (length(switch_colors) != 3) {
    message("⚠️ Number of custom colors (", length(switch_colors), 
            ") does not equal 3. Using default colors instead.")
    colors <- default_switch_colors
  } else {
    colors <- setNames(switch_colors, c("before", "switch", "after"))
    message("✔️ Custom colors recorded.")
  }
  message("✔️ Colors used :")
  message(sprintf("- before: %s", colors["before"]))
  message(sprintf("- switch: %s", colors["switch"]))
  message(sprintf("- after: %s", colors["after"]))
  
  # ---------------------------
  # Step 6: Define themes.
  light_theme <- function(base_size = 11, base_family = "") {
    theme_bw() %+replace%
      theme(
        plot.title = element_text(color = "black", size = 14, hjust = 0.5),
        axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
        legend.position = "right",
        legend.text = element_text(color = "black", size = 12, face = "italic"),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.background = element_rect(fill = "white"),
        plot.caption = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
      )
  }
  
  dark_theme <- function(base_size = 11, base_family = "") {
    theme_bw() %+replace%
      theme(
        plot.title = element_text(color = "white", size = 14, hjust = 0.5),
        axis.text.y = element_text(color = "white", size = 12),
        axis.text.x = element_text(color = "white", size = 12),
        axis.title.x = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
        axis.title.y = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
        legend.position = "right",
        legend.text = element_text(color = "white", size = 12, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(fill = "black", color = NA),
        strip.text.x = element_text(size = 12, color = "white"),
        strip.background = element_rect(fill = "black", color = "white"),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = "black"),
        panel.border = element_rect(color = "white", fill = NA),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey30"),
        plot.caption = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
      )
  }
  
  # ---------------------------
  # Step 7: Get prompts for output formats.
  message("❓ Please specify the output formats for delta boxplots:")
  generate_delta_boxplots_html <- get_input_local(
    "generate_delta_boxplots_html",
    prompt_msg = "❓ Do you want to generate interactive HTML delta boxplots? (yes/no): ",
    validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
    transform_fn = function(x) tolower(trimws(x)),
    error_msg = "⚠️ Please enter 'yes' or 'no'."
  )
  
  generate_delta_boxplots_png <- get_input_local(
    "generate_delta_boxplots_png",
    prompt_msg = "❓ Do you want to generate static PNG delta boxplots? (yes/no): ",
    validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
    transform_fn = function(x) tolower(trimws(x)),
    error_msg = "⚠️ Please enter 'yes' or 'no'."
  )
  
  # ---------------------------
  # Step 8: Generate delta boxplots if requested.
  if (generate_delta_boxplots) {
    message("⏳ Generating delta boxplots... Please wait.")
    message("☕ Plot generation may take some time. Enjoy a coffee while you wait.")
    
    # Ensure the 'momentum' column is a factor in the desired order.
    boxplot_data$momentum <- factor(boxplot_data$momentum, levels = c("before", "switch", "after"))
    
    for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
      for (zone_number in unique(boxplot_data$zone)) {
        zone_data <- boxplot_data %>% filter(zone == zone_number)
        for (theme_name in c("light", "dark")) {
          current_theme <- if (theme_name == "light") light_theme() else dark_theme()
          line_color <- if (theme_name == "light") "black" else "white"
          jitter_color <- if (theme_name == "light") "black" else "white"
          
          plot <- tryCatch({
            p <- ggplot(zone_data, aes(
              x = condition_grouped,
              y = .data[[response_var]],
              fill = momentum
            )) +
              geom_boxplot(aes(group = interaction(condition_grouped, momentum)),
                           outlier.shape = NA, alpha = 0.6, color = line_color) +
              geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
                          alpha = 0.6, color = jitter_color) +
              labs(
                x = "Conditions",
                y = sprintf("%s (Zone %s)", response_var, zone_number),
                fill = "Momentum"
              ) +
              current_theme
            p
          }, error = function(e) {
            message("❌ Error: Failed to create plot for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ". Error: ", e$message)
            return(NULL)
          })
          
          if (is.null(plot)) next
          
          if (tolower(generate_delta_boxplots_png) %in% c("yes", "y")) {
            tryCatch({
              png_file <- file.path(png_path, sprintf("delta_boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
              ggsave(filename = png_file, plot = plot, width = 12, height = 9, dpi = 300)
              message("✔️ PNG saved: ", png_file)
            }, error = function(e) {
              message("❌ Error: Failed to save PNG for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ". Error: ", e$message)
            })
          }
          
          if (tolower(generate_delta_boxplots_html) %in% c("yes", "y")) {
            tryCatch({
              html_file <- file.path(html_path, sprintf("delta_boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
              interactive_plot <- ggplotly(plot)
              suppressMessages(saveWidget(interactive_plot, html_file, selfcontained = TRUE))
              message("✔️ HTML saved: ", html_file)
            }, error = function(e) {
              message("❌ Error: Failed to save HTML for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ". Error: ", e$message)
            })
          }
        }
      }
    }
    message("🎉 Delta boxplots generation completed!")
  } else {
    message("❌ Delta boxplot generation skipped.")
  }
  
  # ---------------------------
  # Step 9: Generate pairwise percentage differences and write Excel file.
  percentage_diff_results <- list()
  for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
    message(sprintf("📊 Calculating percentage differences for %s (pairwise comparisons)...", response_var))
    results <- boxplot_data %>% 
      group_by(period_without_numbers, zone) %>% 
      nest() %>% 
      mutate(
        comparison_results = purrr::map(data, function(df) {
          condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
          purrr::map_dfr(condition_pairs, function(pair) {
            cond1 <- df %>% filter(condition_grouped == pair[1])
            cond2 <- df %>% filter(condition_grouped == pair[2])
            if (nrow(cond1) > 0 && nrow(cond2) > 0) {
              tibble(
                condition_comparison = paste(pair[1], pair[2], sep = "-"),
                mean_value_1 = round(mean(cond1[[response_var]], na.rm = TRUE), 2),
                mean_value_2 = round(mean(cond2[[response_var]], na.rm = TRUE), 2),
                median_value_1 = round(median(cond1[[response_var]], na.rm = TRUE), 2),
                median_value_2 = round(median(cond2[[response_var]], na.rm = TRUE), 2),
                mean_diff_pct = round(
                  (mean(cond2[[response_var]], na.rm = TRUE) - mean(cond1[[response_var]], na.rm = TRUE)) /
                    abs(mean(cond1[[response_var]], na.rm = TRUE)) * 100, 2
                ),
                median_diff_pct = round(
                  (median(cond2[[response_var]], na.rm = TRUE) - median(cond1[[response_var]], na.rm = TRUE)) /
                    abs(median(cond1[[response_var]], na.rm = TRUE)) * 100, 2
                )
              )
            } else {
              tibble()
            }
          })
        })
      ) %>% 
      select(-data) %>% 
      tidyr::unnest(comparison_results)
    
    percentage_diff_results[[response_var]] <- results
  }
  
  message(sprintf("🎉 Pairwise percentage differences written to %s with conditional formatting!", 
                  file.path(excel_output_dir, "delta_percentage_differences_pairwise.xlsx")))
  excel_file <- file.path(excel_output_dir, "delta_percentage_differences_pairwise.xlsx")
  wb <- createWorkbook()
  
  for (response_var in names(percentage_diff_results)) {
    addWorksheet(wb, response_var)
    writeData(wb, response_var, percentage_diff_results[[response_var]])
    
    mean_diff_col <- which(names(percentage_diff_results[[response_var]]) == "mean_diff_pct")
    median_diff_col <- which(names(percentage_diff_results[[response_var]]) == "median_diff_pct")
    
    if (length(mean_diff_col) > 0) {
      conditionalFormatting(
        wb, sheet = response_var,
        cols = mean_diff_col,
        rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
        rule = ">0", style = createStyle(bgFill = "#b9ffb2")
      )
      conditionalFormatting(
        wb, sheet = response_var,
        cols = mean_diff_col,
        rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
        rule = "<0", style = createStyle(bgFill = "#ffb2b2")
      )
    }
    
    if (length(median_diff_col) > 0) {
      conditionalFormatting(
        wb, sheet = response_var,
        cols = median_diff_col,
        rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
        rule = ">0", style = createStyle(bgFill = "#b9ffb2")
      )
      conditionalFormatting(
        wb, sheet = response_var,
        cols = median_diff_col,
        rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
        rule = "<0", style = createStyle(bgFill = "#ffb2b2")
      )
    }
  }
  
  tryCatch({
    saveWorkbook(wb, excel_file, overwrite = TRUE)
  }, error = function(e) {
    message("❌ Error: Failed to save Excel workbook. Error: ", e$message)
  })
}
