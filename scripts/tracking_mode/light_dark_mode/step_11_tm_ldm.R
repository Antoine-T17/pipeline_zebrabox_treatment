# -----------------------------------------------------------
# File: generate_and_save_boxplots_delta_with_excel_files.R
# -----------------------------------------------------------
# Harmonized version of the generate_and_save_boxplots_delta_with_excel_files function for light dark mode.
# Harmonized version of the generate_and_save_boxplots_delta_with_excel_files function.
# This function generates delta boxplots from pretreated delta data,
# validates data structure, orders conditions, manages colors and themes,
# prompts for output formats, and writes pairwise percentage differences to an Excel file.
# -----------------------------------------------------------

generate_and_save_boxplots_delta_with_excel_files <- function(input_data = get("pretreated_delta_data_for_boxplots_df", envir = .GlobalEnv),
                                                              output_dir = "outputs/tracking_mode/light_dark_mode/figures/boxplots",
                                                              excel_output_dir = "outputs/tracking_mode/light_dark_mode/tables") {
  message("\n---\n")
  message("👋 Welcome to the Delta Boxplot Generation Process!\n")
  message("📋 This function will help you:")
  message("   • Generate delta boxplots to visualize experimental data differences.")
  message("   • Customize plots with themes and custom colors.")
  message("   • Save plots in PNG and/or interactive HTML formats.")
  message("   • Write pairwise percentage differences to an Excel file.\n")
  
  # Retrieve pre-recorded inputs from the global pipeline_inputs.
  pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
  
  # Unified input helper.
  get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                              transform_fn = function(x) x,
                              error_msg = "❌ Invalid input. Please try again.") {
    if (!is.null(pipeline_inputs[[param]]) && !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
      if (validate_fn(candidate)) {
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param, "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate <- transform_fn(user_input)
      if (validate_fn(candidate)) {
        message("✔️ Input for '", param, "' recorded: ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      } else {
        message(error_msg)
      }
    }
  }
  
  split_and_trim <- function(x) trimws(unlist(strsplit(x, ",")))
  
  # Step 1: Ask whether to generate delta boxplots.
  generate_delta_boxplots <- get_input_local("generate_delta_boxplots",
                                             "❓ Do you want to generate delta boxplots? (yes/no): ",
                                             validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                             transform_fn = function(x) tolower(trimws(x)),
                                             error_msg = "❌ Please enter 'yes' or 'no'.")
  generate_delta_boxplots <- generate_delta_boxplots %in% c("yes", "y")
  if (generate_delta_boxplots) {
    message("✔️ Proceeding with delta boxplot generation.")
  } else {
    message("❌ Delta boxplot generation skipped. The Excel file will still be generated.")
  }
  
  # Step 2: Validate input data structure.
  message("🔍 Validating delta input data structure...")
  if (!"data.frame" %in% class(input_data)) {
    stop("❌ input_data must be a data frame!")
  }
  required_cols <- c("start", "zone", "condition_grouped", "period_without_numbers", "momentum")
  if (!all(required_cols %in% colnames(input_data))) {
    stop("❌ input_data missing required columns: ", paste(required_cols, collapse = ", "))
  }
  boxplot_data <- input_data
  
  # Step 3: Order conditions.
  if (exists("generated_condition_grouped_order", envir = .GlobalEnv)) {
    message("✔️ Ordering condition groups using 'generated_condition_grouped_order'.")
    boxplot_data$condition_grouped <- factor(boxplot_data$condition_grouped,
                                             levels = get("generated_condition_grouped_order", envir = .GlobalEnv))
  } else {
    stop("❌ 'generated_condition_grouped_order' not found in global environment.")
  }
  
  # Step 4: Define output directories.
  message("📁 Creating output directories for delta boxplots...")
  html_path <- file.path(output_dir, "html")
  png_path  <- file.path(output_dir, "png")
  jpg_path  <- file.path(output_dir, "jpg")
  excel_path <- file.path(excel_output_dir)
  dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(jpg_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(excel_path, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Output directories created.")
  
  # Step 5: Manage colors.
  switch_colors <- get_input_local("switch_colors",
                                   "🎨 Enter custom colors for delta boxplots (before, switch, after; comma-separated), or press Enter for defaults: ",
                                   validate_fn = function(x) TRUE,
                                   transform_fn = function(x) {
                                     trimmed <- trimws(x)
                                     if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
                                   })
  default_switch_colors <- c("before" = "#1f77b4", "switch" = "#2ca02c", "after" = "#ff7f0e")
  if (length(switch_colors) == 0) {
    colors <- default_switch_colors
    message("✔️ Using default delta colors.")
  } else if (length(switch_colors) != 3) {
    message("⚠️ Number of colors provided does not equal 3. Using default colors.")
    colors <- default_switch_colors
  } else {
    colors <- setNames(switch_colors, c("before", "switch", "after"))
    message("✔️ Custom delta colors set.")
  }
  message("✔️ Delta colors: before: ", colors["before"], "; switch: ", colors["switch"], "; after: ", colors["after"])
  
  # Step 6: Define themes.
  light_theme <- function(base_size = 11, base_family = "") {
    theme_bw() %+replace% theme(
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
    theme_bw() %+replace% theme(
      plot.title = element_text(color = "white", size = 14, hjust = 0.5),
      axis.text.y = element_text(color = "white", size = 12),
      axis.text.x = element_text(color = "white", size = 12),
      axis.title.x = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
      axis.title.y = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
      legend.position = "right",
      legend.text = element_text(color = "white", size = 12, face = "italic"),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      strip.text.x = element_text(size = 12, color = "white"),
      strip.background = element_rect(fill = "black", color = "white"),
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      panel.border = element_rect(color = "white", fill = NA),
      panel.grid.major = element_line(color = "grey30"),
      panel.grid.minor = element_line(color = "grey30"),
      plot.caption = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
    )
  }
  
  # Step 7: Prompt for output formats.
  generate_delta_boxplots_html <- get_input_local("generate_delta_boxplots_html",
                                                  "❓ Generate interactive HTML delta boxplots? (yes/no): ",
                                                  validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                  transform_fn = function(x) tolower(trimws(x)),
                                                  error_msg = "❌ Please enter 'yes' or 'no'.")
  generate_delta_boxplots_png <- get_input_local("generate_delta_boxplots_png",
                                                 "❓ Generate static PNG delta boxplots? (yes/no): ",
                                                 validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                 transform_fn = function(x) tolower(trimws(x)),
                                                 error_msg = "❌ Please enter 'yes' or 'no'.")
  
  # Step 8: Generate delta boxplots if requested.
  if (generate_delta_boxplots) {
    message("⏳ Generating delta boxplots... This may take some time.")
    boxplot_data$momentum <- factor(boxplot_data$momentum, levels = c("before", "switch", "after"))
    for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
      for (zone_number in unique(boxplot_data$zone)) {
        zone_data <- boxplot_data %>% filter(zone == zone_number)
        for (theme_name in c("light", "dark")) {
          current_theme <- if (theme_name == "light") light_theme() else dark_theme()
          plot <- tryCatch({
            p <- ggplot(zone_data, aes(x = condition_grouped, y = .data[[response_var]], fill = momentum)) +
              geom_boxplot(aes(group = interaction(condition_grouped, momentum)),
                           outlier.shape = NA, alpha = 0.6, color = if (theme_name == "light") "black" else "white") +
              geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
                          alpha = 0.6, color = if (theme_name == "light") "black" else "white") +
              labs(x = "Conditions", y = sprintf("%s (Zone %s)", response_var, zone_number), fill = "Momentum") +
              current_theme
            p
          }, error = function(e) {
            message("❌ Error creating plot for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
            return(NULL)
          })
          if (is.null(plot)) next
          if (tolower(generate_delta_boxplots_png) %in% c("yes", "y")) {
            tryCatch({
              png_file <- file.path(png_path, sprintf("delta_boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
              ggsave(filename = png_file, plot = plot, width = 12, height = 9, dpi = 300)
              message("✔️ PNG saved: ", png_file)
            }, error = function(e) {
              message("❌ Error saving PNG for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
            })
          }
          if (tolower(generate_delta_boxplots_html) %in% c("yes", "y")) {
            tryCatch({
              html_file <- file.path(html_path, sprintf("delta_boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
              interactive_plot <- plotly::ggplotly(plot)
              suppressMessages(htmlwidgets::saveWidget(interactive_plot, html_file, selfcontained = TRUE))
              message("✔️ HTML saved: ", html_file)
            }, error = function(e) {
              message("❌ Error saving HTML for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
            })
          }
        }
      }
    }
    message("🎉 Delta boxplots generated!")
  } else {
    message("❌ Delta boxplot generation skipped.")
  }
  
  # Step 9: Generate pairwise percentage differences and write Excel file.
  percentage_diff_results <- list()
  for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
    message(sprintf("📊 Calculating percentage differences for %s...", response_var))
    results <- boxplot_data %>% group_by(period_without_numbers, zone) %>% 
      tidyr::nest() %>% mutate(
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
                mean_diff_pct = round((mean(cond2[[response_var]], na.rm = TRUE) - mean(cond1[[response_var]], na.rm = TRUE)) /
                                        abs(mean(cond1[[response_var]], na.rm = TRUE)) * 100, 2),
                median_diff_pct = round((median(cond2[[response_var]], na.rm = TRUE) - median(cond1[[response_var]], na.rm = TRUE)) /
                                          abs(median(cond1[[response_var]], na.rm = TRUE)) * 100, 2)
              )
            } else {
              tibble()
            }
          })
        })
      ) %>% select(-data) %>% tidyr::unnest(comparison_results)
    
    percentage_diff_results[[response_var]] <- results
  }
  
  excel_file <- file.path(excel_output_dir, "delta_percentage_differences_pairwise.xlsx")
  wb <- openxlsx::createWorkbook()
  for (response_var in names(percentage_diff_results)) {
    openxlsx::addWorksheet(wb, response_var)
    openxlsx::writeData(wb, response_var, percentage_diff_results[[response_var]])
    mean_diff_col <- which(names(percentage_diff_results[[response_var]]) == "mean_diff_pct")
    median_diff_col <- which(names(percentage_diff_results[[response_var]]) == "median_diff_pct")
    if (length(mean_diff_col) > 0) {
      openxlsx::conditionalFormatting(wb, sheet = response_var, cols = mean_diff_col,
                                      rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
                                      rule = ">0", style = openxlsx::createStyle(bgFill = "#b9ffb2"))
      openxlsx::conditionalFormatting(wb, sheet = response_var, cols = mean_diff_col,
                                      rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
                                      rule = "<0", style = openxlsx::createStyle(bgFill = "#ffb2b2"))
    }
    if (length(median_diff_col) > 0) {
      openxlsx::conditionalFormatting(wb, sheet = response_var, cols = median_diff_col,
                                      rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
                                      rule = ">0", style = openxlsx::createStyle(bgFill = "#b9ffb2"))
      openxlsx::conditionalFormatting(wb, sheet = response_var, cols = median_diff_col,
                                      rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
                                      rule = "<0", style = openxlsx::createStyle(bgFill = "#ffb2b2"))
    }
  }
  
  tryCatch({
    openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
  }, error = function(e) {
    message("❌ Error saving Excel workbook: ", e$message)
  })
  message(sprintf("🎉 Delta pairwise differences written to %s!\n", excel_file))
}
