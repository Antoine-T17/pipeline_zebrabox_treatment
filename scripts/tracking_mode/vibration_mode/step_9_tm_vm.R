# -----------------------------------------------------------
# primary mode : tracking mode
# secondary mode : vibration mode
# Function: generate_and_save_boxplots_with_excel_files
# Purpose: Generates boxplots from pretreated boxplot data, validates the data structure,
#          manages colors and themes, saves plots in PNG and interactive HTML formats,
#          and writes pairwise percentage differences to an Excel file with conditional formatting.
#          Interactive HTML plots are first saved to a temporary directory and then moved.
# -----------------------------------------------------------
generate_and_save_boxplots_with_excel_files <- function(input_data = get("pretreated_data_for_boxplots_df", envir = .GlobalEnv),
                                                        output_dir = "outputs/tracking_mode/vibration_mode/figures/boxplots",
                                                        excel_output_dir = "outputs/tracking_mode/vibration_mode/tables") {
  tryCatch({
    # Step 1: Display welcome message.
    message("\n---\n")
    message("👋 Welcome to the Boxplot Generation Process (Vibration Mode)!")
    message("📋 This function will help you:")
    message("   • Generate boxplots for visualizing experimental data.")
    message("   • Customize plots using themes and colors.")
    message("   • Save plots in PNG and interactive HTML formats.")
    message("   • Write pairwise percentage differences to an Excel file.\n")
    
    # Step 2: Retrieve pre-recorded inputs and initialize input record.
    pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
    input_record_list <<- list()
    
    # Step 3: Define unified input helper.
    get_input_local <- function(param, prompt_msg, default_value = NULL,
                                validate_fn = function(x) TRUE,
                                transform_fn = function(x) x,
                                error_msg = "❌ Invalid input. Please try again.") {
      if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "" &&
          !is.na(pipeline_inputs[[param]])) {
        candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
        message("💾 Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- candidate
        return(candidate)
      }
      if (!interactive() && !is.null(default_value)) {
        message("ℹ️ Non-interactive mode: using default value for '", param, "': ", default_value)
        input_record_list[[param]] <<- default_value
        return(default_value)
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
    
    # Step 4: Ask whether to generate boxplots.
    generate_boxplots <- get_input_local("generate_boxplots",
                                         "❓ Do you want to generate boxplots? (yes/no): ",
                                         default_value = "yes",
                                         validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                         transform_fn = function(x) tolower(trimws(x)),
                                         error_msg = "❌ Please enter 'yes' or 'no'.")
    do_plot_generation <- generate_boxplots %in% c("yes", "y")
    if (!do_plot_generation) {
      message("❌ Boxplot figure generation skipped. Proceeding with pairwise comparisons.")
    } else {
      message("✔️ Proceeding with boxplot figure generation.")
    }
    
    # Step 5: Validate input data structure.
    message("🔍 Validating input data structure for boxplots...")
    if (!"data.frame" %in% class(input_data)) {
      message("❌ input_data must be a data frame. Skipping boxplot generation.")
      return(invisible(NULL))
    }
    required_cols <- c("start_rounded", "zone", "condition", "condition_grouped")
    if (!all(required_cols %in% colnames(input_data))) {
      message("❌ input_data missing required columns: ", paste(required_cols, collapse = ", "), ". Skipping.")
      return(invisible(NULL))
    }
    boxplot_data <- input_data
    message("✔️ Data structure validated.")
    
    # Step 6: Remove acclimatation period if required.
    keep_acclimatation <- get_input_local("keep_acclimatation",
                                          "❓ Keep the acclimatation period? (yes/no): ",
                                          default_value = "no",
                                          validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                          transform_fn = function(x) tolower(trimws(x)),
                                          error_msg = "❌ Please enter 'yes' or 'no'.")
    if (keep_acclimatation %in% c("no", "n")) {
      boxplot_data <- filter(boxplot_data, !grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
      message("✔️ Acclimatation period removed.")
    } else {
      message("✔️ Acclimatation period retained.")
    }
    
    # Step 7: Define output directories.
    html_dir <- file.path(output_dir, "html")
    png_dir  <- file.path(output_dir, "png")
    excel_path <- file.path(excel_output_dir)
    dir.create(html_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(excel_path, recursive = TRUE, showWarnings = FALSE)
    message("✔️ Output directories created.")
    
    # Step 8: Manage colors.
    if (!is.null(pipeline_inputs[["conditions_grouped_order"]]) &&
        pipeline_inputs[["conditions_grouped_order"]] != "") {
      order_str <- unlist(strsplit(as.character(pipeline_inputs[["conditions_grouped_order"]]), ";"))[1]
      condition_groups <- trimws(unlist(strsplit(order_str, ",")))
    } else {
      condition_groups <- unique(boxplot_data$condition_grouped)
    }
    default_condition_colors <- rep(c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF",
                                      "#FF9966", "#66CCCC", "#FF6699", "#99CCFF", "#66FF66",
                                      "#CCCC33", "#CC9966", "#66FFCC", "#FF6666", "#9933CC",
                                      "#3366FF", "#FFCC99", "#66CC99", "#FF9999", "#CCCCFF"),
                                    length.out = length(condition_groups))
    names(default_condition_colors) <- condition_groups
    custom_color <- get_input_local("custom_color",
                                    "🎨 Enter custom colors for conditions (comma-separated), or press Enter for defaults: ",
                                    validate_fn = function(x) TRUE,
                                    transform_fn = function(x) {
                                      trimmed <- trimws(x)
                                      if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
                                    })
    if (length(custom_color) == 0) {
      colors <- default_condition_colors
      message("✔️ Using default colors.")
    } else if (length(custom_color) != length(condition_groups)) {
      message("⚠️ Number of custom colors does not match condition groups. Using default colors.")
      colors <- default_condition_colors
    } else {
      colors <- custom_color
      names(colors) <- condition_groups
      message("✔️ Custom colors set.")
    }
    for (group in condition_groups) {
      message(sprintf("   - %s: %s", group, colors[group]))
    }
    
    # Step 9: Define themes.
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
        strip.text.x = element_text(color = "white", size = 12),
        strip.background = element_rect(fill = "black", color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_rect(color = "white", fill = NA),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey30"),
        plot.caption = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
      )
    }
    
    # Step 10: Prompt for desired output formats.
    generate_boxplots_html <- get_input_local("generate_boxplots_html",
                                              "❓ Generate interactive HTML boxplots? (yes/no): ",
                                              default_value = "no",
                                              validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                              transform_fn = function(x) tolower(trimws(x)),
                                              error_msg = "❌ Please enter 'yes' or 'no'.")
    generate_boxplots_png <- get_input_local("generate_boxplots_png",
                                             "❓ Generate static PNG boxplots? (yes/no): ",
                                             default_value = "yes",
                                             validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                             transform_fn = function(x) tolower(trimws(x)),
                                             error_msg = "❌ Please enter 'yes' or 'no'.")
    
    # Step 11: Set condition_grouped factor levels using the desired order.
    if (!is.null(pipeline_inputs[["conditions_grouped_order"]]) &&
        pipeline_inputs[["conditions_grouped_order"]] != "") {
      order_str <- unlist(strsplit(as.character(pipeline_inputs[["conditions_grouped_order"]]), ";"))[1]
      desired_order <- trimws(unlist(strsplit(order_str, ",")))
      boxplot_data$condition_grouped <- factor(boxplot_data$condition_grouped, levels = desired_order)
      message("✔️ condition_grouped factor levels set to: ", paste(levels(boxplot_data$condition_grouped), collapse = ", "))
    }
    
    # Step 12: Generate boxplots.
    if (do_plot_generation) {
      message("⏳ Generating boxplots... This may take a moment.")
      for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
        for (zone_number in unique(boxplot_data$zone)) {
          zone_data <- filter(boxplot_data, zone == zone_number)
          for (theme_name in c("light", "dark")) {
            current_theme <- if (theme_name == "light") light_theme() else dark_theme()
            
            p_png <- ggplot(zone_data, aes(x = condition_grouped, y = .data[[response_var]], fill = condition_grouped)) +
              geom_boxplot(outlier.shape = NA, alpha = 0.6,
                           color = if (theme_name == "light") "black" else "white") +
              geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                         size = 1.75, alpha = 0.6,
                         color = if (theme_name == "light") "black" else "white") +
              facet_wrap(~period_without_numbers, scales = "free_x") +
              labs(x = "Conditions", y = sprintf("%s (Zone %s)", response_var, zone_number), fill = "Condition") +
              scale_fill_manual(values = if (exists("custom_colors_global", envir = .GlobalEnv))
                get("custom_colors_global", envir = .GlobalEnv) else colors) +
              current_theme
            
            p_html <- ggplot(zone_data, aes(x = condition_grouped, y = .data[[response_var]], fill = condition_grouped,
                                            text = paste("Condition Grouped:", condition_grouped,
                                                         "<br>Condition Tagged:", condition_tagged,
                                                         "<br>Animal:", animal,
                                                         "<br>Response:", .data[[response_var]]))) +
              geom_boxplot(outlier.shape = NA, alpha = 0.6,
                           color = if (theme_name == "light") "black" else "white") +
              geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                         size = 1.5, alpha = 0.6,
                         color = if (theme_name == "light") "black" else "white") +
              facet_wrap(~period_without_numbers, scales = "free_x") +
              labs(x = "Conditions", y = sprintf("%s (Zone %s)", response_var, zone_number), fill = "Condition") +
              scale_fill_manual(values = if (exists("custom_colors_global", envir = .GlobalEnv))
                get("custom_colors_global", envir = .GlobalEnv) else colors) +
              current_theme
            
            if (tolower(generate_boxplots_png) %in% c("yes", "y")) {
              tryCatch({
                png_file <- file.path(png_dir, sprintf("boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
                ggsave(filename = png_file, plot = p_png, width = 12, height = 9, dpi = 300)
                message("✔️ PNG saved: ", png_file)
              }, error = function(e) {
                message("❌ Error saving PNG for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
              })
            }
            if (tolower(generate_boxplots_html) %in% c("yes", "y")) {
              tryCatch({
                temp_html <- file.path(tempdir(), sprintf("boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
                final_html <- file.path(html_dir, sprintf("boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
                suppressWarnings(htmlwidgets::saveWidget(plotly::ggplotly(p_html, tooltip = "text"), 
                                                         temp_html, selfcontained = TRUE))
                file.copy(temp_html, final_html, overwrite = TRUE)
                file.remove(temp_html)
                message("✔️ HTML saved: ", final_html)
              }, error = function(e) {
                message("❌ Error saving HTML for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
              })
            }
          }
        }
      }
      message("🎉 Boxplots generated successfully!\n")
    } else {
      message("❌ Boxplot generation skipped.")
    }
    
    # Step 13: Generate pairwise percentage differences and write Excel file.
    percentage_diff_results <- list()
    for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
      message(sprintf("📊 Calculating percentage differences for %s...", response_var))
      results <- boxplot_data %>% group_by(period_without_numbers, zone) %>% 
        tidyr::nest() %>% 
        mutate(
          comparison_results = purrr::map(data, function(df) {
            condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
            purrr::map_dfr(condition_pairs, function(pair) {
              cond1 <- filter(df, condition_grouped == pair[1])
              cond2 <- filter(df, condition_grouped == pair[2])
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
    
    excel_file <- file.path(excel_output_dir, "percentage_differences_pairwise.xlsx")
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
    
    openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
    message(sprintf("🎉 Pairwise differences saved to %s with conditional formatting!\n", excel_file))
    
    # Step 14: Save combined outputs globally.
    final_boxplots <- do.call(rbind, boxplot_list)
    assign("pretreated_data_for_boxplots_df", final_boxplots, envir = .GlobalEnv)
    
    return(list(
      boxplots = final_boxplots,
      percentage_differences = percentage_diff_results
    ))
    
  }, error = function(e) {
    message("❌ Error in generate_and_save_boxplots_with_excel_files: ", e$message)
    return(invisible(NULL))
  })
}
