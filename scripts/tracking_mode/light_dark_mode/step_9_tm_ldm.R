generate_and_save_boxplots_with_excel_files <- function(input_data = get("pretreated_data_for_boxplots_df", envir = .GlobalEnv),
                                                        output_dir = "outputs/tracking_mode/light_dark_mode/figures/boxplots",
                                                        excel_output_dir = "outputs/tracking_mode/light_dark_mode/tables") {
  tryCatch({
    message("\n---\n---\n---\n")
    message("👋 Welcome to the Boxplot Generation Process!")
    message("This function helps you:")
    message("  📊 Generate high-quality boxplots to visualize your experimental data.")
    message("  🔧 Customize visualization by selecting periods, colors, and plot themes.")
    message("  💾 Save plots in PNG and HTML formats, and calculate pairwise comparisons.\n")
    
    # ---------------------------
    # Load pre-recorded inputs.
    pipeline_inputs <- list()
    inputs_path <- "inputs/tracking_mode/light_dark_mode/inputs_values"
    inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
    inputs_file_csv <- file.path(inputs_path, "pipeline_inputs.csv")
    if (file.exists(inputs_file_xlsx)) {
      df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
      if (!all(c("parameters", "input") %in% colnames(df))) {
        message("❌ The pipeline_inputs.xlsx file must contain the columns 'parameters' and 'input'. Skipping boxplot generation.")
        return(invisible(NULL))
      }
      pipeline_inputs <- setNames(as.list(df$input), df$parameters)
    } else if (file.exists(inputs_file_csv)) {
      df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
      if (!all(c("parameters", "input") %in% colnames(df))) {
        message("❌ The pipeline_inputs.csv file must contain the columns 'parameters' and 'input'. Skipping boxplot generation.")
        return(invisible(NULL))
      }
      pipeline_inputs <- setNames(as.list(df$input), df$parameters)
    }
    
    # ---------------------------
    # Local helper: get_input_local
    get_input_local <- function(param, prompt_msg, default_value = NULL,
                                validate_fn = function(x) TRUE,
                                transform_fn = function(x) x,
                                error_msg = "Invalid input. Please try again.") {
      if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "" && !is.na(pipeline_inputs[[param]])) {
        candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
        message("Using pre-recorded input for '", param, "': ", candidate)
        input_record_list[[param]] <<- paste(candidate, collapse = ", ")
        return(candidate)
      }
      if (!interactive() && !is.null(default_value)) {
        message("Non-interactive mode: using default value for '", param, "': ", default_value)
        input_record_list[[param]] <<- paste(default_value, collapse = ", ")
        return(default_value)
      }
      repeat {
        user_input <- readline(prompt = prompt_msg)
        candidate <- transform_fn(user_input)
        if (validate_fn(candidate)) {
          input_record_list[[param]] <<- paste(candidate, collapse = ", ")
          return(candidate)
        } else {
          message(error_msg)
        }
      }
    }
    
    # Helper: Split comma-separated strings and trim whitespace.
    split_and_trim <- function(x) {
      trimws(unlist(strsplit(x, ",")))
    }
    
    # ---------------------------
    # Step 1: Ask whether to generate boxplots.
    generate_boxplots <- get_input_local(
      "generate_boxplots",
      prompt_msg = "❓ Do you want to generate boxplots? (yes/no): ",
      default_value = "yes",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Please enter 'yes' or 'no'."
    )
    do_plot_generation <- generate_boxplots %in% c("yes", "y")
    if (!do_plot_generation) {
      message("❌ Boxplot figure generation skipped. Pairwise comparisons will still be calculated.")
    } else {
      message("✔️ Proceeding with boxplot figure generation.")
    }
    
    # ---------------------------
    # Step 2: Validate input data structure.
    message("🔍 Checking data structure...")
    if (!"data.frame" %in% class(input_data)) {
      message("❌ Error: input_data must be a data frame! Skipping boxplot generation.")
      return(invisible(NULL))
    }
    required_cols <- c("start_rounded", "zone", "condition", "condition_grouped")
    if (!all(required_cols %in% colnames(input_data))) {
      message("❌ Error: input_data must contain 'start_rounded', 'zone', 'condition', and 'condition_grouped' columns. Skipping boxplot generation.")
      return(invisible(NULL))
    }
    boxplot_data <- input_data
    
    # ---------------------------
    # Step 3: Remove acclimatation period if required.
    keep_acclimatation <- get_input_local(
      "keep_acclimatation",
      prompt_msg = "❓ Do you want to keep the acclimatation period in the data? (yes/no): ",
      default_value = "no",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Please enter 'yes' or 'no'."
    )
    if (keep_acclimatation %in% c("no", "n")) {
      boxplot_data <- dplyr::filter(boxplot_data, !grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
      message("✔️ Acclimatation period removed from the data.")
    } else {
      message("✔️ Acclimatation period kept in the data.")
    }
    
    # ---------------------------
    # Step 4: Define output directories.
    html_path <- file.path(output_dir, "html")
    png_path <- file.path(output_dir, "png")
    jpg_path <- file.path(output_dir, "jpg")
    excel_path <- file.path(excel_output_dir)
    
    dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(jpg_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(excel_path, recursive = TRUE, showWarnings = FALSE)
    
    # ---------------------------
    # Step 5: Manage colors.
    # Notice: No default_value is provided so the user is prompted if the input is empty.
    custom_color <- get_input_local(
      "custom_color",
      prompt_msg = "🎨 Enter custom colors for conditions (comma-separated, e.g., #FF6666, #66B2FF, #99CC33), or press Enter to use defaults: ",
      validate_fn = function(x) TRUE,
      transform_fn = function(x) {
        trimmed <- trimws(x)
        if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
      }
    )
    condition_groups <- unique(boxplot_data$condition_grouped)
    default_condition_colors <- c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF",
                                  "#FF9966", "#66CCCC", "#FF6699", "#99CCFF", "#66FF66",
                                  "#CCCC33", "#CC9966", "#66FFCC", "#FF6666", "#9933CC",
                                  "#3366FF", "#FFCC99", "#66CC99", "#FF9999", "#CCCCFF")
    default_condition_colors <- rep(default_condition_colors, length.out = length(condition_groups))
    names(default_condition_colors) <- condition_groups
    if (length(custom_color) == 0) {
      colors <- default_condition_colors
      message("✔️ No custom colors provided; using default colors:")
    } else if (length(custom_color) != length(condition_groups)) {
      message("⚠️ The number of custom colors provided (", length(custom_color),
              ") does not match the number of condition groups (", length(condition_groups), "). Using default colors instead.")
      colors <- default_condition_colors
    } else {
      colors <- custom_color
      names(colors) <- condition_groups
      message("✔️ Custom colors recorded:")
    }
    for (group in condition_groups) {
      message(sprintf("   - %s: %s", group, colors[group]))
    }
    
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
    # Step 7: Generate boxplot figures if requested.
    generate_boxplots_html <- get_input_local(
      "generate_boxplots_html",
      prompt_msg = "❓ Do you want to generate interactive HTML boxplots? (yes/no): ",
      default_value = "no",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Please enter 'yes' or 'no'."
    )
    generate_boxplots_png <- get_input_local(
      "generate_boxplots_png",
      prompt_msg = "❓ Do you want to generate static PNG boxplots? (yes/no): ",
      default_value = "yes",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Please enter 'yes' or 'no'."
    )
    
    if (do_plot_generation) {
      message("⏳ Generating boxplots... Please wait.")
      message("☕ Boxplot generation may take some time. Enjoy a coffee while you wait.")
      
      for (response_var in grep("^mean_", colnames(boxplot_data), value = TRUE)) {
        for (zone_number in unique(boxplot_data$zone)) {
          zone_data <- dplyr::filter(boxplot_data, zone == zone_number)
          for (theme_name in c("light", "dark")) {
            current_theme <- if (theme_name == "light") light_theme() else dark_theme()
            line_color <- if (theme_name == "light") "black" else "white"
            jitter_color <- if (theme_name == "light") "black" else "white"
            
            plot <- ggplot(zone_data, aes(
              x = condition_grouped,
              y = .data[[response_var]],
              fill = condition_grouped
            )) +
              geom_boxplot(outlier.shape = NA, alpha = 0.6, color = line_color) +
              geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
                          alpha = 0.6, color = jitter_color) +
              facet_wrap(~period_without_numbers, scales = "free_x") +
              labs(
                x = "Conditions",
                y = sprintf("%s (Zone %s)", response_var, zone_number),
                fill = "Condition"
              ) +
              current_theme +
              scale_fill_manual(values = if (exists("custom_colors_global", envir = .GlobalEnv))
                get("custom_colors_global", envir = .GlobalEnv)
                else colors)
            
            if (generate_boxplots_png %in% c("yes", "y")) {
              png_file <- file.path(png_path, sprintf("boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
              ggsave(filename = png_file, plot = plot, width = 12, height = 9, dpi = 300)
              message("✔️ PNG saved: ", png_file)
            }
            
            if (generate_boxplots_html %in% c("yes", "y")) {
              html_file <- file.path(html_path, sprintf("boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
              interactive_plot <- plotly::ggplotly(plot)
              suppressMessages(htmlwidgets::saveWidget(interactive_plot, html_file, selfcontained = TRUE))
              message("✔️ HTML saved: ", html_file)
            }
          }
        }
      }
      message("🎉 Boxplots generation completed successfully in the selected formats!\n")
    } else {
      message("❌ Boxplot figure generation skipped. Proceeding with pairwise comparisons.")
    }
    
    # ---------------------------
    # Step 8: Generate pairwise percentage differences and write Excel file.
    response_vars <- grep("^mean_", colnames(boxplot_data), value = TRUE)
    percentage_diff_results <- list()
    
    for (response_var in response_vars) {
      message(sprintf("📊 Calculating percentage differences for %s (pairwise comparisons)...", response_var))
      
      results <- boxplot_data %>%
        dplyr::group_by(period_without_numbers, zone) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          comparison_results = purrr::map(data, function(df) {
            condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
            purrr::map_dfr(condition_pairs, function(pair) {
              cond1 <- dplyr::filter(df, condition_grouped == pair[1])
              cond2 <- dplyr::filter(df, condition_grouped == pair[2])
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
        dplyr::select(-data) %>%
        tidyr::unnest(comparison_results)
      
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
        openxlsx::conditionalFormatting(
          wb, sheet = response_var,
          cols = mean_diff_col,
          rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
          rule = ">0", style = openxlsx::createStyle(bgFill = "#b9ffb2")
        )
        openxlsx::conditionalFormatting(
          wb, sheet = response_var,
          cols = mean_diff_col,
          rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
          rule = "<0", style = openxlsx::createStyle(bgFill = "#ffb2b2")
        )
      }
      
      if (length(median_diff_col) > 0) {
        openxlsx::conditionalFormatting(
          wb, sheet = response_var,
          cols = median_diff_col,
          rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
          rule = ">0", style = openxlsx::createStyle(bgFill = "#b9ffb2")
        )
        openxlsx::conditionalFormatting(
          wb, sheet = response_var,
          cols = median_diff_col,
          rows = 2:(nrow(percentage_diff_results[[response_var]]) + 1),
          rule = "<0", style = openxlsx::createStyle(bgFill = "#ffb2b2")
        )
      }
    }
    
    openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
    message(sprintf("🎉 Pairwise percentage differences written to %s with conditional formatting!\n", excel_file))
    
  }, error = function(e) {
    message("❌ Error in generate_and_save_boxplots_with_excel_files: ", e$message)
    return(invisible(NULL))
  })
}
