generate_and_save_boxplots_with_excel_files <- function(input_data,
                                                        output_dir = "outputs/quantization_mode/vibration_mode/figures/boxplots",
                                                        excel_output_dir = "outputs/quantization_mode/vibration_mode/tables") {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(ggplot2)
    library(plotly)
    library(htmlwidgets)
    library(webshot2)
    library(openxlsx)
  })
  
  message("\n---\n---\n---\n")
  
  # Introduction
  message("👋 Welcome to the Boxplot and Excel Generation Process!\n")
  message("This function helps you:")
  message("🔧 Customize options such as removing the acclimatation period and choosing colors.")
  message("📊 Generate boxplots in various formats:")
  message("  - PNG: High-quality images suitable for publications or presentations.")
  message("  - JPG: Standard-quality images with smaller file size.")
  message("  - HTML: Interactive plots viewable in a browser.")
  message("💾 Save pairwise percentage differences as an Excel file.\n")
  
  # Step 1: Ask if the user wants to generate boxplots
  repeat {
    generate_boxplots_input <- readline(prompt = "Do you want to generate boxplots? (yes/no): ")
    if (tolower(generate_boxplots_input) %in% c("yes", "y", "no", "n")) break
    message("⚠️ Invalid response. Please enter 'yes' or 'no'.")
  }
  
  generate_boxplots <- tolower(generate_boxplots_input) %in% c("yes", "y")
  if (!generate_boxplots) {
    message("❌ Boxplot generation skipped.")
  } else {
    message("✔️ Proceeding with boxplot generation.")
  }
  
  # Validate input data structure
  message("\n🔍 Checking data structure...")
  if (!"data.frame" %in% class(input_data)) {
    stop("Error: input_data must be a data frame!")
  }
  required_cols <- c("start_rounded", "zone", "condition", "condition_grouped")
  if (!all(required_cols %in% colnames(input_data))) {
    stop("Error: input_data must contain 'start_rounded', 'zone', 'condition', and 'condition_grouped' columns.")
  }
  
  # Initialize boxplot_data
  # Make sure this variable (pretreated_data_for_boxplots_df) exists in your environment 
  # or adjust this line to use input_data directly if needed.
  boxplot_data <- pretreated_data_for_boxplots_df
  
  # Ensure the conditions are ordered based on generated_condition_grouped_order
  if (exists("generated_condition_grouped_order", envir = .GlobalEnv)) {
    message("\n✔️ Using 'generated_condition_grouped_order' to define the order of condition groups.")
    boxplot_data$condition_grouped <- factor(
      boxplot_data$condition_grouped,
      levels = get("generated_condition_grouped_order", envir = .GlobalEnv)
    )
  } else {
    stop("❌ 'generated_condition_grouped_order' does not exist in the global environment. Please define it before running this function.")
  }
  
  # Step 2: Remove acclimatation period if required
  keep_acclimatation <- readline(prompt = "Do you want to keep the acclimatation period in the data? (yes/no): ")
  if (tolower(keep_acclimatation) %in% c("no", "n")) {
    boxplot_data <- boxplot_data %>%
      filter(!grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
    message("❌ Acclimatation period removed from the data.")
  } else {
    message("✔️ Acclimatation period kept in the data.")
  }
  
  # Define output directories
  html_path <- file.path(output_dir, "html")
  png_path <- file.path(output_dir, "png")
  jpg_path <- file.path(output_dir, "jpg")
  excel_path <- file.path(excel_output_dir)
  
  dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(jpg_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(excel_path, recursive = TRUE, showWarnings = FALSE)
  
  # Step 3: Manage colors
  if (exists("custom_colors_global", envir = .GlobalEnv)) {
    colors <- get("custom_colors_global", envir = .GlobalEnv)
    message("✔️ Using existing colors from 'custom_colors_global'.\n")
  } else {
    message("🎨 Generating custom colors...")
    condition_groups <- unique(boxplot_data$condition_grouped)
    default_colors <- c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF",
                        "#FF9966", "#66CCCC", "#FF6699", "#99CCFF", "#66FF66",
                        "#CCCC33", "#CC9966", "#66FFCC", "#FF6666", "#9933CC",
                        "#3366FF", "#FFCC99", "#66CC99", "#FF9999", "#CCCCFF")
    default_colors <- rep(default_colors, length.out = length(condition_groups))
    names(default_colors) <- condition_groups
    
    custom_colors <- list()
    for (group in condition_groups) {
      color_input <- readline(prompt = sprintf("Enter a color for %s (default: %s): ", group, default_colors[group]))
      custom_colors[[group]] <- if (color_input == "") default_colors[group] else color_input
    }
    colors <- custom_colors
    assign("custom_colors_global", colors, envir = .GlobalEnv)
    message("✔️ Colors saved in 'custom_colors_global'.")
  }
  
  # Step 4: Define themes
  light_theme <- function(base_size = 11, base_family = "") {
    theme_bw() %+replace%
      theme(
        plot.title = element_text(color = "black", size = 14, hjust = 0.5),
        axis.text.y  = element_text(color = "black", size = 12, angle = 0),
        axis.text.x = element_text(color = "black", size = 12, angle = 0),
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
        axis.text.y  = element_text(color = "white", size = 12, angle = 0),
        axis.text.x = element_text(color = "white", size = 12, angle = 0),
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
  
  # Step 5: Generate boxplots (only if the user wants to)
  if (generate_boxplots) {
    response_vars <- grep("^mean_", colnames(boxplot_data), value = TRUE)
    message("⏳ Generating boxplots... Please wait.")
    message("☕ Generating plots may take some time. Feel free to enjoy a coffee while you wait.\n")
    
    for (response_var in response_vars) {
      for (zone_number in unique(boxplot_data$zone)) {
        zone_data <- boxplot_data %>% filter(zone == zone_number)
        
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
            geom_jitter(position = position_jitter(width = 0.2), alpha = 0.6, color = jitter_color) +
            facet_wrap(~period_without_numbers, scales = "free_x") +
            labs(
              x = "Conditions",
              y = sprintf("%s (Zone %s)", response_var, zone_number),
              fill = "Condition"
            ) +
            current_theme +
            scale_fill_manual(values = colors)
          
          html_file <- file.path(html_path, sprintf("boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
          plotly_plot <- ggplotly(plot)
          
          # Disable hover info on jitter points so they don't overlap with box hover
          for (i in seq_along(plotly_plot$x$data)) {
            if (plotly_plot$x$data[[i]]$type != "box") {
              plotly_plot$x$data[[i]]$hoverinfo <- "skip"
            }
          }
          saveWidget(plotly_plot, html_file, selfcontained = TRUE)
          
          png_file <- file.path(png_path, sprintf("boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
          suppressMessages(webshot2::webshot(html_file, file = png_file, vwidth = 1200, vheight = 900, zoom = 2))
          
          jpg_file <- file.path(jpg_path, sprintf("boxplot_%s_zone_%s_%s.jpg", response_var, zone_number, theme_name))
          suppressMessages(webshot2::webshot(html_file, file = jpg_file, vwidth = 1200, vheight = 900, zoom = 2))
        }
      }
    }
    message("\n🎉 Boxplots generation completed in PNG, JPG, and HTML formats!\n")
  }
  
  # Step 6: Generate percentage differences for all pairwise comparisons and write Excel file
  response_vars <- grep("^mean_", colnames(boxplot_data), value = TRUE)
  percentage_diff_results <- list()
  
  for (response_var in response_vars) {
    message(sprintf("📊 Calculating percentage differences for %s (pairwise comparisons)...", response_var))
    
    results <- boxplot_data %>%
      group_by(period_without_numbers, zone) %>%
      nest() %>%
      mutate(
        comparison_results = purrr::map(data, function(df) {
          # Generate all pairwise combinations of condition_grouped
          condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
          
          # Create a results data frame for pairwise comparisons
          purrr::map_dfr(condition_pairs, function(pair) {
            cond1 <- df %>% filter(condition_grouped == pair[1])
            cond2 <- df %>% filter(condition_grouped == pair[2])
            
            # Ensure the response_var column exists for calculations
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
  
  # Write the results to Excel with conditional formatting
  excel_file <- file.path(excel_output_dir, "percentage_differences_pairwise.xlsx")
  wb <- createWorkbook()
  
  for (response_var in names(percentage_diff_results)) {
    addWorksheet(wb, response_var)
    writeData(wb, response_var, percentage_diff_results[[response_var]])
    
    # Apply conditional formatting to mean_diff_pct and median_diff_pct
    mean_diff_col <- which(names(percentage_diff_results[[response_var]]) == "mean_diff_pct")
    median_diff_col <- which(names(percentage_diff_results[[response_var]]) == "median_diff_pct")
    
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
  
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  message(sprintf("\n🎉 Pairwise percentage differences written to %s with conditional formatting!", excel_file))
}
