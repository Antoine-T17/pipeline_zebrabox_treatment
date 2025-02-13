generate_and_save_boxplots_delta_with_excel_files <- function(
    input_data = get("pretreated_delta_data_for_boxplots_df", envir = .GlobalEnv),
    output_dir = "outputs/tracking_mode/vibration_mode/figures/boxplots",
    excel_output_dir = "outputs/tracking_mode/vibration_mode/tables"
) {
  message("\n---\n---\n---\n")
  
  # Introduction
  message("\n👋 Welcome to the Delta Boxplot Generation Process!\n")
  message("This function helps you:\n")
  message("📊 Generate high-quality boxplots to visualize delta-based experimental data.")
  message("🔧 Customize visualization by selecting periods, colors, and plot themes.")
  message("💾 Save plots in the following formats:\n")
  message("  - PNG: High-quality images for publications or presentations.")
  message("  - JPG: Standard-quality images with smaller file sizes.")
  message("  - HTML: Interactive plots viewable in a browser.\n")
  message("💾 Save pairwise percentage differences as an Excel file.\n")
  
  # Step 1: Ask if the user wants to generate boxplots
  repeat {
    generate_boxplots_input <- readline(prompt = "❓ Do you want to generate delta boxplots? (yes/no): ")
    if (tolower(generate_boxplots_input) %in% c("yes", "y", "no", "n")) break
    message("⚠️ Invalid response. Please enter 'yes' or 'no'.")
  }
  
  generate_boxplots <- tolower(generate_boxplots_input) %in% c("yes", "y")
  if (!generate_boxplots) {
    message("❌ Delta boxplot generation skipped.")
  } else {
    message("✔️ Proceeding with delta boxplot generation.")
  }
  
  # Validate input data structure
  message("🔍 Checking data structure...")
  if (!"data.frame" %in% class(input_data)) {
    stop("Error: input_data must be a data frame!")
  }
  required_cols <- c("start", "zone", "condition_grouped", "period_without_numbers")
  if (!all(required_cols %in% colnames(input_data))) {
    stop("Error: input_data must contain 'start', 'zone', 'condition_grouped', and 'period_without_numbers' columns.")
  }
  
  # Initialize boxplot_data
  boxplot_data <- pretreated_delta_data_for_boxplots_df
  
  # Ensure the conditions are ordered based on generated_condition_grouped_order
  if (exists("generated_condition_grouped_order", envir = .GlobalEnv)) {
    message("✔️ Using 'generated_condition_grouped_order' to define the order of condition groups.")
    boxplot_data$condition_grouped <- factor(
      boxplot_data$condition_grouped,
      levels = get("generated_condition_grouped_order", envir = .GlobalEnv)
    )
  } else {
    stop("❌ 'generated_condition_grouped_order' does not exist in the global environment. Please define it before running this function.")
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
  
  # Step 3: Manage colors (centralized customization for 'before', 'after', and 'switch')
  if (generate_boxplots) {
    message("🎨 You can customize the colors for 'before', 'after', and 'switch'. Press Enter to use the default colors.")
    
    # Default colors
    default_colors <- c(
      "before" = "#1f77b4",  # Blue for 'before'
      "after"  = "#ff7f0e",  # Orange for 'after'
      "switch" = "#2ca02c"   # Green for 'switch'
    )
    
    # Prompt the user for colors
    before_color <- readline(prompt = sprintf("Enter a color for 'before' (default: %s): ", default_colors["before"]))
    after_color <- readline(prompt = sprintf("Enter a color for 'after' (default: %s): ", default_colors["after"]))
    switch_color <- readline(prompt = sprintf("Enter a color for 'switch' (default: %s): ", default_colors["switch"]))
    
    # Assign colors, defaulting to predefined ones if the user presses Enter
    colors <- c(
      "before" = ifelse(before_color == "", default_colors["before"], before_color),
      "after"  = ifelse(after_color == "", default_colors["after"], after_color),
      "switch" = ifelse(switch_color == "", default_colors["switch"], switch_color)
    )
    
    message("✔️ Colors configured for 'before', 'after', and 'switch':")
    message(sprintf("   - 'before': %s", colors["before"]))
    message(sprintf("   - 'after': %s", colors["after"]))
    message(sprintf("   - 'switch': %s", colors["switch"]))
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
  
  
  # Step 5: Suppress Plotly Warning
  suppress_plotly_warning <- function(plot) {
    plot$x$layout <- plot$x$layout[grep('NA', names(plot$x$layout), invert = TRUE)]
    return(plot)
  }
  
  # Step 6: Generate Boxplots (Only If the User Wants To)
  if (generate_boxplots) {
    response_vars <- grep("^mean_", colnames(boxplot_data), value = TRUE)
    message("⏳ Generating boxplots... Please wait.")
    message("☕ Generating plots may take some time. Feel free to enjoy a coffee while you wait.")
    
    # Ensure 'momentum' has the correct order
    boxplot_data$momentum <- factor(boxplot_data$momentum, levels = c("before", "switch", "after"))
    
    for (response_var in response_vars) {
      for (zone_number in unique(boxplot_data$zone)) {
        zone_data <- boxplot_data %>% filter(zone == zone_number)
        
        for (theme_name in c("light", "dark")) {
          # Apply the correct theme and colors
          current_theme <- if (theme_name == "light") light_theme() else dark_theme()
          line_color <- if (theme_name == "light") "black" else "white"
          jitter_color <- if (theme_name == "light") "black" else "white"
          
          # Create the ggplot
          plot <- ggplot(zone_data, aes(
            x = condition_grouped,
            y = .data[[response_var]],
            fill = momentum
          )) +
            geom_boxplot(
              aes(group = interaction(condition_grouped, momentum)),  # Fix grouping for boxplots
              position = position_dodge2(preserve = "single", width = 0.75),  # Fix position
              outlier.shape = NA,
              alpha = 0.6,
              color = line_color
            ) +
            geom_jitter(
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width = 0.75
              ),
              alpha = 0.6,
              color = jitter_color
            ) +
            scale_fill_manual(values = colors) +  # Use centralized colors
            labs(
              x = "Conditions",
              y = sprintf("%s (Zone %s)", response_var, zone_number),
              fill = "Momentum"
            ) +
            current_theme
          
          # Suppress warnings during ggplotly conversion
          suppressWarnings({
            html_file <- file.path(html_path, sprintf("delta_boxplot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
            plotly_plot <- ggplotly(plot) %>%
              layout(
                boxmode = "group"
              )
            
            # Suppress the warning
            plotly_plot <- suppress_plotly_warning(plotly_plot)
            
            # Disable hover info on jitter points
            for (i in seq_along(plotly_plot$x$data)) {
              if (plotly_plot$x$data[[i]]$type != "box") {
                plotly_plot$x$data[[i]]$hoverinfo <- "skip"
              }
            }
            
            saveWidget(plotly_plot, html_file, selfcontained = TRUE)
          })
          
          # Save PNG and JPG
          png_file <- file.path(png_path, sprintf("delta_boxplot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
          suppressMessages(webshot2::webshot(html_file, file = png_file, vwidth = 1200, vheight = 900, zoom = 2))
          
          jpg_file <- file.path(jpg_path, sprintf("delta_boxplot_%s_zone_%s_%s.jpg", response_var, zone_number, theme_name))
          suppressMessages(webshot2::webshot(html_file, file = jpg_file, vwidth = 1200, vheight = 900, zoom = 2))
        }
      }
    }
    message("🎉 Delta boxplots generation completed in PNG, JPG, and HTML formats!\n")
  }
  
  # Step 7: Generate percentage differences for specified "momentum" pairs and write Excel file
  response_vars <- grep("^mean_", colnames(boxplot_data), value = TRUE)
  percentage_diff_results <- list()
  
  # Define the specific momentum pairs to compare
  momentum_pairs <- list(
    c("before", "switch"),
    c("before", "after"),
    c("switch", "after")
  )
  
  for (response_var in response_vars) {
    message(sprintf("📊 Calculating percentage differences for %s (specified momentum pairs)...", response_var))
    
    results <- boxplot_data %>%
      group_by(zone, condition_grouped) %>%  # Group by zone and condition
      nest() %>%
      mutate(
        comparison_results = purrr::map(data, function(df) {
          # Filter data for relevant momentum values
          available_momentum <- unique(df$momentum)
          filtered_pairs <- momentum_pairs %>%
            purrr::keep(~all(.x %in% available_momentum))
          
          # Create results for the specified momentum pairs
          purrr::map_dfr(filtered_pairs, function(pair) {
            cond1 <- df %>% filter(momentum == pair[1])
            cond2 <- df %>% filter(momentum == pair[2])
            
            # Ensure both pairs exist for the calculations
            if (nrow(cond1) > 0 && nrow(cond2) > 0) {
              tibble(
                momentum_comparison = paste(pair[1], pair[2], sep = "-"),
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
              tibble()  # Return empty if data is insufficient
            }
          })
        })
      ) %>%
      select(-data) %>%
      tidyr::unnest(comparison_results)
    
    percentage_diff_results[[response_var]] <- results
  }
  
  # Write the results to Excel with conditional formatting
  excel_file <- file.path(excel_output_dir, "delta_percentage_differences_pairwise.xlsx")
  wb <- createWorkbook()
  
  for (response_var in names(percentage_diff_results)) {
    addWorksheet(wb, response_var)
    writeData(wb, response_var, percentage_diff_results[[response_var]])
    
    # Ensure column indices are dynamically identified
    column_names <- names(percentage_diff_results[[response_var]])
    mean_diff_col <- which(column_names == "mean_diff_pct")
    median_diff_col <- which(column_names == "median_diff_pct")
    
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
  
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  message(sprintf("🎉 Delta pairwise percentage differences written to %s with conditional formatting!\n", excel_file))
}