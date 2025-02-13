generate_and_save_lineplots <- function(input_data, output_dir = "outputs/quantization_mode/light_dark_mode/figures/lineplots") {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(plotly)
    library(htmlwidgets)
    library(colorspace)
    library(webshot2)
  })
  
  message("\n---\n---\n---\n")
  
  # Welcome message
    message("👋 Welcome to the Lineplot Generation Process!\n")
  message("This function helps you:\n")
  message("📊 Generate high-quality line plots for your experimental data.")
  message("🔧 Customize visualization by selecting periods, colors, and plot themes.")
  message("💾 Save plots in the following formats:\n")
  message("  - PNG: High-quality images for publications or presentations.")
  message("  - JPG: Standard-quality images with smaller file sizes.")
  message("  - HTML: Interactive plots viewable in a browser.\n")
  
  # Step 0: Ask if the user wants to generate line plots
  repeat {
    generate_plots <- readline(prompt = "❓ Do you want to generate line plots? (yes/no): ")
    if (tolower(generate_plots) %in% c("yes", "y")) break
    if (tolower(generate_plots) %in% c("no", "n")) {
      message("❌ Lineplot generation skipped.")
      return(invisible(NULL))
    }
    message("⚠️ Invalid response. Please enter 'yes' or 'no'.")
  }
  
  # Validate input data structure
  message("🔍 Validating input data structure...\n")
  if (!"data.frame" %in% class(input_data)) stop("❌ Error: input_data must be a data frame!")
  required_columns <- c("start_rounded", "zone", "condition", "condition_grouped")
  if (!all(required_columns %in% colnames(input_data))) {
    stop("❌ Error: input_data must contain the following columns: ", paste(required_columns, collapse = ", "))
  }
  message("✔️ Data structure validated successfully.\n")
  
  # Ensure the conditions are ordered based on generated_condition_order
  if (exists("generated_condition_order", envir = .GlobalEnv)) {
    message("\n✔️ Using generated_condition_order to define the order of conditions.")
    input_data$condition <- factor(input_data$condition, levels = get("generated_condition_order", envir = .GlobalEnv))
  } else {
    stop("❌ 'generated_condition_order' does not exist in the global environment. Please define it before running this function.")
  }
  
  # Step 1: Remove acclimatation period if required
  repeat {
    keep_acclimatation <- readline(prompt = "❔ Do you want to keep the acclimatation period in the data? (yes/no): ")
    if (tolower(keep_acclimatation) %in% c("yes", "y")) {
      message("✔️ Acclimatation period kept in the data.\n")
      break
    }
    if (tolower(keep_acclimatation) %in% c("no", "n")) {
      input_data <- input_data %>% filter(!grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
      message("✔️ Acclimatation period removed from the data.\n")
      break
    }
    message("⚠️ Invalid response. Please enter 'yes' or 'no'.")
  }
  
  # Step 2: Define output directories
  html_path <- file.path(output_dir, "html")
  png_path <- file.path(output_dir, "png")
  jpg_path <- file.path(output_dir, "jpg")
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(jpg_path, recursive = TRUE, showWarnings = FALSE)
  message("✔️ Output directories created.\n")
  
  # Step 3: Manage colors
  condition_groups <- unique(input_data$condition_grouped)
  default_colors <- c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF")
  default_colors <- rep(default_colors, length.out = length(condition_groups))
  names(default_colors) <- condition_groups
  
  custom_colors <- list()
  for (group in condition_groups) {
    color_input <- readline(prompt = sprintf("🎨 Enter a color for '%s' (default: %s): ", group, default_colors[group]))
    custom_colors[[group]] <- if (color_input == "") default_colors[group] else color_input
  }
  message("✔️ Custom colors recorded.\n")
  
  generated_colors <- list()
  for (group in condition_groups) {
    group_conditions <- grep(paste0("^", group), unique(input_data$condition), value = TRUE)
    color_palette <- colorRampPalette(c(custom_colors[[group]], lighten(custom_colors[[group]], 0.4)))(length(group_conditions))
    names(color_palette) <- group_conditions
    generated_colors <- c(generated_colors, color_palette)
  }
  assign("custom_colors_global", custom_colors, envir = .GlobalEnv)
  assign("generated_colors_global", generated_colors, envir = .GlobalEnv)
  message("✔️ Colors saved as 'custom_colors_global' and 'generated_colors_global' in the global environment.\n")
  
  # Define themes
  light_theme <- theme_bw() %+replace% theme(
    plot.title      = element_text(color = "black", size = 14, hjust = 0.5),
    axis.text.y     = element_text(color = "black", size = 12),
    axis.text.x     = element_text(color = "black", size = 12),
    axis.title.x    = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y    = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position = "right",
    legend.text     = element_text(color = "black", size = 12, face = "italic"),
    legend.title    = element_blank(),
    strip.text.x    = element_text(size = 12),
    strip.background= element_rect(fill = "white"),
    plot.caption    = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
  )
  
  dark_theme <- theme_bw() %+replace% theme(
    plot.title       = element_text(color = "white", size = 14, hjust = 0.5),
    axis.text.y      = element_text(color = "white", size = 12),
    axis.text.x      = element_text(color = "white", size = 12),
    axis.title.x     = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y     = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position  = "right",
    legend.text      = element_text(color = "white", size = 12, face = "italic"),
    legend.title     = element_blank(),
    legend.background= element_rect(fill = "black", color = NA),
    legend.key       = element_rect(fill = "black", color = NA),
    strip.text.x     = element_text(size = 12, color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    plot.background  = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black", colour = "black"),
    panel.border     = element_rect(color = "white", fill = NA),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_line(color = "grey30"),
    plot.caption     = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
  )
  
  # Step 3: Generate plots
  response_vars <- grep("^sum_", colnames(input_data), value = TRUE)
  message("\n⏳ Generating plots... Please wait.")
  message("☕ Generating plots may take some time. Feel free to enjoy a coffee while you wait.\n")
  
  for (response_var in response_vars) {
    for (zone_number in unique(input_data$zone)) {
      zone_data <- input_data %>% filter(zone == zone_number)
      if (!all(c("start_rounded", response_var) %in% colnames(zone_data))) {
        message(sprintf("⚠️ Missing required columns for zone %s. Skipping...", zone_number))
        next
      }
      
      for (theme_name in c("light", "dark")) {
        current_theme <- if (theme_name == "light") light_theme else dark_theme
        vline_color <- if (theme_name == "light") "black" else "white"
        
        plot <- ggplot(zone_data, aes(
          x = start_rounded,
          y = .data[[response_var]],
          color = condition,
          group = condition
        )) +
          geom_point(size = 2) +
          geom_line(linewidth = 0.8) +
          geom_vline(
            xintercept = period_boundaries,
            linetype = "dashed",
            color = vline_color,
            alpha = 0.7
          ) +
          labs(x = "Time (minutes)", y = sprintf("%s (Zone %s)", response_var, zone_number)) +
          current_theme
        
        # Generate HTML for PNG and JPG rendering
        html_file <- file.path(html_path, sprintf("plot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
        interactive_plot <- ggplotly(plot)
        suppressMessages(saveWidget(interactive_plot, html_file, selfcontained = TRUE))
        
        # Save as PNG
        png_file <- file.path(png_path, sprintf("plot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
        suppressMessages(webshot2::webshot(html_file, file = png_file, vwidth = 1200, vheight = 900, zoom = 2))
        
        # Save as JPG
        jpg_file <- file.path(jpg_path, sprintf("plot_%s_zone_%s_%s.jpg", response_var, zone_number, theme_name))
        suppressMessages(webshot2::webshot(html_file, file = jpg_file, vwidth = 1200, vheight = 900, zoom = 2))
      }
    }
  }
  
  message("\n🎉 Lineplot generation completed successfully in PNG, JPG, and HTML formats!\n")
}


