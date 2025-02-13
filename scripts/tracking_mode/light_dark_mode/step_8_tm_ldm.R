generate_and_save_lineplots <- function(input_data = get("pretreated_data_for_lineplots_df", envir = .GlobalEnv),
                                        output_dir = "outputs/tracking_mode/light_dark_mode/figures/lineplots") {
  tryCatch({
    message("\n---\n---\n---\n")
    message("👋 Welcome to the Lineplot Generation Process!\n")
    message("This function helps you:")
    message("  • Generate high-quality line plots for your experimental data.")
    message("  • Customize visualization by selecting periods, colors, and plot themes.")
    message("  • Save plots in PNG and HTML formats.\n")
    
    # ---------------------------
    # Load pre-recorded inputs (if available)
    pipeline_inputs <- list()
    inputs_path <- "inputs/tracking_mode/light_dark_mode/inputs_values"
    inputs_file_xlsx <- file.path(inputs_path, "pipeline_inputs.xlsx")
    inputs_file_csv  <- file.path(inputs_path, "pipeline_inputs.csv")
    
    if (file.exists(inputs_file_xlsx)) {
      df <- readxl::read_excel(inputs_file_xlsx, sheet = 1)
      if (!all(c("parameters", "input") %in% colnames(df))) {
        message("❌ The pipeline_inputs.xlsx file must contain the columns 'parameters' and 'input'. Skipping lineplot generation.")
        return(invisible(NULL))
      }
      pipeline_inputs <- setNames(as.list(df$input), df$parameters)
    } else if (file.exists(inputs_file_csv)) {
      df <- read.csv2(inputs_file_csv, sep = ";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
      if (!all(c("parameters", "input") %in% colnames(df))) {
        message("❌ The pipeline_inputs.csv file must contain the columns 'parameters' and 'input'. Skipping lineplot generation.")
        return(invisible(NULL))
      }
      pipeline_inputs <- setNames(as.list(df$input), df$parameters)
    }
    
    # ---------------------------
    # Helper function to get input (from pre-recorded values or interactively)
    get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                                transform_fn = function(x) x,
                                error_msg = "Invalid input. Please try again.") {
      if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "" && !is.na(pipeline_inputs[[param]])) {
        candidate <- transform_fn(as.character(pipeline_inputs[[param]]))
        if (validate_fn(candidate)) {
          message("Using pre-recorded input for '", param, "': ", candidate)
          input_record_list[[param]] <<- paste(candidate, collapse = ", ")
          return(candidate)
        } else {
          message("Pre-recorded input for '", param, "' is invalid. Falling back to interactive prompt.")
        }
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
    
    # Helper to split and trim comma-separated strings.
    split_and_trim <- function(x) {
      trimws(unlist(strsplit(x, ",")))
    }
    
    # ---------------------------
    # Step 0: Ask whether to generate line plots.
    generate_lines_plots <- get_input_local(
      "generate_lines_plots",
      prompt_msg = "❓ Do you want to generate line plots? (yes/no): ",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Invalid response. Please enter 'yes' or 'no'."
    )
    if (generate_lines_plots %in% c("no", "n")) {
      message("❌ Lineplot generation skipped as per user input.")
      return(invisible(NULL))
    }
    
    # ---------------------------
    # Step 0.5: Ask for output formats.
    generate_lines_plots_html <- get_input_local(
      "generate_lines_plots_html",
      prompt_msg = "❓ Do you want to generate HTML line plots? (yes/no): ",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Invalid response. Please enter 'yes' or 'no'."
    )
    
    generate_lines_plots_png <- get_input_local(
      "generate_lines_plots_png",
      prompt_msg = "❓ Do you want to generate PNG line plots? (yes/no): ",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Invalid response. Please enter 'yes' or 'no'."
    )
    
    if (generate_lines_plots_html %in% c("no", "n") && generate_lines_plots_png %in% c("no", "n")) {
      message("❌ No output format selected. Skipping line plot generation.")
      return(invisible(NULL))
    }
    
    # ---------------------------
    # Validate input data structure.
    message("🔍 Validating input data structure...\n")
    if (!inherits(input_data, "data.frame")) {
      message("❌ Error: input_data must be a data frame! Skipping line plot generation.")
      return(invisible(NULL))
    }
    required_columns <- c("start_rounded", "zone", "condition", "condition_grouped")
    if (!all(required_columns %in% colnames(input_data))) {
      message("❌ Error: input_data must contain the following columns: ", paste(required_columns, collapse = ", "), ". Skipping line plot generation.")
      return(invisible(NULL))
    }
    message("✔️ Data structure validated successfully.")
    
    # Order conditions using global variable "generated_condition_order".
    if (exists("generated_condition_order", envir = .GlobalEnv)) {
      message("✔️ Using generated_condition_order to define the order of conditions.")
      input_data$condition <- factor(input_data$condition, levels = get("generated_condition_order", envir = .GlobalEnv))
    } else {
      message("⚠️ Warning: 'generated_condition_order' not found in global environment. Using default condition order.")
      input_data$condition <- factor(input_data$condition, levels = unique(input_data$condition))
    }
    
    # ---------------------------
    # Step 1: Check whether to keep the acclimatation period.
    keep_acclimatation <- get_input_local(
      "keep_acclimatation",
      prompt_msg = "❓ Do you want to keep the acclimatation period in the data? (yes/no): ",
      validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
      transform_fn = function(x) tolower(trimws(x)),
      error_msg = "⚠️ Invalid response. Please enter 'yes' or 'no'."
    )
    if (keep_acclimatation %in% c("no", "n")) {
      if ("period_with_numbers" %in% colnames(input_data)) {
        input_data <- dplyr::filter(input_data, !grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
        message("✔️ Acclimatation period removed from the data.")
      } else {
        message("⚠️ 'period_with_numbers' column not found. Skipping acclimatation filtering.")
      }
    } else {
      message("✔️ Acclimatation period kept in the data.\n")
    }
    
    # ---------------------------
    # Step 2: Define output directories.
    png_path  <- file.path(output_dir, "png")
    html_path <- file.path(output_dir, "html")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(png_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(html_path, recursive = TRUE, showWarnings = FALSE)
    message("✔️ Output directories created.")
    
    # ---------------------------
    # Step 3: Manage colors.
    message("🎨 Managing colors...")
    condition_groups <- unique(input_data$condition_grouped)
    default_colors <- c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF")
    default_colors <- rep(default_colors, length.out = length(condition_groups))
    names(default_colors) <- condition_groups
    
    custom_color_input <- get_input_local(
      "custom_color",
      prompt_msg = "🎨 Enter custom colors for conditions (comma-separated, e.g., #FF6666, #66B2FF, #99CC33), or press Enter to use defaults: ",
      validate_fn = function(x) TRUE,
      transform_fn = function(x) {
        trimmed <- trimws(x)
        if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
      }
    )
    
    if (length(custom_color_input) > 0) {
      if (length(custom_color_input) != length(condition_groups)) {
        message("⚠️ Warning: The number of custom colors (", length(custom_color_input),
                ") does not match the number of condition groups (", length(condition_groups), "). Using default colors.")
        custom_colors <- default_colors
      } else {
        custom_colors <- custom_color_input
        names(custom_colors) <- condition_groups
        message("✔️ Custom colors recorded: ", paste(custom_colors, collapse = ", "))
      }
    } else {
      custom_colors <- default_colors
      message("✔️ No custom colors provided; using default colors: ", paste(custom_colors, collapse = ", "))
    }
    
    generated_colors <- list()
    if (!exists("lighten", mode = "function")) {
      lighten <- function(color, factor = 0.4) {
        warning("lighten() function not found. Returning original color.")
        return(color)
      }
    }
    
    for (group in condition_groups) {
      group_conditions <- grep(paste0("^", group), unique(input_data$condition), value = TRUE)
      color_palette <- colorRampPalette(c(custom_colors[group],
                                          lighten(custom_colors[group], 0.4)))(length(group_conditions))
      names(color_palette) <- group_conditions
      generated_colors <- c(generated_colors, color_palette)
    }
    
    assign("custom_colors_global", custom_colors, envir = .GlobalEnv)
    assign("generated_colors_global", generated_colors, envir = .GlobalEnv)
    message("✔️ Colors saved as 'custom_colors_global' and 'generated_colors_global' in the global environment.")
    
    # ---------------------------
    # Define themes for plots.
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
    
    # ---------------------------
    # Step 4: Generate plots.
    response_vars <- grep("^sum_", colnames(input_data), value = TRUE)
    message("⏳ Generating plots... Please wait.")
    message("☕ Plot generation may take some time. Enjoy a coffee while you wait.")
    
    for (response_var in response_vars) {
      for (zone_number in unique(input_data$zone)) {
        zone_data <- dplyr::filter(input_data, zone == zone_number)
        
        if (!all(c("start_rounded", response_var) %in% colnames(zone_data))) {
          message(sprintf("⚠️ Missing required columns for zone %s. Skipping...", zone_number))
          next
        }
        
        for (theme_name in c("light", "dark")) {
          current_theme <- if (theme_name == "light") light_theme else dark_theme
          
          p <- ggplot(zone_data, aes(
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
              color = if (theme_name == "light") "black" else "white",
              alpha = 0.7
            ) +
            labs(x = "Time (minutes)", y = sprintf("%s (Zone %s)", response_var, zone_number)) +
            current_theme
          
          # Save as PNG if requested.
          if (generate_lines_plots_png %in% c("yes", "y")) {
            png_file <- file.path(png_path, sprintf("plot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
            ggsave(filename = png_file, plot = p, width = 12, height = 9, dpi = 300)
            message("✔️ PNG saved: ", png_file)
          }
          
          # Save as an interactive HTML file if requested.
          if (generate_lines_plots_html %in% c("yes", "y")) {
            interactive_plot <- plotly::ggplotly(p)
            html_file <- file.path(html_path, sprintf("plot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
            htmlwidgets::saveWidget(interactive_plot, html_file, selfcontained = TRUE)
            message("✔️ HTML saved: ", html_file)
          }
        }
      }
    }
    
    message("🎉 Lineplot generation completed successfully in the selected formats!\n")
    
  }, error = function(e) {
    message("❌ Error in generate_and_save_lineplots: ", e$message)
    return(invisible(NULL))
  })
}
