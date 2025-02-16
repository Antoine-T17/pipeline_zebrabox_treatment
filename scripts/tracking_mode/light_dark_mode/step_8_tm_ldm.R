# -----------------------------------------------------------
# File: generate_and_save_lineplots.R
# -----------------------------------------------------------
# Harmonized version of the generate_and_save_lineplots function.
# This function generates line plots from the pretreated line plot data,
# validates the data structure, manages colors and themes, and saves plots
# in PNG and interactive HTML formats to specified directories.
# Interactive HTML plots are first saved to a temporary directory and then moved to the final destination.
# -----------------------------------------------------------

generate_and_save_lineplots <- function(input_data = get("pretreated_data_for_lineplots_df", envir = .GlobalEnv),
                                        output_dir = "outputs/tracking_mode/light_dark_mode/figures/lineplots") {
  tryCatch({
    message("\n---\n")
    message("👋 Welcome to the Lineplot Generation Process!")
    message("📋 This function will help you:")
    message("   • Generate high-quality line plots from your data.")
    message("   • Customize plot appearance using themes and colors.")
    message("   • Save plots in PNG and interactive HTML formats.\n")
    
    # Retrieve pre-recorded inputs from the global pipeline_inputs.
    pipeline_inputs <- get("pipeline_inputs", envir = .GlobalEnv)
    
    # Unified input helper.
    get_input_local <- function(param, prompt_msg, validate_fn = function(x) TRUE,
                                transform_fn = function(x) x,
                                error_msg = "❌ Invalid input. Please try again.") {
      if (!is.null(pipeline_inputs[[param]]) && pipeline_inputs[[param]] != "" &&
          !is.na(pipeline_inputs[[param]])) {
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
    
    # Step 0: Ask whether to generate line plots.
    generate_lines_plots <- get_input_local("generate_lines_plots",
                                            "❓ Do you want to generate line plots? (yes/no): ",
                                            validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                            transform_fn = function(x) tolower(trimws(x)),
                                            error_msg = "❌ Please enter 'yes' or 'no'.")
    if (generate_lines_plots %in% c("no", "n")) {
      message("❌ Lineplot generation skipped as per user input.")
      return(invisible(NULL))
    }
    
    # Step 0.5: Ask for output formats.
    generate_lines_plots_html <- get_input_local("generate_lines_plots_html",
                                                 "❓ Generate interactive HTML line plots? (yes/no): ",
                                                 validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                 transform_fn = function(x) tolower(trimws(x)),
                                                 error_msg = "❌ Please enter 'yes' or 'no'.")
    generate_lines_plots_png <- get_input_local("generate_lines_plots_png",
                                                "❓ Generate static PNG line plots? (yes/no): ",
                                                validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                transform_fn = function(x) tolower(trimws(x)),
                                                error_msg = "❌ Please enter 'yes' or 'no'.")
    
    if (generate_lines_plots_html %in% c("no", "n") && generate_lines_plots_png %in% c("no", "n")) {
      message("❌ No output format selected. Skipping lineplot generation.")
      return(invisible(NULL))
    }
    
    message("🔍 Validating input data structure...")
    if (!inherits(input_data, "data.frame")) {
      message("❌ input_data must be a data frame. Skipping lineplot generation.")
      return(invisible(NULL))
    }
    required_columns <- c("start_rounded", "zone", "condition", "condition_grouped")
    if (!all(required_columns %in% colnames(input_data))) {
      message("❌ input_data missing required columns: ", paste(required_columns, collapse = ", "), ". Skipping.")
      return(invisible(NULL))
    }
    message("✔️ Data structure validated.")
    
    # Order conditions.
    if (exists("generated_condition_order", envir = .GlobalEnv)) {
      message("✔️ Ordering conditions using 'generated_condition_order'.")
      input_data$condition <- factor(input_data$condition, levels = get("generated_condition_order", envir = .GlobalEnv))
    } else {
      message("⚠️ 'generated_condition_order' not found. Using default ordering.")
      input_data$condition <- factor(input_data$condition, levels = unique(input_data$condition))
    }
    
    # Step 1: Optionally remove acclimatation period.
    keep_acclimatation <- get_input_local("keep_acclimatation",
                                          "❓ Keep the acclimatation period? (yes/no): ",
                                          validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                          transform_fn = function(x) tolower(trimws(x)),
                                          error_msg = "❌ Please enter 'yes' or 'no'.")
    if (keep_acclimatation %in% c("no", "n") && "period_with_numbers" %in% colnames(input_data)) {
      input_data <- dplyr::filter(input_data, !grepl("acclimatation", period_with_numbers, ignore.case = TRUE))
      message("✔️ Acclimatation period removed.")
    } else {
      message("✔️ Acclimatation period retained.")
    }
    
    # Step 2: Define output directories.
    message("📁 Creating output directories for line boxplots...")
    html_dir <- "outputs/tracking_mode/light_dark_mode/figures/lineplots/html"
    png_dir  <- file.path(output_dir, "png")
    temp_dir <- tempdir()  # Use system temporary directory (short path)
    dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
    message("✔️ Output directories created.")
    
    # Step 3: Manage colors.
    message("🎨 Managing colors...")
    condition_groups <- unique(input_data$condition_grouped)
    default_colors <- rep(c("#FF6666", "#66B2FF", "#99CC33", "#FFCC33", "#CC66FF"), length.out = length(condition_groups))
    names(default_colors) <- condition_groups
    
    custom_color_input <- get_input_local("custom_color",
                                          "🎨 Enter custom colors for conditions (comma-separated), or press Enter for defaults: ",
                                          validate_fn = function(x) TRUE,
                                          transform_fn = function(x) {
                                            trimmed <- trimws(x)
                                            if (trimmed == "") return(character(0)) else split_and_trim(trimmed)
                                          })
    if (length(custom_color_input) > 0) {
      if (length(custom_color_input) != length(condition_groups)) {
        message("⚠️ Number of custom colors does not match condition groups. Using default colors.")
        custom_colors <- default_colors
      } else {
        custom_colors <- custom_color_input
        names(custom_colors) <- condition_groups
        message("✔️ Custom colors set: ", paste(custom_colors, collapse = ", "))
      }
    } else {
      custom_colors <- default_colors
      message("✔️ Using default colors: ", paste(custom_colors, collapse = ", "))
    }
    
    generated_colors <- list()
    if (!exists("lighten", mode = "function")) {
      lighten <- function(color, factor = 0.4) { color }
    }
    for (group in condition_groups) {
      group_conditions <- grep(paste0("^", group), unique(input_data$condition), value = TRUE)
      color_palette <- colorRampPalette(c(custom_colors[group], lighten(custom_colors[group], 0.4)))(length(group_conditions))
      names(color_palette) <- group_conditions
      generated_colors <- c(generated_colors, color_palette)
    }
    assign("custom_colors_global", custom_colors, envir = .GlobalEnv)
    assign("generated_colors_global", generated_colors, envir = .GlobalEnv)
    message("✔️ Colors saved globally.")
    
    # Define themes.
    light_theme <- theme_bw() %+replace% theme(
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
    dark_theme <- theme_bw() %+replace% theme(
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
    
    # Step 7: Prompt for output formats.
    generate_lines_plots_html <- get_input_local("generate_lines_plots_html",
                                                 "❓ Generate interactive HTML line plots? (yes/no): ",
                                                 validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                 transform_fn = function(x) tolower(trimws(x)),
                                                 error_msg = "❌ Please enter 'yes' or 'no'.")
    generate_lines_plots_png <- get_input_local("generate_lines_plots_png",
                                                "❓ Generate static PNG line plots? (yes/no): ",
                                                validate_fn = function(x) tolower(x) %in% c("yes", "y", "no", "n"),
                                                transform_fn = function(x) tolower(trimws(x)),
                                                error_msg = "❌ Please enter 'yes' or 'no'.")
    
    # Step 8: Generate line plots.
    response_vars <- grep("^sum_", colnames(input_data), value = TRUE)
    message("⏳ Generating line plots... This may take a moment.")
    for (response_var in response_vars) {
      for (zone_number in unique(input_data$zone)) {
        zone_data <- dplyr::filter(input_data, zone == zone_number)
        if (!all(c("start_rounded", response_var) %in% colnames(zone_data))) {
          message(sprintf("⚠️ Missing columns for zone %s. Skipping...", zone_number))
          next
        }
        for (theme_name in c("light", "dark")) {
          current_theme <- if (theme_name == "light") light_theme else dark_theme
          p <- ggplot(zone_data, aes(x = start_rounded, y = .data[[response_var]], color = condition, group = condition)) +
            geom_point(size = 2) +
            geom_line(linewidth = 0.8) +
            geom_vline(xintercept = period_boundaries, linetype = "dashed",
                       color = if (theme_name == "light") "black" else "white", alpha = 0.7) +
            labs(x = "Time (minutes)", y = sprintf("%s (Zone %s)", response_var, zone_number)) +
            current_theme
          
          if (tolower(generate_lines_plots_png) %in% c("yes", "y")) {
            tryCatch({
              png_file <- file.path(png_dir, sprintf("plot_%s_zone_%s_%s.png", response_var, zone_number, theme_name))
              ggsave(filename = png_file, plot = p, width = 12, height = 9, dpi = 300)
              message("✔️ PNG saved: ", png_file)
            }, error = function(e) {
              message("❌ Error saving PNG for ", response_var, ", zone ", zone_number, ", theme ", theme_name, ": ", e$message)
            })
          }
          if (tolower(generate_lines_plots_html) %in% c("yes", "y")) {
            tryCatch({
              temp_html <- file.path(temp_dir, sprintf("plot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
              final_html <- file.path(html_dir, sprintf("plot_%s_zone_%s_%s.html", response_var, zone_number, theme_name))
              suppressWarnings(saveWidget(plotly::ggplotly(p) %>% layout(boxmode = "group"), temp_html, selfcontained = TRUE))
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
    message("🎉 Lineplot generation completed!\n")
    
  }, error = function(e) {
    message("❌ Error in generate_and_save_lineplots: ", e$message)
    return(invisible(NULL))
  })
}
