generate_figures_and_tables <- function(
    lineplot_list    = get("all_zone_combined_lineplots", envir = .GlobalEnv),
    boxplot_list     = get("all_zone_combined_light_dark_boxplots", envir = .GlobalEnv),
    output_base_dir  = "outputs/tracking_mode/light_dark_mode",
    condition_order  = condition_grouped_order
) {
  # UNIFIED WELCOME MESSAGE
  message("\n---\n")
  message("👋 Welcome to the Figure and Table Generation Process!")
  message("📋 This function will:")
  message("   • Generate PNG lineplots from 'all_zone_combined_lineplots'.")
  message("   • Generate PNG boxplots from 'all_zone_combined_light_dark_boxplots'.")
  message("   • Save each plot under ", output_base_dir, "\n")
  
  # TAG: HELPER FUNCTIONS
  
  get_input_local <- function(param, prompt_msg,
                              validate_fn = function(x) TRUE,
                              transform_fn  = function(x) x,
                              error_msg    = "❌ Invalid input. Please try again.") {
    if (!is.null(input_record_list[[param]])) {
      message("💾 Using cached input for '", param, "'.")
      return(input_record_list[[param]])
    }
    if (!is.null(pipeline_inputs[[param]]) &&
        !is.na(pipeline_inputs[[param]]) &&
        pipeline_inputs[[param]] != "") {
      raw       <- pipeline_inputs[[param]]
      candidate <- transform_fn(raw)
      if (validate_fn(raw)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("💾 Using pre-recorded input for '", param, "': ",
                  paste(candidate, collapse = ", "))
        } else {
          message("💾 Using pre-recorded input for '", param, "': ", candidate)
        }
        return(candidate)
      } else {
        message("⚠️ Pre-recorded input for '", param,
                "' is invalid. Switching to interactive prompt.")
      }
    }
    repeat {
      user_input <- readline(prompt = prompt_msg)
      candidate  <- transform_fn(user_input)
      if (validate_fn(user_input)) {
        input_record_list[[param]] <<- candidate
        # collapse multiple values with commas
        if (is.character(candidate) && length(candidate) > 1) {
          message("✔️ Input for '", param, "' recorded: ",
                  paste(candidate, collapse = ", "))
        } else {
          message("✔️ Input for '", param, "' recorded: ", candidate)
        }
        return(candidate)
      }
      message(error_msg)
    }
  }
  
  # TAG: INPUT COLLECTION
  
  gen_choice <- get_input_local(
    "generate_figures_and_or_tables",
    "❓ What do you want to generate? (none, figures, tables, figures and tables): ",
    validate_fn  = function(x) {
      x <- tolower(trimws(x))
      x %in% c("none", "figures", "tables", "figures and tables")
    },
    transform_fn = function(x) tolower(trimws(x)),
    error_msg    = "❌ Enter exactly: none, figures, tables, or figures and tables."
  )
  
  do_figures <- grepl("figure", gen_choice)
  do_tables  <- grepl("table",  gen_choice)
  
  do_png <- FALSE
  do_html <- FALSE
  
  # early‐exit if "none"v
  if (!do_figures && !do_tables) {
    message("-----")
    message("❌ Nothing to generate. Exiting.")
    message("-----")
    
    return(invisible(NULL))
  }
  
  if (do_figures) {
    fig_fmt <- get_input_local(
      "figures_format",
      "❓ In which format? (png, html, png and html): ",
      validate_fn  = function(x) {
        x <- tolower(trimws(x))
        x %in% c("png", "html", "png and html")
      },
      transform_fn = function(x) tolower(trimws(x)),
      error_msg    = "❌ Enter exactly: png, html, or png and html."
    )
    do_png  <- grepl("png",  fig_fmt)
    do_html <- grepl("html", fig_fmt)
  }
  
  condition_grouped_order <- get_input_local(
    "condition_grouped_order",
    "❓ Enter the sequence you want (comma-separated), e.g. Control,1 ppm,10 ppm: ",
    validate_fn = function(x) {
      parts <- trimws(unlist(strsplit(x, ",")))
      length(parts) > 0 && all(nzchar(parts))
    },
    transform_fn = function(x) {
      trimws(unlist(strsplit(x, ",")))
    },
    error_msg = "❌ Please enter at least one condition, separated by commas."
  )
  
  condition_grouped_color <- get_input_local(
    "condition_grouped_color",
    sprintf(
      "❓ Enter one HTML hex color per condition (total %d), comma-separated: ",
      length(condition_grouped_order)
    ),
    validate_fn = function(raw) {
      cols <- trimws(unlist(strsplit(raw, ",")))
      # must match count *and* all be valid hex codes
      length(cols) == length(condition_grouped_order) &&
        all(grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", cols))
    },
    transform_fn = function(raw) {
      trimws(unlist(strsplit(raw, ",")))
    },
    error_msg = sprintf(
      "❌ You must enter exactly %d valid HTML hex codes (one per condition).",
      length(condition_grouped_order)
    )
  )
  
  boxplot_light_dark_mode <- get_input_local(
    "boxplot_light_dark_mode",
    "❓ Boxplot mode? Type 'separated' for facet_wrap by period, or 'pooled' for side-by-side boxplots: ",
    validate_fn = function(x) x %in% c("separated", "pooled"),
    transform_fn = function(x) tolower(trimws(x)),
    error_msg = "❌ Please type exactly 'separated' or 'pooled'."
  )
  
  if (boxplot_light_dark_mode == "pooled") {
    # Collect exactly two hex‐colors for Light vs. Dark periods
    boxplot_light_dark_periods_colors <- get_input_local(
      "boxplot_light_dark_periods_colors",
      "❓ Enter exactly 2 HTML hex colors (Light period, Dark period), comma-separated: ",
      validate_fn = function(raw) {
        cols <- trimws(unlist(strsplit(raw, ",")))
        length(cols) == 2 &&
          all(grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", cols))
      },
      transform_fn = function(raw) {
        trimws(unlist(strsplit(raw, ",")))
      },
      error_msg = "❌ Need exactly 2 valid hex codes, e.g. #1f77b4,#ff7f0e"
    )
    names(boxplot_light_dark_periods_colors) <- c("Light period", "Dark period")
  }

  # HELPER: ensure output directories exist
  ensure_directory <- function(dir_path) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message("✔️ Created directory: ", dir_path)
    }
  }

  # THEMES
  light_theme <- function(base_size = 11, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
      plot.title       = element_text(color = "black", size = 14, hjust = 0.5),
      axis.text.y      = element_text(color = "black", size = 12),
      axis.text.x      = element_text(color = "black", size = 12),
      axis.title.x     = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
      axis.title.y     = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
      legend.position  = "right",
      legend.text      = element_text(color = "black", size = 12, face = "italic"),
      legend.title     = element_blank(),
      strip.text.x     = element_text(size = 12),
      strip.background = element_rect(fill = "white"),
      plot.caption     = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
    )
  }
  dark_theme <- function(base_size = 11, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
      plot.title         = element_text(color = "white", size = 14, hjust = 0.5),
      axis.text.y        = element_text(color = "white", size = 12),
      axis.text.x        = element_text(color = "white", size = 12),
      axis.title.x       = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
      axis.title.y       = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
      legend.position    = "right",
      legend.text        = element_text(color = "white", size = 12, face = "italic"),
      legend.title       = element_blank(),
      legend.background  = element_rect(fill = "black"),
      legend.key         = element_rect(fill = "black"),
      strip.text.x       = element_text(color = "white", size = 12),
      strip.background   = element_rect(fill = "black", color = "white"),
      plot.background    = element_rect(fill = "black"),
      panel.background   = element_rect(fill = "black"),
      panel.border       = element_rect(color = "white", fill = NA),
      panel.grid.major   = element_line(color = "grey30"),
      panel.grid.minor   = element_line(color = "grey30"),
      plot.caption       = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
    )
  }

  # INITIALIZE DIRECTORIES
  
  figure_dirs <- character()
  if (do_png) {
    figure_dirs <- c(
      figure_dirs,
      file.path(output_base_dir, "figures/lineplots/png/light_theme"),
      file.path(output_base_dir, "figures/lineplots/png/dark_theme"),
      file.path(output_base_dir, "figures/boxplots_light_dark/png/light_theme"),
      file.path(output_base_dir, "figures/boxplots_light_dark/png/dark_theme"),
      file.path(output_base_dir, "figures/boxplots_cum/png/light_theme"),
      file.path(output_base_dir, "figures/boxplots_cum/png/dark_theme"),
      file.path(output_base_dir, "figures/heatmap_cum/png/light_theme"),  
      file.path(output_base_dir, "figures/heatmap_cum/png/dark_theme"),   
      file.path(output_base_dir, "figures/boxplots_delta/png/light_theme"),
      file.path(output_base_dir, "figures/boxplots_delta/png/dark_theme")
    )
  }
  if (do_html) {
    figure_dirs <- c(
      figure_dirs,
      file.path(output_base_dir, "figures/lineplots/html/light_theme"),
      file.path(output_base_dir, "figures/lineplots/html/dark_theme"),
      file.path(output_base_dir, "figures/boxplots_light_dark/html/light_theme"),
      file.path(output_base_dir, "figures/boxplots_light_dark/html/dark_theme"),
      file.path(output_base_dir, "figures/boxplots_cum/html/light_theme"),
      file.path(output_base_dir, "figures/boxplots_cum/html/dark_theme"),
      file.path(output_base_dir, "figures/heatmap_cum/html/light_theme"),  
      file.path(output_base_dir, "figures/heatmap_cum/html/dark_theme"),   
      file.path(output_base_dir, "figures/boxplots_delta/html/light_theme"),
      file.path(output_base_dir, "figures/boxplots_delta/html/dark_theme")
    )
  }
  lapply(unique(figure_dirs), ensure_directory)
  
  if (do_tables) {
    table_dir <- file.path(output_base_dir, "tables")
    ensure_directory(table_dir)
  }
  
dirs <- figure_dirs

  
  # —————————————————————————————
  # FIGURE GENERATION
  # —————————————————————————————
  if (do_figures) {
    # --------------------
    # BOXPLOT LIGHT DARK GENERATION
    # --------------------
    message("-----")
    message("🔄 Generating light dark boxplots (light period, dark period)")
    message("-----")
    
    cap_text_light_dark_boxplots <- str_wrap("Each point corresponds to the mean of the response variable for one animal according to the selected period.",
                                             width = 60)
    
    for (var in names(boxplot_list)) {
      df <- boxplot_list[[var]]
      req <- c("condition_grouped", "condition_tagged",
               "period_without_numbers", "zone", "mean_val")
      if (any(!req %in% colnames(df))) {
        message("⚠️ Skipping boxplot '", var, "': missing ",
                paste(setdiff(req, colnames(df)), collapse = ", "))
        next
      }
      
      # factorize condition and period
      df$condition_grouped <- factor(df$condition_grouped, levels = condition_order)
      df$period_without_numbers <- factor(df$period_without_numbers, levels = c("light", "dark"), labels = c("Light period", "Dark period"))
      
      for (z in sort(unique(df$zone))) {
        sub <- subset(df, zone == z)
        for (th in c("light", "dark")) {
          theme_obj <- if (th == "light") light_theme() else dark_theme()
          edge_col  <- if (th == "light") "black" else "white"
          dir_index <- ifelse(th == "light", 3, 4)
          dir_path  <- dirs[[dir_index]]
          fname_png  <- sprintf("boxplot_%s_zone_%s_%s.png", var, z, th)
          html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
          fname_html <- sub("\\.png$", ".html", fname_png)
          
          if (boxplot_light_dark_mode == "separated") {
            
            # ─── PNG version ───
            if (do_png) {
              p_png <- ggplot(sub, aes(
                x    = condition_grouped,
                y    = mean_val,
                fill = condition_grouped
              )) +
                geom_boxplot(outlier.shape = NA, alpha = 0.6, color = edge_col) +
                geom_jitter(aes(color = condition_tagged),
                            position = position_jitterdodge(0.2, 0.2),
                            size = 1.75, alpha = 0.4, color = edge_col) +
                facet_wrap(~period_without_numbers, scales = "free_x") +
                scale_fill_manual(values = condition_grouped_color) +
                labs(
                  x       = "Condition",
                  y       = sprintf("%s (Zone %s)", var, z),
                  caption = cap_text_light_dark_boxplots
                ) +
                theme_obj +
                theme(
                  plot.caption.position = "plot",
                  plot.caption          = element_text(hjust = 1),
                  legend.position       = "none"
                )
              
              tryCatch({
                ggsave(
                  filename = file.path(dir_path, fname_png),
                  plot     = p_png,
                  width    = 8,
                  height   = 6,
                  dpi      = 300
                )
                message("✔️ PNG: ", fname_png)
              }, error = function(e) {
                message("❌ PNG failed: ", e$message)
              })
            }
            
            # ─── HTML version ───
            if (do_html) {
              html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
              html_fname <- sub("\\.png$", ".html", fname_png)
              final_html <- file.path(html_dir, html_fname)
              
              # temporary file in tempdir()
              temp_html  <- file.path(tempdir(), html_fname)
              
              ensure_directory(html_dir)
              
              p_html <- ggplot(sub, aes(
                x    = condition_grouped,
                y    = mean_val,
                fill = condition_grouped,
                text = paste0(
                  "Tag: ", condition_tagged,
                  "<br>Well: ", animal,
                  "<br>Plate ID: ", plate_id,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              )) +
                geom_boxplot(outlier.shape = NA, alpha = 0.6, color = edge_col) +
                geom_jitter(aes(color = condition_tagged),
                            position = position_jitterdodge(0.2, 0.2),
                            size = 1.75, alpha = 0.4, color = edge_col) +
                facet_wrap(~period_without_numbers, scales = "free_x") +
                scale_fill_manual(values = condition_grouped_color) +
                labs(
                  x       = "Condition",
                  y       = sprintf("%s (Zone %s)", var, z),
                  caption = cap_text_light_dark_boxplots
                ) +
                theme_obj +
                theme(
                  plot.caption.position = "plot",
                  plot.caption          = element_text(hjust = 1),
                  legend.position       = "none"
                )
              
              tryCatch({
                w <- ggplotly(p_html, tooltip = "text") %>% layout(boxmode = "group")
                htmlwidgets::saveWidget(
                  widget        = w,
                  file          = temp_html,
                  selfcontained = TRUE
                )
                file.copy(temp_html, final_html, overwrite = TRUE)
                file.remove(temp_html)
                message("✔️ HTML: ", html_fname)
              }, error = function(e) {
                message("❌ HTML failed: ", e$message)
              })
            }
            
          } else {
            # pooled mode: side-by-side light/dark per condition
            
            # ─── PNG version ───
            if (do_png) {
              p_png <- ggplot(sub, aes(
                x    = condition_grouped,
                y    = mean_val,
                fill = period_without_numbers
              )) +
                geom_boxplot(
                  position      = position_dodge(width = 0.8),
                  outlier.shape = NA,
                  alpha         = 0.6,
                  width         = 0.7,
                  color         = edge_col
                ) +
                geom_jitter(
                  aes(fill = period_without_numbers),
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                  shape    = 21,
                  color    = edge_col,
                  size     = 1.75,
                  alpha    = 0.4
                ) +
                scale_fill_manual(values = boxplot_light_dark_periods_colors) +
                labs(
                  x       = "Condition",
                  y       = sprintf("%s (Zone %s)", var, z),
                  fill    = "Period",
                  caption = cap_text_light_dark_boxplots
                ) +
                theme_obj +
                theme(
                  plot.caption.position = "plot",
                  plot.caption          = element_text(hjust = 1),
                  legend.position       = "right"
                )
              
              tryCatch({
                ggsave(
                  filename = file.path(dir_path, fname_png),
                  plot     = p_png,
                  width    = 8,
                  height   = 6,
                  dpi      = 300
                )
                message("✔️ PNG: ", fname_png)
              }, error = function(e) {
                message("❌ PNG failed: ", e$message)
              })
            }
            
            # ─── HTML version ───
            if (do_html) {
              html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
              html_fname <- sub("\\.png$", ".html", fname_png)
              final_html <- file.path(html_dir, html_fname)
              
              # temporary file in tempdir()
              temp_html  <- file.path(tempdir(), html_fname)
              
              ensure_directory(html_dir)
              
              p_html <- ggplot(sub, aes(
                x    = condition_grouped,
                y    = mean_val,
                fill = period_without_numbers,
                text = paste0(
                  "Tag: ", condition_tagged,
                  "<br>Well: ", animal,
                  "<br>Plate ID: ", plate_id,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              )) +
                geom_boxplot(
                  position      = position_dodge(width = 0.8),
                  outlier.shape = NA,
                  alpha         = 0.6,
                  width         = 0.7,
                  color         = edge_col
                ) +
                geom_jitter(
                  aes(fill = period_without_numbers),
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                  shape    = 21,
                  color    = edge_col,
                  size     = 1.75,
                  alpha    = 0.4
                ) +
                scale_fill_manual(values = boxplot_light_dark_periods_colors) +
                labs(
                  x       = "Condition",
                  y       = sprintf("%s (Zone %s)", var, z),
                  fill    = "Period",
                  caption = cap_text_light_dark_boxplots
                ) +
                theme_obj +
                theme(
                  plot.caption.position = "plot",
                  plot.caption          = element_text(hjust = 1),
                  legend.position       = "right"
                )
              
              tryCatch({
                w <- ggplotly(p_html, tooltip = "text") %>% layout(boxmode = "group")
                htmlwidgets::saveWidget(
                  widget        = w,
                  file          = temp_html,
                  selfcontained = TRUE
                )
                file.copy(temp_html, final_html, overwrite = TRUE)
                file.remove(temp_html)
                message("✔️ HTML: ", html_fname)
              }, error = function(e) {
                message("❌ HTML failed: ", e$message)
              })
            }
            
          }
        }
      }
    }
  
    # --------------------
    # DELTA BOXPLOT GENERATION
    # --------------------
    message("-----")
    message("🔄 Generating delta boxplots (before-switch-after)")
    message("-----")
    
    # palette pour before/switch/after
    colors <- c(before = "#1f77b4", switch = "#ff7f0e", after = "#2ca02c")
    
    for (var in names(all_zone_combined_delta_boxplots)) {
      zone_data <- all_zone_combined_delta_boxplots[[var]]
      
      req <- c("condition_grouped", "zone", "momentum", "mean_val")
      if (any(!req %in% colnames(zone_data))) {
        message("⚠️ Skipping delta-boxplot '", var, "': missing ",
                paste(setdiff(req, colnames(zone_data)), collapse = ", "))
        next
      }
      
      cap_text_delta_boxplot <- str_wrap("Each point corresponds to one animal. The value of each point represents the mean of the response variable for one animal over the selected time interval.", 
                                         width = 100)
      
      # fixer l’ordre des conditions
      zone_data$condition_grouped <- factor(zone_data$condition_grouped, levels = condition_order)
      zone_data$momentum <- factor(zone_data$momentum, levels = c("before", "switch", "after"))
      
      for (z in sort(unique(zone_data$zone))) {
        sub <- subset(zone_data, zone == z)
        for (th in c("light", "dark")) {
          theme_obj <- if (th == "light") light_theme() else dark_theme()
          edge_col  <- if (th == "light") "black" else "white"
          dir       <- dirs[[ ifelse(th == "light", 9, 10) ]]
          fname_png  <- sprintf("delta_boxplot_%s_zone_%s_%s.png", var, z, th)
          html_dir   <- sub("/png/", "/html/", dir, fixed = TRUE)
          fname_html <- sub("\\.png$", ".html", fname_png)
          
          # ─── PNG version ───
          if (do_png) {
            p_png <- ggplot(sub, aes(
              x    = condition_grouped,
              y    = mean_val,
              fill = momentum
            )) +
              geom_boxplot(
                position      = position_dodge(width = 0.85),
                outlier.shape = NA,
                alpha         = 0.6,
                color         = edge_col
              ) +
              geom_point(
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.85),
                shape    = 21,
                size     = 1.75,
                alpha    = 0.4,
                color    = edge_col
              ) +
              labs(
                x       = "Condition",
                y       = sprintf("%s (Zone %s)", var, z),
                fill    = "Momentum",
                caption = cap_text_delta_boxplot
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       # put caption in outer margin
                plot.caption          = element_text(hjust = 1, margin = margin(t = 10))
              ) +
              scale_fill_manual(values = colors)
            
            tryCatch({
              ggsave(
                filename = file.path(dir, fname_png),
                plot     = p_png,
                width    = 8,
                height   = 6,
                dpi      = 300
              )
              message("✔️ PNG: ", fname_png)
            }, error = function(e) {
              message("❌ PNG failed: ", e$message)
            })
          }
          
          # ─── HTML version ───
          if (do_html) {
            html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
            html_fname <- sub("\\.png$", ".html", fname_png)
            final_html <- file.path(html_dir, html_fname)
            
            # temporary file in tempdir()
            temp_html  <- file.path(tempdir(), html_fname)
            
            ensure_directory(html_dir)
            
            p_html <- ggplot(sub, aes(
              x    = condition_grouped,
              y    = mean_val,
              fill = momentum,
              text = paste0(
                "Tag: ", condition_tagged,
                "<br> Well: ", animal,
                "<br> Plate ID: ", plate_id,
                "<br> Value: ", sprintf("%.2f", mean_val)
              )
            )) +
              geom_boxplot(
                position      = position_dodge(width = 0.85),
                outlier.shape = NA,
                alpha         = 0.6,
                color         = edge_col
              ) +
              geom_point(
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.85),
                shape    = 21,
                size     = 1.75,
                alpha    = 0.4,
                color    = edge_col
              ) +
              labs(
                x       = "Condition",
                y       = sprintf("%s (Zone %s)", var, z),
                fill    = "Momentum",
                caption = cap_text_delta_boxplot
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       # put caption in outer margin
                plot.caption          = element_text(hjust = 1, margin = margin(t = 10))
              ) +
              scale_fill_manual(values = colors)
            
            tryCatch({
              w <- ggplotly(p_html, tooltip = "text") %>% layout(boxmode = "group")
              htmlwidgets::saveWidget(
                widget        = w,
                file          = temp_html,
                selfcontained = TRUE
              )
              file.copy(temp_html, final_html, overwrite = TRUE)
              file.remove(temp_html)
              message("✔️ HTML: ", html_fname)
            }, error = function(e) {
              message("❌ HTML failed: ", e$message)
            })
          }
        }
      }
    }
      
    # --------------------
    # CUMULATIVE PER-WELL BOXPLOT GENERATION
    # --------------------
    message("-----")
    message("🔄 Generating cumulative boxplots (no difference between light and dark periods)")
    message("-----")
    
    cap_text_cum_boxplots <- str_wrap("Each point represents the total response for a single animal (summed across all periods), then normalized by the number of wells in that condition.",
                                      width = 100)
    
    for (var in names(all_zone_combined_cum_boxplots)) {
      df <- all_zone_combined_cum_boxplots[[var]]
      req <- c("condition_grouped", "zone", "cum_per_well")
      if (any(!req %in% colnames(df))) {
        message("⚠️ Skipping cum-boxplot '", var, "': missing ",
                paste(setdiff(req, colnames(df)), collapse = ", "))
        next
      }
      # On fixe l’ordre des conditions
      df$condition_grouped <- factor(df$condition_grouped, levels = condition_order)
      
      for (z in sort(unique(df$zone))) {
        sub <- subset(df, zone == z)
        for (th in c("light", "dark")) {
          theme_obj <- if (th == "light") light_theme() else dark_theme()
          edge_col  <- if (th == "light") "black" else "white"
          dir       <- dirs[[ ifelse(th == "light", 5, 6) ]]
          fname_png  <- sprintf("cum_boxplot_%s_zone_%s_%s.png", var, z, th)
          html_dir   <- sub("/png/", "/html/", dir, fixed = TRUE)
          fname_html <- sub("\\.png$", ".html", fname_png)
          
          # ─── PNG version ───
          if (do_png) {
            p_png <- ggplot(sub, aes(
              x    = condition_grouped,
              y    = cum_per_well,
              fill = condition_grouped
            )) +
              geom_boxplot(outlier.shape = NA, alpha = 0.6, color = edge_col) +
              geom_jitter(aes(color = condition_tagged),
                          position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                          size = 1.75, alpha = 0.4, color = edge_col) +
              labs(
                x       = "Condition",
                y       = sprintf("%s (Zone %s)", var, z),
                fill    = "Condition",
                caption = cap_text_cum_boxplots
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       # put caption in outer margin
                plot.caption          = element_text(hjust = 1), # right-align it
                legend.position       = "none"
              ) +
              guides(fill = "none") +
              scale_fill_manual(values = condition_grouped_color)
            
            tryCatch({
              ggsave(
                filename = file.path(dir, fname_png),
                plot     = p_png,
                width    = 8,
                height   = 6,
                dpi      = 300
              )
              message("✔️ PNG: ", fname_png)
            }, error = function(e) {
              message("❌ PNG failed: ", e$message)
            })
          }
          
          # ─── HTML version ───
          if (do_html) {
            html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
            html_fname <- sub("\\.png$", ".html", fname_png)
            final_html <- file.path(html_dir, html_fname)
            
            # temporary file in tempdir()
            temp_html  <- file.path(tempdir(), html_fname)
            
            ensure_directory(html_dir)
            
            p_html <- ggplot(sub, aes(
              x    = condition_grouped,
              y    = cum_per_well,
              fill = condition_grouped,
              text = paste0(
                "Tag: ", condition_tagged,
                "<br> Well: ", animal,
                "<br> Plate ID: ", plate_id,
                "<br> Value: ", sprintf("%.2f", cum_per_well)
              )
            )) +
              geom_boxplot(outlier.shape = NA, alpha = 0.6, color = edge_col) +
              geom_jitter(aes(color = condition_tagged),
                          position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                          size = 1.75, alpha = 0.4, color = edge_col) +
              labs(
                x       = "Condition",
                y       = sprintf("%s (Zone %s)", var, z),
                fill    = "Condition",
                caption = cap_text_cum_boxplots
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       
                plot.caption          = element_text(hjust = 1)
              ) +
              guides(fill = "none") +
              scale_fill_manual(values = condition_grouped_color)
            
            tryCatch({
              w <- ggplotly(p_html, tooltip = "text") %>% layout(boxmode = "group")
              htmlwidgets::saveWidget(
                widget        = w,
                file          = temp_html,
                selfcontained = TRUE
              )
              file.copy(temp_html, final_html, overwrite = TRUE)
              file.remove(temp_html)
              message("✔️ HTML: ", html_fname)
            }, error = function(e) {
              message("❌ HTML failed: ", e$message)
            })
          }
          
        }
      }
    }
     
    # --------------------
    # LINEPLOT GENERATION
    # --------------------
    message("-----")
    message("🔄 Generating lineplots")
    message("-----")
    
    cap_text_lineplot <- str_wrap("Each point represents the summed response across all animals in a given condition (over the aggregation period), divided by the number of wells in that condition.",
                                  width = 100)
    
    for (var in names(lineplot_list)) {
      df <- lineplot_list[[var]]
      req <- c("start_rounded", "zone", "condition_grouped", "val_per_well")
      if (any(!req %in% colnames(df))) {
        message("⚠️ Skipping lineplot '", var, "': missing ",
                paste(setdiff(req, colnames(df)), collapse = ", "))
        next
      }
      
      df$condition_grouped <- factor(df$condition_grouped, levels = condition_order)
      
      for (z in sort(unique(df$zone))) {
        sub <- subset(df, zone == z)
        for (th in c("light", "dark")) {
          theme_obj <- if (th == "light") light_theme() else dark_theme()
          dir       <- dirs[[ ifelse(th == "light", 1, 2) ]]
          fname_png  <- sprintf("lineplot_%s_zone_%s_%s.png", var, z, th)
          html_dir   <- sub("/png/", "/html/", dir, fixed = TRUE)
          fname_html <- sub("\\.png$", ".html", fname_png)
          
          # ─── PNG version ───
          if (do_png) {
            p_png <- ggplot(sub, aes(
              x     = start_rounded,
              y     = val_per_well,
              color = condition_grouped,
              group = condition_grouped
            )) +
              geom_line(linewidth = 0.8) +
              geom_point(size = 1.75) +
              labs(
                x       = "Time (rounded minutes)",
                y       = sprintf("%s (Zone %s)", var, z),
                caption = cap_text_lineplot
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       
                plot.caption          = element_text(hjust = 1, margin = margin(t = 10))
              ) +
              scale_color_manual(values = condition_grouped_color)
            
            tryCatch({
              ggsave(
                filename = file.path(dir, fname_png),
                plot     = p_png,
                width    = 8,
                height   = 6,
                dpi      = 300
              )
              message("✔️ PNG: ", fname_png)
            }, error = function(e) {
              message("❌ Error saving PNG ", fname_png, ": ", e$message)
            })
          }
          
          # ─── HTML version ───
          if (do_html) {
            html_dir   <- sub("/png/", "/html/", dir_path, fixed = TRUE)
            html_fname <- sub("\\.png$", ".html", fname_png)
            final_html <- file.path(html_dir, html_fname)
            
            # temporary file in tempdir()
            temp_html  <- file.path(tempdir(), html_fname)
            
            ensure_directory(html_dir)
            
            p_html <- ggplot(sub, aes(
              x     = start_rounded,
              y     = val_per_well,
              color = condition_grouped,
              group = condition_grouped,
              text  = paste0(
                "Tag: ", condition_grouped,
                "<br> Time: ", start_rounded,
                "<br> Total well: ", n_wells,
                "<br> Value: ", sprintf("%.2f", val_per_well)
              )
            )) +
              geom_line(linewidth = 0.8) +
              geom_point(size = 1.75) +
              labs(
                x       = "Time (rounded minutes)",
                y       = sprintf("%s (Zone %s)", var, z),
                caption = cap_text_lineplot
              ) +
              theme_obj +
              theme(
                plot.caption.position = "plot",       
                plot.caption          = element_text(hjust = 1, margin = margin(t = 10))
              ) +
              scale_color_manual(values = condition_grouped_color)
            
            tryCatch({
              w <- ggplotly(p_html, tooltip = "text") %>% layout(boxmode = "group")
              htmlwidgets::saveWidget(
                widget        = w,
                file          = temp_html,
                selfcontained = TRUE
              )
              file.copy(temp_html, final_html, overwrite = TRUE)
              file.remove(temp_html)
              message("✔️ HTML: ", html_fname)
            }, error = function(e) {
              message("❌ HTML failed: ", e$message)
            })
          }
        }
      }
    }
      
    # --------------------
    # HEATMAP GENERATION
    # --------------------
    message("-----")
    message("🔄 Generating heatmaps")
    message("-----")
    
    for (var in names(all_zone_combined_cum_heatmap)) {
      df <- all_zone_combined_cum_heatmap[[var]]
      req <- c("condition_grouped", "zone", "val_per_well")
      if (any(!req %in% colnames(df))) {
        message("⚠️ Skipping heatmap '", var, "': missing ",
                paste(setdiff(req, colnames(df)), collapse = ", "))
        next
      }
      # on fixe l’ordre des conditions
      df$condition_grouped <- factor(df$condition_grouped, levels = condition_order)
      
      for (th in c("light", "dark")) {
        theme_obj <- if (th == "light") light_theme() else dark_theme()
        
        p <- ggplot(df, aes(x = condition_grouped, y = factor(zone), fill = val_per_well)) +
          geom_tile() +
          labs(title = sprintf("Heatmap: %s (cum per well)", var),
               x = "Condition",
               y = "Zone",
               fill = "Value per well") +
          scale_fill_gradient(low  = "white",
                              high = "steelblue",
                              name = "Cum per well") +
          theme_obj
        
        # on choisit le dossier heatmaps
        dir   <- dirs[[ ifelse(th == "light", 7, 8) ]]
        fname <- sprintf("heatmap_%s_%s.png", var, th)
        tryCatch({
          ggsave(file.path(dir, fname),
                 plot   = p, width = 8, height = 6, dpi = 300)
          message("✔️ ", fname)
        }, error = function(e) {
          message("❌ Error saving ", fname, ": ", e$message)
        })
      }
    }
    message("\n🎉 Figure generation completed!\n")
  }
  
  
  # —————————————————————————————
  # TABLES / EXCEL GENERATION
  # —————————————————————————————
  if (do_tables) {
    
    # --------------------
    # PAIRWISE PERCENTAGE DIFFERENCES → Excel
    # --------------------
    
    message("-----")
    message("🔄 Exporting Excel: condition vs. zone & period comparisons")
    message("-----")
    
    excel_output_dir <- file.path(output_base_dir, "excel")
    ensure_directory(excel_output_dir)
    
    percentage_diff_results <- list()
    for (var in names(boxplot_list)) {
      message("📊 Calculating percentage differences for ", var, "…")
      boxplot_data <- boxplot_list[[var]]
      
      results <- boxplot_data %>%
        group_by(period_without_numbers, zone) %>%
        nest() %>%
        mutate(
          comparison_results = map(data, function(df) {
            conds <- unique(df$condition_grouped)
            pairs <- combn(conds, 2, simplify = FALSE)
            map_dfr(pairs, function(pair) {
              c1 <- df[df$condition_grouped == pair[1], ]
              c2 <- df[df$condition_grouped == pair[2], ]
              if (nrow(c1) && nrow(c2)) {
                tibble(
                  condition_comparison = paste(pair[1], pair[2], sep = " vs "),
                  mean_value_1         = round(mean(c1$mean_val,  na.rm = TRUE), 2),
                  mean_value_2         = round(mean(c2$mean_val,  na.rm = TRUE), 2),
                  median_value_1       = round(median(c1$mean_val,na.rm = TRUE), 2),
                  median_value_2       = round(median(c2$mean_val,na.rm = TRUE), 2),
                  mean_diff_pct        = round((mean_value_2 - mean_value_1) /
                                                 abs(mean_value_1) * 100, 2),
                  median_diff_pct      = round((median_value_2 - median_value_1) /
                                                 abs(median_value_1) * 100, 2)
                )
              } else {
                tibble()
              }
            })
          })
        ) %>%
        unnest(comparison_results) %>%
        select(-data)
      
      percentage_diff_results[[var]] <- results
    }
    
    # write out Excel workbook
    excel_file <- file.path(excel_output_dir, "percentage_differences_pairwise.xlsx")
    wb <- createWorkbook()
    for (var in names(percentage_diff_results)) {
      df_out <- percentage_diff_results[[var]]
      addWorksheet(wb, var)
      writeData(wb, var, df_out)
      
      n <- nrow(df_out)
      if (n > 0) {
        mean_col   <- which(names(df_out) == "mean_diff_pct")
        median_col <- which(names(df_out) == "median_diff_pct")
        rows       <- 2:(n + 1)
        
        if (length(mean_col)) {
          conditionalFormatting(
            wb, var, cols = mean_col, rows = rows,
            rule = ">0", style = createStyle(bgFill = "#b9ffb2")
          )
          conditionalFormatting(
            wb, var, cols = mean_col, rows = rows,
            rule = "<0", style = createStyle(bgFill = "#ffb2b2")
          )
        }
        if (length(median_col)) {
          conditionalFormatting(
            wb, var, cols = median_col, rows = rows,
            rule = ">0", style = createStyle(bgFill = "#b9ffb2")
          )
          conditionalFormatting(
            wb, var, cols = median_col, rows = rows,
            rule = "<0", style = createStyle(bgFill = "#ffb2b2")
          )
        }
      }
    }
    saveWorkbook(wb, excel_file, overwrite = TRUE)
    message("🎉 Pairwise percentage differences saved to: ", excel_file)
    
    
    
    # --------------------
    # PAIRWISE PERCENTAGE DIFFERENCES (MOMENTUM) → Excel
    # --------------------
    
    message("-----")
    message("🔄 Exporting Excel: momentum comparisons by condition × zone…")
    message("-----")
    
    excel_output_dir <- file.path(output_base_dir, "excel")
    ensure_directory(excel_output_dir)
    
    percentage_diff_results_momentum <- list()
    for (var in names(all_zone_combined_delta_boxplots)) {
      message("📊 Calculating momentum percentage differences for ", var, "…")
      boxplot_data <- all_zone_combined_delta_boxplots[[var]]
      
      results_momentum <- boxplot_data %>%
        group_by(condition_grouped, zone) %>%
        nest() %>%
        mutate(
          comparison_results = map(data, function(df) {
            momentum_pairs <- list(
              c("before","switch"),
              c("switch","after"),
              c("before","after")
            )
            map_dfr(momentum_pairs, function(pair) {
              m1 <- filter(df, momentum == pair[1])
              m2 <- filter(df, momentum == pair[2])
              if (nrow(m1)>0 && nrow(m2)>0) {
                tibble(
                  momentum_comparison = paste(pair[1], pair[2], sep = "-"),
                  mean_value_1        = round(mean(m1$mean_val, na.rm=TRUE), 2),
                  mean_value_2        = round(mean(m2$mean_val, na.rm=TRUE), 2),
                  median_value_1      = round(median(m1$mean_val, na.rm=TRUE), 2),
                  median_value_2      = round(median(m2$mean_val, na.rm=TRUE), 2),
                  mean_diff_pct       = round((mean_value_2 - mean_value_1) /
                                                abs(mean_value_1) * 100, 2),
                  median_diff_pct     = round((median_value_2 - median_value_1) /
                                                abs(median_value_1) * 100, 2)
                )
              } else {
                tibble()
              }
            })
          })
        ) %>%
        unnest(comparison_results) %>%
        select(-data)
      
      percentage_diff_results_momentum[[var]] <- results_momentum
    }
    
    wb_momentum <- createWorkbook()
    for (var in names(percentage_diff_results_momentum)) {
      addWorksheet(wb_momentum, var)
      writeData(wb_momentum, var, percentage_diff_results_momentum[[var]])
      
      df_out <- percentage_diff_results_momentum[[var]]
      n       <- nrow(df_out)
      if (n>0) {
        mean_col   <- which(names(df_out) == "mean_diff_pct")
        median_col <- which(names(df_out) == "median_diff_pct")
        rows       <- 2:(n+1)
        
        if (length(mean_col)) {
          conditionalFormatting(wb_momentum, var, cols = mean_col, rows = rows,
                                rule = ">0", style = createStyle(bgFill="#b9ffb2"))
          conditionalFormatting(wb_momentum, var, cols = mean_col, rows = rows,
                                rule = "<0", style = createStyle(bgFill="#ffb2b2"))
        }
        if (length(median_col)) {
          conditionalFormatting(wb_momentum, var, cols = median_col, rows = rows,
                                rule = ">0", style = createStyle(bgFill="#b9ffb2"))
          conditionalFormatting(wb_momentum, var, cols = median_col, rows = rows,
                                rule = "<0", style = createStyle(bgFill="#ffb2b2"))
        }
      }
    }
    
    saveWorkbook(wb_momentum,
                 file.path(excel_output_dir, "delta_percentage_differences_momentum.xlsx"),
                 overwrite = TRUE)
    message("🎉 Momentum delta pairwise differences written to delta_percentage_differences_momentum.xlsx")
    
    # --------------------
    # PAIRWISE PERCENTAGE DIFFERENCES (CONDITION) → Excel
    # --------------------
    
    message("-----")
    message("🔄 Exporting Excel: condition comparisons by momentum × zone…")
    message("-----")
    
    percentage_diff_results_condition <- list()
    for (var in names(all_zone_combined_delta_boxplots)) {
      message("📊 Calculating condition percentage differences for ", var, "…")
      boxplot_data <- all_zone_combined_delta_boxplots[[var]]
      
      results_condition <- boxplot_data %>%
        group_by(momentum, zone) %>%
        nest() %>%
        mutate(
          comparison_results = map(data, function(df) {
            condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
            map_dfr(condition_pairs, function(pair) {
              c1 <- filter(df, condition_grouped == pair[1])
              c2 <- filter(df, condition_grouped == pair[2])
              if (nrow(c1)>0 && nrow(c2)>0) {
                tibble(
                  condition_comparison = paste(pair[1], pair[2], sep = "-"),
                  mean_value_1         = round(mean(c1$mean_val, na.rm=TRUE), 2),
                  mean_value_2         = round(mean(c2$mean_val, na.rm=TRUE), 2),
                  median_value_1       = round(median(c1$mean_val, na.rm=TRUE), 2),
                  median_value_2       = round(median(c2$mean_val, na.rm=TRUE), 2),
                  mean_diff_pct        = round((mean_value_2 - mean_value_1) /
                                                 abs(mean_value_1) * 100, 2),
                  median_diff_pct      = round((median_value_2 - median_value_1) /
                                                 abs(median_value_1) * 100, 2)
                )
              } else {
                tibble()
              }
            })
          })
        ) %>%
        unnest(comparison_results) %>%
        select(-data)
      
      percentage_diff_results_condition[[var]] <- results_condition
    }
    
    wb_condition <- createWorkbook()
    for (var in names(percentage_diff_results_condition)) {
      addWorksheet(wb_condition, var)
      writeData(wb_condition, var, percentage_diff_results_condition[[var]])
      
      df_out <- percentage_diff_results_condition[[var]]
      n       <- nrow(df_out)
      if (n>0) {
        mean_col   <- which(names(df_out) == "mean_diff_pct")
        median_col <- which(names(df_out) == "median_diff_pct")
        rows       <- 2:(n+1)
        
        if (length(mean_col)) {
          conditionalFormatting(wb_condition, var, cols = mean_col, rows = rows,
                                rule = ">0", style = createStyle(bgFill="#b9ffb2"))
          conditionalFormatting(wb_condition, var, cols = mean_col, rows = rows,
                                rule = "<0", style = createStyle(bgFill="#ffb2b2"))
        }
        if (length(median_col)) {
          conditionalFormatting(wb_condition, var, cols = median_col, rows = rows,
                                rule = ">0", style = createStyle(bgFill="#b9ffb2"))
          conditionalFormatting(wb_condition, var, cols = median_col, rows = rows,
                                rule = "<0", style = createStyle(bgFill="#ffb2b2"))
        }
      }
    }
  
    saveWorkbook(wb_condition,
                 file.path(excel_output_dir, "delta_percentage_differences_condition.xlsx"),
                 overwrite = TRUE)
    message("🎉 Condition delta pairwise differences written to delta_percentage_differences_condition.xlsx")
    
    message("\n🎉 Table generation completed!\n")
  }
}
