# contains functions for exploring the composites 
# such as performance over time

explore_composites <- function(data, version, before_cleaning = TRUE) {

  get_experiment_subset <- function(data, version) {
    data %>% filter(experiment == version)
  }

  data <- get_experiment_subset(data, version)
  cleaning_state <- ifelse(before_cleaning, "before_cleaning", "after_cleaning")
  output_folder <- here("output", "composites", version, cleaning_state)
  sub_folders <- c("performance_by_block")

  walk(sub_folders, function(sub_folder) {
    dir.create(here(output_folder, sub_folder), recursive = TRUE, showWarnings = FALSE)
  })

  # Helper functions
  save_plot <- function(plot_obj, filename, width = 6, height = 4) {
    ggsave(here(output_folder, filename), plot_obj, width = width, height = height)
  }

  filter_single_trial <- function(df, trial_col = "trial_v2") {
    df %>% filter(!!sym(trial_col) == 1)
  }

  make_density_plot <- function(df, x, fill, title, filename, position = "identity") {
    p <- ggplot(df, aes_string(x = x, fill = fill)) +
      geom_density(alpha = 0.5, position = position, bw = 4) +
      theme_minimal() +
      xlim(0, 100) +
      labs(title = title, x = x)
    save_plot(p, filename)
  }

  warn_if_out_of_bounds <- function(x, var_name) {
    if (any(x < 0 | x > 100, na.rm = TRUE)) {
      warning(paste("Some", var_name, "values are out of the 0-100 range"))
    }
  }

  #############################
  ### Overall Performance   ###
  #############################

  data_overall <- data %>% group_by(p_num, condition) %>% filter(trial_v2 == 1) %>% ungroup()

  # Violin + boxplot
  p <- ggplot(data_overall, aes(x = condition, y = percent_correct_condition, fill = condition)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.1) +
    theme_minimal() +
    ylim(0, 100) +
    labs(title = "Overall Performance by Condition", x = "Condition", y = "Percent Correct")
  save_plot(p, "overall_performance.png")

  warn_if_out_of_bounds(data_overall$percent_correct_condition, "percent_correct_condition")

  # Density Plots
  make_density_plot(data_overall, "percent_correct_condition", "condition", "Density of Overall Performance", "density_overall_performance.png")
  make_density_plot(data_overall, "percent_correct_condition", "condition", "Balanced Density", "density_balanced_overall_performance.png", position = "fill")
  make_density_plot(data_overall, "percent_correct_condition", "condition", "Stacked Density", "density_stacked_overall_performance.png", position = "stack")

  # Individual Jitter Plot
  p <- ggplot(data_overall, aes(x = condition, y = percent_correct_condition, fill = condition)) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
    theme_minimal() +
    ylim(0, 100) +
    labs(title = "Individual Overall Performance", x = "Condition", y = "Percent Correct")
  save_plot(p, "individual_overall_performance.png")

  ###############################
  ### Performance by Block    ###
  ###############################

  data_block <- data %>% group_by(p_num, condition, block) %>% filter(trial_v1 == 1) %>% ungroup()

  p <- ggplot(data_block, aes(x = block, y = percent_correct_block, color = as.numeric(p_num), group = p_num)) +
    geom_line() + geom_point() + theme_minimal() + ylim(0, 100) +
    labs(title = "Performance by Block", x = "Block", y = "Percent Correct") +
    scale_color_gradientn(colors = rainbow(length(unique(data_block$p_num))))
  save_plot(p, "performance_by_block.png")

  # Individual by participant
  walk(unique(data_block$p_num), function(pid) {
    p_data <- filter(data_block, p_num == pid)
    p <- ggplot(p_data, aes(x = block, y = percent_correct_block)) +
      geom_line() + geom_point() + theme_minimal() + ylim(0, 100) +
      labs(title = paste("Performance by Block for", pid), x = "Block", y = "Percent Correct")
    save_plot(p, file.path("performance_by_block", paste0("performance_by_block_p", pid, ".png")))
  })

  # Add more cleaned modules for each section as needed...
  # - cumulative_performance
  # - confidence/trust calibration
  # - demographic summaries
  # - etc.

  message("Finished exploring composites for: ", version)
  return(invisible(NULL))

}