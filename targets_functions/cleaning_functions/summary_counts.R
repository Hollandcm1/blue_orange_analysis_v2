

summary_counts <- function(data, before_cleaning) {
  save_path <- here("output", "summary", if (before_cleaning) "before_cleaning" else "after_cleaning")

  # Create output directory if it doesn't exist
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Count unique participants by experiment and condition
  counts_summary <- data %>%
    group_by(experiment, condition) %>%
    summarise(n_participants = n_distinct(p_num), .groups = "drop")

  # Create summary table
  formatted_table <- counts_summary %>%
    gt() %>%
    tab_header(
      title = "Participant Counts by Experiment and Condition",
      subtitle = "Summary of unique participants across experiments and conditions"
    ) %>%
    cols_label(
      experiment = "Experiment",
      condition = "Condition",
      n_participants = "Number of Participants"
    ) %>%
    fmt_number(
      columns = vars(n_participants),
      decimals = 0
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    )

  gtsave(formatted_table, here(save_path, "participant_counts_table.html"))

  # Create and save plot
  plot_data <- counts_summary %>%
    ggplot(aes(x = experiment, y = n_participants, fill = condition)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Participant Counts by Experiment and Condition",
      x = "Experiment",
      y = "Number of Participants",
      fill = "Condition"
    ) +
    theme_minimal()

  ggsave(here(save_path, "participant_counts_plot.png"), plot_data, width = 8, height = 6, dpi = 300)

  return(invisible(NULL))
}