# Runs generic ANOVAs that work on all versions of the experiment

simple_ANOVAs <- function(data, experiment, version) {

  # Define save paths
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "general analysis", version_string, "ANOVAs")
  sub_folders <- c("performance", "trust", "confidence")

  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE), 
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block),
      .groups = "drop"
    )

  # Helper function
  run_anova_and_plots <- function(variable) {
    # Run ANOVA
    formula <- as.formula(paste(variable, "~ condition * reliability_level + Error(p_num)"))
    result <- aov(formula, data = block_summary)
    summary_result <- capture.output(summary(result))
    writeLines(summary_result, here(output_folder, variable, paste0(variable, "_ANOVA.txt")))

    # Plot 1: by reliability_level and condition
    p1 <- ggplot(block_summary, aes_string(x = "reliability_level", y = variable, color = "condition")) +
      geom_beeswarm() + geom_smooth(method = "lm") + theme_minimal() +
      labs(title = paste(variable, "by Reliability Level and Condition"),
           x = "Reliability Level", y = str_to_title(variable))
    
    # Plot 2: by reliability_level
    p2 <- ggplot(block_summary, aes_string(x = "reliability_level", y = variable)) +
      geom_beeswarm() + geom_smooth(method = "lm") + theme_minimal() +
      labs(title = paste(variable, "by Reliability Level"),
           x = "Reliability Level", y = str_to_title(variable))

    # Plot 3: by condition
    p3 <- ggplot(block_summary, aes_string(x = "condition", y = variable, fill = "condition")) +
      geom_violin(alpha = 0.5) + geom_boxplot(width = 0.3) + geom_beeswarm(size = 0.3) +
      theme_minimal() +
      labs(title = paste(variable, "by Condition"), x = "Condition", y = str_to_title(variable)) +
      ylim(0, 100)

    # Save plots
    suppressMessages(ggsave(here(output_folder, variable, paste0(variable, "_by_reliability_level_and_condition.png")), p1, width = 6, height = 4))
    suppressMessages(ggsave(here(output_folder, variable, paste0(variable, "_by_reliability_level.png")), p2, width = 6, height = 4))
    suppressMessages(ggsave(here(output_folder, variable, paste0(variable, "_by_condition.png")), p3, width = 6, height = 4))
  }

  # Run for each variable
  walk(c("performance", "trust", "confidence"), run_anova_and_plots)
}