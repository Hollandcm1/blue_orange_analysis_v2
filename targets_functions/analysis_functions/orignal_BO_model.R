# reproduce the original BO model

original_BO <- function(data, version, experiment) {

  # save path
  output_folder <- here("output", "original_BO_model", paste0(experiment, "_", version))
  sub_folders <- c("trust")
  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- get_block_summary(data)
  participant_summary <- get_participant_summary(data)

  #############
  ### Model ###
  #############

  model <- lmer(
    trust ~ condition * reliability_level * confidence + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust", "trust_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust", "trust_LME_formatted.html"))

  #############
  ### Plots ###
  #############

  # --- Trust by Reliability Level, Confidence, and Condition ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Reliability Level, Confidence, and Condition",
         x = "Confidence", y = "Trust") +
    facet_wrap(~ condition) +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "3_way_trust_by_reliability_level_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability Level and Condition ---
  p <- ggplot(block_summary, aes(x = reliability_level, y = trust, color = as.factor(condition))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Reliability Level and Condition",
         x = "Reliability Level", y = "Trust") +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_reliability_level_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability Level and Confidence ---
  p <- ggplot(block_summary, aes(x = reliability_level, y = trust, color = as.factor(confidence))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Reliability Level and Confidence",
         x = "Reliability Level", y = "Trust") +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_reliability_level_and_confidence.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Confidence and Condition ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust, color = as.factor(condition))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Confidence and Condition",
         x = "Confidence", y = "Trust") +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_confidence_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability Level ---
  p <- ggplot(block_summary, aes(x = reliability_level, y = trust)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Reliability Level",
         x = "Reliability Level", y = "Trust") +
    # xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_reliability_level.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Condition ---
  p <- ggplot(block_summary, aes(x = condition, y = trust)) +
    geom_beeswarm(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Condition",
         x = "Condition", y = "Trust") +
    # xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Confidence ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Trust by Confidence",
         x = "Confidence", y = "Trust") +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_confidence.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

}