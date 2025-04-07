

simple_figures <- function(data, experiment, version) {

  # Define save paths
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "general analysis", version_string, "all figures")
  sub_folders <- c("performance", "trust", "confidence")

  walk(sub_folders, ~ dir.create(here(output_folder, .x), recursive = TRUE))

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

  # change reliability_level to factor
  block_summary <- block_summary %>%
    mutate(reliability_level = as.factor(reliability_level))

  #############
  ### Trust ###
  #############

  # --- 2 Way ---
  
  p <- flexplot(
    formula = trust ~ condition,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "2Vars_trust_by_condition.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ reliability_level,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "2Vars_trust_by_reliability_level.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ confidence,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "2Vars_trust_by_confidence.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "2Vars_trust_by_performance.png"),
    plot = p, device = "png"
  ))

  # --- 3 Way ---
  
  p <- flexplot(
    formula = trust ~ condition + reliability_level,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_condition_and_reliability_level.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ condition + confidence,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_condition_and_confidence.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ condition + performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_condition_and_performance.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ reliability_level + confidence,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_reliability_level_and_confidence.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ reliability_level + performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_reliability_level_and_performance.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ confidence + performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "3Vars_trust_by_confidence_and_performance.png"),
    plot = p, device = "png"
  ))

  # --- 4 Way ---

  p <- flexplot(
    formula = trust ~ condition + reliability_level | confidence,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "4Vars_trust_by_condition_and_confidence_by_reliability_level.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ condition + reliability_level | performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "4Vars_trust_by_condition_and_performance_by_reliability_level.png"),
    plot = p, device = "png"
  ))

  p <- flexplot(
    formula = trust ~ reliability_level + confidence | performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "4Vars_trust_by_reliability_level_and_performance_by_confidence.png"),
    plot = p, device = "png"
  ))

  # --- 5 Way ---

  p <- flexplot(
    formula = trust ~ condition + reliability_level | confidence + performance,
    data = block_summary,
    method = "lm"
  )
  suppressMessages(ggsave(
    here(output_folder, "trust", "5Vars_trust_by_condition_and_confidence_and_performance_by_reliability_level.png"),
    plot = p, device = "png"
  ))
  

}