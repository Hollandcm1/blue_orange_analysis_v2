
# library(targets)
# library(here)
# library(dplyr)
# library(lme4)
# library(sjPlot)
# library(ggplot2)
# library(flexplot)

# data <- tar_read("data_reliance_50_specific")

# analyze increasing and decreasing groups seperately
seperate_increasing_vs_decreasing_LMES <- function(data){

  save_path <- here("output", "specific", "dependence_50", "seperate_increasing_vs_decreasing_LMES")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  data_increasing <- data %>%
    filter(condition == "50% IR")

  data_decreasing <- data %>%
    filter(condition == "50% DR")

  # Summarize data
  block_summary_increasing <- data_increasing %>%
    group_by(p_num, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )
  block_summary_decreasing <- data_decreasing %>%
    group_by(p_num, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )


  #############
  ### Model ###
  #############

  model_increasing <- lmer(
    dependence ~ trust * confidence * reliability_level + (1 | p_num),
    block_summary_increasing
  )
  # tab_model(model_increasing)
  model_summary <- capture.output(summary(model_increasing))
  writeLines(as.character(model_summary), here(save_path, "dependence_increasing_LME.txt"))
  model_summary_formated <- tab_model(model_increasing)
  writeLines(as.character(model_summary_formated$knitr), here(save_path, "dependence_increasing_LME_formatted.html"))

  model_decreasing <- lmer(
    dependence ~ trust * confidence * reliability_level + (1 | p_num),
    block_summary_decreasing
  )
  # tab_model(model_decreasing)
  model_summary <- capture.output(summary(model_decreasing))
  writeLines(as.character(model_summary), here(save_path, "dependence_decreasing_LME.txt"))
  model_summary_formated <- tab_model(model_decreasing)
  writeLines(as.character(model_summary_formated$knitr), here(save_path, "dependence_decreasing_LME_formatted.html"))


  #############
  ### Plots ###
  #############

  # --- Dependence by Trust, and Reliability Level (Increasing) ---
  p_increasing <- ggplot(block_summary_increasing, aes(x = trust, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Dependence by Trust, Confidence, and Reliability Level (Increasing)",
        x = "Trust", y = "Dependence") +
    xlim(0, 100) +
    ylim(0, 100)

  # p_increasing

  suppressMessages(ggsave(
    here(save_path, "dependence_increasing_by_trust_reliability.png"),
    plot = p_increasing, device = "png",
    width = 10, height = 8
  ))

  # --- Dependence by Trust, Confidence, and Reliability Level (Decreasing) ---
  p_decreasing <- ggplot(block_summary_decreasing, aes(x = trust, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Dependence by Trust and Reliability Level (Decreasing)",
        x = "Trust", y = "Dependence") +
    xlim(0, 100) +
    ylim(0, 100)

  # p_decreasing

  suppressMessages(ggsave(
    here(save_path, "dependence_decreasing_by_trust_reliability.png"),
    plot = p_decreasing, device = "png",
    width = 10, height = 8
  ))

  # p_decreasing <- ggplot(block_summary_decreasing, aes(x = trust, y = dependence, color = as.factor(reliability_level))) +
  #   geom_point(alpha = 0.3) +
  #   geom_smooth(method = "lm", alpha = 0.1) +
  #   theme_minimal() +
  #   labs(title = "Dependence by Trust, Confidence, and Reliability Level (Decreasing)",
  #       x = "Trust", y = "Dependence") +
  #   facet_wrap(~ reliability_level) +
  #   xlim(0, 100) +
  #   ylim(0, 100)

  # suppressMessages(ggsave(
  #   here(save_path, "dependence_decreasing_by_trust_reliability_facet.png"),
  #   plot = p_decreasing, device = "png",
  #   width = 10, height = 8
  # ))

  # make factor version of reliability_level
  block_summary_decreasing <- block_summary_decreasing %>%
    mutate(reliability_level_factor = as.factor(reliability_level))

  p <- flexplot(
    data = block_summary_decreasing,
    dependence ~ trust + confidence | reliability_level_factor,
    method = 'lm'
  )

  suppressMessages(ggsave(
    here(save_path, "dependence_decreasing_by_trust_confidence_reliability_flexplot.png"),
    plot = p, device = "png",
    width = 15, height = 4
  ))


  p_decreasing <- ggplot(block_summary_decreasing, aes(x = confidence, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Dependence by Confidence, and Reliability Level (Decreasing)",
        x = "COnfidence", y = "Dependence") +
    xlim(0, 100) +
    ylim(0, 100)

  suppressMessages(ggsave(
    here(save_path, "dependence_decreasing_by_confidence_reliability.png"),
    plot = p_decreasing, device = "png",
    width = 10, height = 8
  ))

  block_summary_decreasing <- block_summary_decreasing %>%
    mutate(confidence_group = ifelse(confidence <= median(confidence, na.rm = TRUE), "Low", "High")) %>%
    filter(!is.na(confidence_group))

  # --- Dependence (y) by Trust (x) by Reliability Level (color) by Confidence (facet) for Decreasing condition ---
  p_increasing_confidence <- ggplot(block_summary_decreasing, aes(x = trust, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Dependence by Trust, Reliability Level, Confidene (Decreasing)",
        x = "Trust", y = "Dependence") +
    facet_wrap(~ confidence_group) +
    xlim(0, 100) +
    ylim(0, 100)

  suppressMessages(ggsave(
    here(save_path, "dependence_increasing_by_trust_reliability_confidence_facet.png"),
    plot = p_increasing_confidence, device = "png",
    width = 12, height = 6
  ))

}


dependence_LME <- function(data_reliance_50_specific, version){

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_LME")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data_reliance_50_specific %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )

  #############
  ### Model ###
  #############

  model <- lmer(
    dependence ~ condition * trust * confidence * reliability_level + (1 | p_num),
    block_summary
  )
  # tab_model(model)
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(save_path, "dependence_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(save_path, "dependence_LME_formatted.html"))

  #############
  ### Plots ###
  #############

  # not being done due to complexity

} 
