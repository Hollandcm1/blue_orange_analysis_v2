
# library(targets)
# library(here)
# library(dplyr)
# library(lme4)
# library(sjPlot)
# library(ggplot2)
# library(flexplot)
# library(glmmTMB)
# library(GGally)
# library(ggbeeswarm)

# data <- tar_read("data_reliance_50_specific")

# analyze increasing and decreasing groups seperately
seperate_increasing_vs_decreasing_LMES_50 <- function(data){

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

  # --- Beta Model ---
  
  # rescale dependence to be 0-1
  block_summary_increasing <- block_summary_increasing %>%
    mutate(dependence = (dependence - min(dependence, na.rm = TRUE)) / 
             (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE)))

  block_summary_decreasing <- block_summary_decreasing %>%
    mutate(dependence = (dependence - min(dependence, na.rm = TRUE)) / 
             (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE)))

  # # Fit beta regression model for increasing condition
  # model_increasing_beta <- glmmTMB(
  #   dependence ~ trust * confidence * reliability_level + (1 | p_num),
  #   data = block_summary_increasing,
  #   family = beta_family(link = "logit")
  # )
  # model_summary <- capture.output(summary(model_increasing_beta))
  # writeLines(as.character(model_summary), here(save_path, "dependence_increasing_beta_LME.txt"))
  # model_summary_formated <- tab_model(model_increasing_beta)
  # writeLines(as.character(model_summary_formated$knitr), here(save_path, "dependence_increasing_beta_LME_formatted.html"))

  # Fit beta regression model for decreasing condition
  

  


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
    ylim(0, 1)

  # p_increasing

  suppressMessages(ggsave(
    here(save_path, "dependence_increasing_by_trust_reliability.png"),
    plot = p_increasing, device = "png",
    width = 10, height = 8
  ))

  # version where reliability is on x axis, trust is color
  p_increasing <- ggplot(block_summary_increasing, aes(x = reliability_level, y = dependence, color = trust)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(title = "Dependence by Reliability Level, Trust, and Confidence (Increasing)",
         x = "Reliability Level", y = "Dependence") +
    xlim(50, 100) +
    ylim(0, 1)


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

  # --- Parallel Coordinates Plot (Increasing) ---
  block_summary_increasing$reliability_color <- as.factor(block_summary_increasing$reliability_level)
  parcoord_data_inc <- block_summary_increasing %>%
    mutate(
      trust = pmax((trust - 30) / 70, 0),
      confidence = pmax((confidence - 30) / 70, 0),
      dependence = (dependence - min(dependence, na.rm = TRUE)) /
        (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE)),
      reliability_level = (reliability_level - min(reliability_level, na.rm = TRUE)) /
        (max(reliability_level, na.rm = TRUE) - min(reliability_level, na.rm = TRUE))
    )
  parcoord_cols_inc <- match(
    c("dependence", "trust", "confidence", "reliability_level"),
    names(parcoord_data_inc))

  p_parcoord_increasing <- GGally::ggparcoord(
    data = parcoord_data_inc,
    columns = parcoord_cols_inc,
    groupColumn = "reliability_color",
    scale = "globalminmax",
    alphaLines = 0.3
  ) +
    ggbeeswarm::geom_quasirandom(
      groupOnX = TRUE,
      width = 0.15,
      alpha = 0.4,
      size = 1
    ) +
    theme_minimal() +
    labs(
      title = "Parallel Coordinates: Dependence, Trust, Confidence, Reliability (Increasing)",
      x = "Variable",
      y = "Scaled Value",
      color = "Reliability Level"
    )

  suppressMessages(ggsave(
    here(save_path, "parallel_coordinates_increasing.png"),
    plot = p_parcoord_increasing, device = "png",
    width = 10, height = 8
  ))

  # --- Parallel Coordinates Plot (Decreasing) ---
  block_summary_decreasing$reliability_color <- as.factor(block_summary_decreasing$reliability_level)
  parcoord_data_dec <- block_summary_decreasing %>%
    mutate(
      trust = pmax((trust - 30) / 70, 0),
      confidence = pmax((confidence - 30) / 70, 0),
      dependence = (dependence - min(dependence, na.rm = TRUE)) /
        (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE)),
      reliability_level = (reliability_level - min(reliability_level, na.rm = TRUE)) /
        (max(reliability_level, na.rm = TRUE) - min(reliability_level, na.rm = TRUE))
    )
  parcoord_cols_dec <- match(
    c("dependence", "trust", "confidence", "reliability_level"),
    names(parcoord_data_dec))

  p_parcoord_decreasing <- GGally::ggparcoord(
    data = parcoord_data_dec,
    columns = parcoord_cols_dec,
    groupColumn = "reliability_color",
    scale = "globalminmax",
    alphaLines = 0.3
  ) +
    ggbeeswarm::geom_quasirandom(
      groupOnX = TRUE,
      width = 0.15,
      alpha = 0.4,
      size = 1
    ) +
    theme_minimal() +
    labs(
      title = "Parallel Coordinates: Dependence, Trust, Confidence, Reliability (Decreasing)",
      x = "Variable",
      y = "Scaled Value",
      color = "Reliability Level"
    )

  suppressMessages(ggsave(
    here(save_path, "parallel_coordinates_decreasing.png"),
    plot = p_parcoord_decreasing, device = "png",
    width = 10, height = 8
  ))

}


dependence_LME_50 <- function(data, version){

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_LME")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
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
