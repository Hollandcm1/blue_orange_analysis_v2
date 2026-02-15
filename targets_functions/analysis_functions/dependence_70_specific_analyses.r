
# library(targets)
# library(here)
# library(dplyr)
# library(lme4)
# library(sjPlot)
# library(ggplot2)
# library(flexplot)
# library(glmmTMB)

# data <- tar_read("data_reliance_70_specific")

# analyze increasing and decreasing groups seperately
seperate_increasing_vs_decreasing_LMES_70 <- function(data){

  save_path <- here("output", "specific", "dependence_70", "seperate_increasing_vs_decreasing_LMES")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  data_increasing <- data %>%
    filter(condition == "70% IR")

  data_decreasing <- data %>%
    filter(condition == "70% DR")

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
  
  # rescale dependence to be 0-1, then squeeze to open interval (0, 1)
  # using Smithson & Verkuilen (2006) transformation for beta regression
  block_summary_increasing <- block_summary_increasing %>%
    mutate(dependence = (dependence - min(dependence, na.rm = TRUE)) /
             (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE))) %>%
    mutate(dependence = (dependence * (n() - 1) + 0.5) / n())

  block_summary_decreasing <- block_summary_decreasing %>%
    mutate(dependence = (dependence - min(dependence, na.rm = TRUE)) /
             (max(dependence, na.rm = TRUE) - min(dependence, na.rm = TRUE))) %>%
    mutate(dependence = (dependence * (n() - 1) + 0.5) / n())

  # Fit beta regression model for increasing condition
  model_increasing_beta <- glmmTMB(
    dependence ~ trust * confidence * reliability_level + (1 | p_num),
    data = block_summary_increasing,
    family = beta_family(link = "logit")
  )
  model_summary <- capture.output(summary(model_increasing_beta))
  writeLines(as.character(model_summary), here(save_path, "dependence_increasing_beta_LME.txt"))
  model_summary_formated <- tab_model(model_increasing_beta)
  writeLines(as.character(model_summary_formated$knitr), here(save_path, "dependence_increasing_beta_LME_formatted.html"))

  # Fit beta regression model for decreasing condition
  

  


  #############
  ### Plots ###
  #############

  # --- Dependence by Trust, and Reliability Level (Increasing) ---
  p_increasing <- ggplot(block_summary_increasing, aes(x = trust, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
    labs(title = "Dependence by Trust, Confidence, and Reliability Level (Increasing)",
        x = "Trust", y = "Dependence") #+
    # xlim(0, 100) +
    #ylim(0, 100)

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
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
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
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
    labs(title = "Dependence by Confidence, and Reliability Level (Decreasing)",
        x = "Self-Confidence", y = "Dependence") +
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
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
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


dependence_LME_70 <- function(data, version){

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

  # plot dependence (y) by confidence (x) by reliability_level (color) by condition (facet)
  p <- ggplot(block_summary, aes(x = confidence, y = dependence, color = as.factor(reliability_level))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
    labs(# title = "Dependence by Confidence and Reliability Level",
         x = "Self-Confidence", y = "Dependence") +
    facet_wrap(~ condition) +
    xlim(0, 100) +
    ylim(0, 100)

  suppressMessages(ggsave(
    here(save_path, "dependence_by_confidence_reliability_facet.png"),
    plot = p, device = "png",
    width = 14, height = 6
  ))

}


dependence_ANOVA_reliability_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_ANOVA_reliability_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      performance_post = mean(percent_correct_block2, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )

  variables <- c("trust", "performance", "performance_post", "confidence", "dependence")

  # Save means by condition
  condition_means <- block_summary %>%
    group_by(condition) %>%
    summarise(
      across(all_of(variables),
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
      .groups = "drop")
  write.csv(condition_means, here(save_path, "means_by_condition.csv"), row.names = FALSE)

  # Factor versions for sphericity testing
  block_summary$reliability_level_f <- as.factor(block_summary$reliability_level)
  block_summary$p_num <- as.factor(block_summary$p_num)

  plot_list <- list()

  for (variable in variables) {
    formula <- as.formula(paste(variable, "~ reliability_level * condition + Error(p_num)"))
    result <- aov(formula, data = block_summary)
    summary_result <- capture.output(summary(result))
    eta_result <- capture.output(print(eta_squared(result, partial = TRUE)))

    # Mauchly's sphericity test + GG/HF corrections
    afex_model <- aov_ez(
      id = "p_num", dv = variable, data = block_summary,
      within = "reliability_level_f", between = "condition", type = 3
    )
    sphericity_result <- capture.output(print(summary(afex_model)))

    # GG-corrected F statistics with corrected degrees of freedom
    afex_summary <- summary(afex_model)
    univ_tests <- afex_summary$univariate.tests
    gg_corrections <- afex_summary$pval.adjustments
    gg_lines <- "Greenhouse-Geisser Corrected F Statistics:"
    for (effect_name in rownames(gg_corrections)) {
      eps <- gg_corrections[effect_name, "GG eps"]
      gg_p <- gg_corrections[effect_name, "Pr(>F[GG])"]
      num_df <- univ_tests[effect_name, "num Df"] * eps
      den_df <- univ_tests[effect_name, "den Df"] * eps
      f_val <- univ_tests[effect_name, "F value"]
      gg_lines <- c(gg_lines, sprintf("  %s: F(%.2f, %.2f) = %.4f, p = %.4f (GG eps = %.3f)",
                                       effect_name, num_df, den_df, f_val, gg_p, eps))
    }

    writeLines(c(summary_result, "", "Partial Eta Squared:", eta_result,
                 "", "Sphericity Tests:", sphericity_result,
                 "", gg_lines),
               here(save_path, paste0(variable, "_ANOVA.txt")))

    p <- ggplot(block_summary, aes(x = reliability_level, y = .data[[variable]], color = condition)) +
      geom_beeswarm(alpha = 0.3) +
      geom_smooth(method = "lm", alpha = 0.1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
            legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
      scale_color_discrete(labels = function(x) {
        x <- sub("^\\d+% ", "", x)
        ifelse(x == "IR", "Increasing Reliability",
          ifelse(x == "DR", "Decreasing Reliability", x))
      }) +
      labs(x = "Reliability Level", y = ifelse(variable == "confidence", "Self-Confidence",
             ifelse(variable == "performance_post", "Post-Rec Performance", str_to_title(variable))))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_reliability_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] + plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 25, height = 8
  ))

  # Combined figure without pre-rec performance
  combined_post <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined_post_rec.png"),
    plot = combined_post, device = "png",
    width = 20, height = 8
  ))

  return(invisible(TRUE))
}


dependence_ANOVA_block_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_ANOVA_block_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      performance_post = mean(percent_correct_block2, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )

  variables <- c("trust", "performance", "performance_post", "confidence", "dependence")

  # Save means by condition
  condition_means <- block_summary %>%
    group_by(condition) %>%
    summarise(
      across(all_of(variables),
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
      .groups = "drop")
  write.csv(condition_means, here(save_path, "means_by_condition.csv"), row.names = FALSE)

  # Factor versions for sphericity testing
  block_summary$block_f <- as.factor(block_summary$block)
  block_summary$p_num <- as.factor(block_summary$p_num)

  plot_list <- list()

  for (variable in variables) {
    formula <- as.formula(paste(variable, "~ block * condition + Error(p_num)"))
    result <- aov(formula, data = block_summary)
    summary_result <- capture.output(summary(result))
    eta_result <- capture.output(print(eta_squared(result, partial = TRUE)))

    # Mauchly's sphericity test + GG/HF corrections
    afex_model <- aov_ez(
      id = "p_num", dv = variable, data = block_summary,
      within = "block_f", between = "condition", type = 3
    )
    sphericity_result <- capture.output(print(summary(afex_model)))

    # GG-corrected F statistics with corrected degrees of freedom
    afex_summary <- summary(afex_model)
    univ_tests <- afex_summary$univariate.tests
    gg_corrections <- afex_summary$pval.adjustments
    gg_lines <- "Greenhouse-Geisser Corrected F Statistics:"
    for (effect_name in rownames(gg_corrections)) {
      eps <- gg_corrections[effect_name, "GG eps"]
      gg_p <- gg_corrections[effect_name, "Pr(>F[GG])"]
      num_df <- univ_tests[effect_name, "num Df"] * eps
      den_df <- univ_tests[effect_name, "den Df"] * eps
      f_val <- univ_tests[effect_name, "F value"]
      gg_lines <- c(gg_lines, sprintf("  %s: F(%.2f, %.2f) = %.4f, p = %.4f (GG eps = %.3f)",
                                       effect_name, num_df, den_df, f_val, gg_p, eps))
    }

    writeLines(c(summary_result, "", "Partial Eta Squared:", eta_result,
                 "", "Sphericity Tests:", sphericity_result,
                 "", gg_lines),
               here(save_path, paste0(variable, "_ANOVA.txt")))

    p <- ggplot(block_summary, aes(x = block, y = .data[[variable]], color = condition)) +
      geom_beeswarm(alpha = 0.3) +
      geom_smooth(method = "lm", alpha = 0.1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
            legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
      scale_color_discrete(labels = function(x) {
        x <- sub("^\\d+% ", "", x)
        ifelse(x == "IR", "Increasing Reliability",
          ifelse(x == "DR", "Decreasing Reliability", x))
      }) +
      labs(x = "Block", y = ifelse(variable == "confidence", "Self-Confidence",
             ifelse(variable == "performance_post", "Post-Rec Performance", str_to_title(variable))))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_block_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] + plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 25, height = 8
  ))

  # Combined figure without pre-rec performance
  combined_post <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined_post_rec.png"),
    plot = combined_post, device = "png",
    width = 20, height = 8
  ))

  return(invisible(TRUE))
}


dependence_RM_ANOVA_reliability_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_RM_ANOVA_reliability_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      performance_post = mean(percent_correct_block2, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )

  block_summary$reliability_level_f <- as.factor(block_summary$reliability_level)
  block_summary$p_num <- as.factor(block_summary$p_num)

  # Set polynomial contrasts for trend analysis
  contrasts(block_summary$reliability_level_f) <- contr.poly(nlevels(block_summary$reliability_level_f))

  variables <- c("trust", "performance", "performance_post", "confidence", "dependence")

  # Save means by condition and reliability level
  condition_means <- block_summary %>%
    group_by(condition, reliability_level_f) %>%
    summarise(
      across(all_of(variables),
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
      .groups = "drop")
  write.csv(condition_means, here(save_path, "means_by_condition_and_reliability.csv"), row.names = FALSE)

  n_levels <- nlevels(block_summary$reliability_level_f)
  trend_names <- c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic")[1:(n_levels - 1)]
  split_list <- setNames(as.list(1:(n_levels - 1)), trend_names)

  plot_list <- list()

  for (variable in variables) {
    # Repeated-measures ANOVA with reliability as factor
    afex_model <- aov_ez(
      id = "p_num", dv = variable, data = block_summary,
      within = "reliability_level_f", between = "condition", type = 3
    )

    # ANOVA table (assuming sphericity)
    afex_summary <- summary(afex_model)
    anova_result <- capture.output(print(afex_summary$univariate.tests))

    # Partial eta squared
    eta_result <- capture.output(print(eta_squared(afex_model, partial = TRUE)))

    # Mauchly's test + GG/HF corrections
    sphericity_result <- capture.output(print(afex_summary))

    # GG-corrected F statistics with corrected degrees of freedom
    univ_tests <- afex_summary$univariate.tests
    gg_corrections <- afex_summary$pval.adjustments
    gg_lines <- "Greenhouse-Geisser Corrected F Statistics:"
    for (effect_name in rownames(gg_corrections)) {
      eps <- gg_corrections[effect_name, "GG eps"]
      gg_p <- gg_corrections[effect_name, "Pr(>F[GG])"]
      num_df <- univ_tests[effect_name, "num Df"] * eps
      den_df <- univ_tests[effect_name, "den Df"] * eps
      f_val <- univ_tests[effect_name, "F value"]
      gg_lines <- c(gg_lines, sprintf("  %s: F(%.2f, %.2f) = %.4f, p = %.4f (GG eps = %.3f)",
                                       effect_name, num_df, den_df, f_val, gg_p, eps))
    }

    # Polynomial trend analysis
    poly_formula <- as.formula(paste(variable, "~ reliability_level_f * condition + Error(p_num/reliability_level_f)"))
    poly_model <- aov(poly_formula, data = block_summary)
    poly_result <- capture.output(summary(poly_model,
      split = list(reliability_level_f = split_list, "reliability_level_f:condition" = split_list)))

    # Per-condition polynomial trend analyses
    conditions <- unique(as.character(block_summary$condition))
    per_cond_lines <- c()
    for (cond in conditions) {
      cond_data <- block_summary[block_summary$condition == cond, ]
      poly_formula_cond <- as.formula(paste(variable, "~ reliability_level_f + Error(p_num/reliability_level_f)"))
      poly_model_cond <- aov(poly_formula_cond, data = cond_data)
      poly_result_cond <- capture.output(summary(poly_model_cond,
        split = list(reliability_level_f = split_list)))
      per_cond_lines <- c(per_cond_lines, "", paste("Condition:", cond), poly_result_cond)
    }

    writeLines(c("Repeated-Measures ANOVA (reliability_level as factor):", "",
                 anova_result, "",
                 "Partial Eta Squared:", eta_result, "",
                 "Sphericity Tests:", sphericity_result, "",
                 gg_lines, "",
                 "Polynomial Trend Analysis:", poly_result, "",
                 "Per-Condition Polynomial Trend Analysis:", per_cond_lines),
               here(save_path, paste0(variable, "_RM_ANOVA.txt")))

    p <- ggplot(block_summary, aes(x = reliability_level_f, y = .data[[variable]], color = condition)) +
      geom_beeswarm(alpha = 0.3, dodge.width = 0.5) +
      stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
      stat_summary(fun = mean, geom = "line", aes(group = condition), position = position_dodge(width = 0.5)) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
            legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
      scale_color_discrete(labels = function(x) {
        x <- sub("^\\d+% ", "", x)
        ifelse(x == "IR", "Increasing Reliability",
          ifelse(x == "DR", "Decreasing Reliability", x))
      }) +
      labs(x = "Reliability Level", y = ifelse(variable == "confidence", "Self-Confidence",
             ifelse(variable == "performance_post", "Post-Rec Performance", str_to_title(variable))))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_reliability_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] + plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 25, height = 8
  ))

  # Combined figure without pre-rec performance
  combined_post <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined_post_rec.png"),
    plot = combined_post, device = "png",
    width = 20, height = 8
  ))

  return(invisible(TRUE))
}


dependence_RM_ANOVA_block_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("dependence_", version), "dependence_RM_ANOVA_block_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      performance_post = mean(percent_correct_block2, na.rm = TRUE),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )

  block_summary$block_f <- as.factor(block_summary$block)
  block_summary$p_num <- as.factor(block_summary$p_num)

  # Set polynomial contrasts for trend analysis
  contrasts(block_summary$block_f) <- contr.poly(nlevels(block_summary$block_f))

  variables <- c("trust", "performance", "performance_post", "confidence", "dependence")

  # Save means by condition and block
  condition_means <- block_summary %>%
    group_by(condition, block_f) %>%
    summarise(
      across(all_of(variables),
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
      .groups = "drop")
  write.csv(condition_means, here(save_path, "means_by_condition_and_block.csv"), row.names = FALSE)

  n_levels <- nlevels(block_summary$block_f)
  trend_names <- c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic")[1:(n_levels - 1)]
  split_list <- setNames(as.list(1:(n_levels - 1)), trend_names)

  plot_list <- list()

  for (variable in variables) {
    # Repeated-measures ANOVA with block as factor
    afex_model <- aov_ez(
      id = "p_num", dv = variable, data = block_summary,
      within = "block_f", between = "condition", type = 3
    )

    # ANOVA table (assuming sphericity)
    afex_summary <- summary(afex_model)
    anova_result <- capture.output(print(afex_summary$univariate.tests))

    # Partial eta squared
    eta_result <- capture.output(print(eta_squared(afex_model, partial = TRUE)))

    # Mauchly's test + GG/HF corrections
    sphericity_result <- capture.output(print(afex_summary))

    # GG-corrected F statistics with corrected degrees of freedom
    univ_tests <- afex_summary$univariate.tests
    gg_corrections <- afex_summary$pval.adjustments
    gg_lines <- "Greenhouse-Geisser Corrected F Statistics:"
    for (effect_name in rownames(gg_corrections)) {
      eps <- gg_corrections[effect_name, "GG eps"]
      gg_p <- gg_corrections[effect_name, "Pr(>F[GG])"]
      num_df <- univ_tests[effect_name, "num Df"] * eps
      den_df <- univ_tests[effect_name, "den Df"] * eps
      f_val <- univ_tests[effect_name, "F value"]
      gg_lines <- c(gg_lines, sprintf("  %s: F(%.2f, %.2f) = %.4f, p = %.4f (GG eps = %.3f)",
                                       effect_name, num_df, den_df, f_val, gg_p, eps))
    }

    # Polynomial trend analysis
    poly_formula <- as.formula(paste(variable, "~ block_f * condition + Error(p_num/block_f)"))
    poly_model <- aov(poly_formula, data = block_summary)
    poly_result <- capture.output(summary(poly_model,
      split = list(block_f = split_list, "block_f:condition" = split_list)))

    # Per-condition polynomial trend analyses
    conditions <- unique(as.character(block_summary$condition))
    per_cond_lines <- c()
    for (cond in conditions) {
      cond_data <- block_summary[block_summary$condition == cond, ]
      poly_formula_cond <- as.formula(paste(variable, "~ block_f + Error(p_num/block_f)"))
      poly_model_cond <- aov(poly_formula_cond, data = cond_data)
      poly_result_cond <- capture.output(summary(poly_model_cond,
        split = list(block_f = split_list)))
      per_cond_lines <- c(per_cond_lines, "", paste("Condition:", cond), poly_result_cond)
    }

    writeLines(c("Repeated-Measures ANOVA (block as factor):", "",
                 anova_result, "",
                 "Partial Eta Squared:", eta_result, "",
                 "Sphericity Tests:", sphericity_result, "",
                 gg_lines, "",
                 "Polynomial Trend Analysis:", poly_result, "",
                 "Per-Condition Polynomial Trend Analysis:", per_cond_lines),
               here(save_path, paste0(variable, "_RM_ANOVA.txt")))

    p <- ggplot(block_summary, aes(x = block_f, y = .data[[variable]], color = condition)) +
      geom_beeswarm(alpha = 0.3, dodge.width = 0.5) +
      stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.5)) +
      stat_summary(fun = mean, geom = "line", aes(group = condition), position = position_dodge(width = 0.5)) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
            legend.background = element_rect(fill = alpha("white", 0.8), color = NA)) +
      scale_color_discrete(labels = function(x) {
        x <- sub("^\\d+% ", "", x)
        ifelse(x == "IR", "Increasing Reliability",
          ifelse(x == "DR", "Decreasing Reliability", x))
      }) +
      labs(x = "Block", y = ifelse(variable == "confidence", "Self-Confidence",
             ifelse(variable == "performance_post", "Post-Rec Performance", str_to_title(variable))))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_block_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] + plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 25, height = 8
  ))

  # Combined figure without pre-rec performance
  combined_post <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance_post"]] + plot_list[["dependence"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined_post_rec.png"),
    plot = combined_post, device = "png",
    width = 20, height = 8
  ))

  return(invisible(TRUE))
}
