
feedback_ANOVA_reliability_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("feedback_", version), "feedback_ANOVA_reliability_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      .groups = "drop"
    )

  variables <- c("trust", "performance", "confidence")

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
      scale_color_discrete(labels = function(x) sub(paste0(version, "% "), "", x)) +
      labs(x = "Reliability Level", y = ifelse(variable == "confidence", "Self-Confidence", str_to_title(variable)))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_reliability_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 15, height = 8
  ))

  return(invisible(TRUE))
}


feedback_ANOVA_block_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("feedback_", version), "feedback_ANOVA_block_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      .groups = "drop"
    )

  variables <- c("trust", "performance", "confidence")

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
      scale_color_discrete(labels = function(x) sub(paste0(version, "% "), "", x)) +
      labs(x = "Block", y = ifelse(variable == "confidence", "Self-Confidence", str_to_title(variable)))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_block_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 15, height = 8
  ))

  return(invisible(TRUE))
}


feedback_RM_ANOVA_reliability_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("feedback_", version), "feedback_RM_ANOVA_reliability_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      .groups = "drop"
    )

  block_summary$reliability_level_f <- as.factor(block_summary$reliability_level)
  block_summary$p_num <- as.factor(block_summary$p_num)

  # Set polynomial contrasts for trend analysis
  contrasts(block_summary$reliability_level_f) <- contr.poly(nlevels(block_summary$reliability_level_f))

  variables <- c("trust", "performance", "confidence")

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
      scale_color_discrete(labels = function(x) sub(paste0(version, "% "), "", x)) +
      labs(x = "Reliability Level", y = ifelse(variable == "confidence", "Self-Confidence", str_to_title(variable)))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_reliability_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 15, height = 8
  ))

  return(invisible(TRUE))
}


feedback_RM_ANOVA_block_version <- function(data, version) {

  save_path <- here("output", "specific", paste0("feedback_", version), "feedback_RM_ANOVA_block_version")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE),
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block, na.rm = TRUE),
      .groups = "drop"
    )

  block_summary$block_f <- as.factor(block_summary$block)
  block_summary$p_num <- as.factor(block_summary$p_num)

  # Set polynomial contrasts for trend analysis
  contrasts(block_summary$block_f) <- contr.poly(nlevels(block_summary$block_f))

  variables <- c("trust", "performance", "confidence")

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
      scale_color_discrete(labels = function(x) sub(paste0(version, "% "), "", x)) +
      labs(x = "Block", y = ifelse(variable == "confidence", "Self-Confidence", str_to_title(variable)))

    plot_list[[variable]] <- p

    suppressMessages(ggsave(
      here(save_path, paste0(variable, "_by_block_and_condition.png")),
      plot = p, device = "png",
      width = 8, height = 8
    ))
  }

  # Combined figure with all DVs side by side
  combined <- plot_list[["trust"]] + plot_list[["confidence"]] +
    plot_list[["performance"]] +
    plot_layout(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  suppressMessages(ggsave(
    here(save_path, "all_DVs_combined.png"),
    plot = combined, device = "png",
    width = 15, height = 8
  ))

  return(invisible(TRUE))
}
