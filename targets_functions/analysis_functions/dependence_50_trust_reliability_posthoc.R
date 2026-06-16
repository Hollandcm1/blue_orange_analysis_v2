
trust_reliability_posthoc_50_increasing <- function(data) {

  save_path <- here(
    "output", "specific", "dependence_50",
    "trust_reliability_posthoc_increasing"
  )
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # Filter to increasing condition and summarise to block level
  block_summary <- data %>%
    filter(condition == "50% IR") %>%
    group_by(p_num, block) %>%
    summarise(
      trust            = mean(trust, na.rm = TRUE),
      confidence       = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      dependence       = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      reliability_level_f = factor(reliability_level)
    )

  #############
  ### Models ###
  #############

  # Continuous model (same as the parent analysis, kept for reference)
  model_continuous <- lmer(
    dependence ~ trust * reliability_level + (1 | p_num),
    data = block_summary
  )
  model_summary <- capture.output(summary(model_continuous))
  writeLines(
    as.character(model_summary),
    here(save_path, "model_trust_x_reliability_continuous.txt")
  )
  tab_model(model_continuous, show.stat = TRUE)$knitr %>%
    writeLines(here(save_path, "model_trust_x_reliability_continuous_formatted.html"))

  # Factor model: reliability_level as factor so each level gets its own slope
  model_factor <- lmer(
    dependence ~ trust * reliability_level_f + (1 | p_num),
    data = block_summary
  )
  model_summary <- capture.output(summary(model_factor))
  writeLines(
    as.character(model_summary),
    here(save_path, "model_trust_x_reliability_factor.txt")
  )
  tab_model(model_factor, show.stat = TRUE)$knitr %>%
    writeLines(here(save_path, "model_trust_x_reliability_factor_formatted.html"))

  #############################
  ### Simple slopes (emmeans) ###
  #############################

  # Trust slope at each reliability level with 95% CIs and p-values
  trust_slopes <- emmeans::emtrends(
    model_factor, ~ reliability_level_f, var = "trust"
  )
  slopes_summary <- summary(trust_slopes, infer = TRUE)
  writeLines(
    capture.output(print(slopes_summary)),
    here(save_path, "simple_slopes_by_reliability_level.txt")
  )

  # Pairwise comparisons of trust slopes across reliability levels
  slope_contrasts <- pairs(trust_slopes, adjust = "holm")
  writeLines(
    capture.output(print(summary(slope_contrasts))),
    here(save_path, "slope_pairwise_comparisons.txt")
  )

  slopes_df <- as.data.frame(slopes_summary)

  # Compact letter display: groups sharing a letter are not significantly different
  cld_df <- as.data.frame(multcomp::cld(trust_slopes, adjust = "holm", Letters = letters)) %>%
    mutate(
      reliability_level_f = factor(
        reliability_level_f,
        levels = sort(as.numeric(as.character(reliability_level_f)))
      ),
      .group = trimws(.group)
    )

  # Pairwise p-value matrix for heatmap
  contrasts_df <- as.data.frame(summary(slope_contrasts)) %>%
    mutate(
      level1 = trimws(sub(" - .*", "", contrast)),
      level2 = trimws(sub(".* - ", "", contrast))
    )
  rel_level_order <- as.character(sort(as.numeric(unique(c(contrasts_df$level1, contrasts_df$level2)))))
  contrasts_sym <- bind_rows(
    contrasts_df %>% select(level1, level2, p.value),
    contrasts_df %>% select(level1 = level2, level2 = level1, p.value)
  ) %>%
    mutate(
      level1 = factor(level1, levels = rel_level_order),
      level2 = factor(level2, levels = rel_level_order)
    )

  #############
  ### Figures ###
  #############

  # --- 1. interact_plot: model-predicted dependence by trust per reliability level ---
  p_interact <- interactions::interact_plot(
    model    = model_factor,
    pred     = trust,
    modx     = reliability_level_f,
    plot.points = TRUE,
    point.alpha = 0.25,
    x.label  = "Trust",
    y.label  = "Dependence",
    legend.main = "Reliability Level"
  ) +
    theme_minimal() +
    labs(title = "Trust × Reliability Interaction on Dependence (Increasing)")

  suppressMessages(ggsave(
    here(save_path, "interact_plot_trust_by_reliability.png"),
    plot = p_interact, device = "png",
    width = 10, height = 7
  ))

  # --- 2. Coefficient plot: trust slope at each reliability level with 95% CIs ---
  slopes_df$reliability_level_f <- factor(
    slopes_df$reliability_level_f,
    levels = sort(as.numeric(as.character(slopes_df$reliability_level_f)))
  )

  p_slopes <- ggplot(
    slopes_df,
    aes(x = reliability_level_f, y = trust.trend, ymin = lower.CL, ymax = upper.CL)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbar(width = 0.2) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
      title = "Effect of Trust on Dependence at Each Reliability Level (Increasing)",
      x     = "Reliability Level",
      y     = "Trust → Dependence Slope (95% CI)"
    )

  suppressMessages(ggsave(
    here(save_path, "trust_slopes_by_reliability_level.png"),
    plot = p_slopes, device = "png",
    width = 8, height = 6
  ))

  # --- 3. Slopes + CLD letters: same plot with letter groupings added ---
  p_slopes_cld <- ggplot(
    cld_df,
    aes(x = reliability_level_f, y = trust.trend, ymin = lower.CL, ymax = upper.CL)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbar(width = 0.2) +
    geom_point(size = 3) +
    geom_text(aes(y = upper.CL, label = .group), vjust = -0.5, size = 4.5) +
    theme_minimal() +
    labs(
      title = "Trust Slopes by Reliability Level with Grouping Letters (Increasing)",
      x       = "Reliability Level",
      y       = "Trust Slope (95% CI)",
      caption = "Levels sharing a letter do not differ significantly (Holm correction)"
    )

  suppressMessages(ggsave(
    here(save_path, "trust_slopes_by_reliability_level_cld.png"),
    plot = p_slopes_cld, device = "png",
    width = 8, height = 6
  ))

  # --- 4. Pairwise p-value heatmap: all reliability level pairs ---
  p_heatmap <- ggplot(contrasts_sym, aes(x = level1, y = level2, fill = p.value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", p.value)), size = 3.2) +
    scale_fill_gradient(
      low = "steelblue", high = "grey92",
      limits = c(0, 1), name = "p-value (Holm)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Pairwise Comparisons of Trust Slopes by Reliability Level (Increasing)",
      x = "Reliability Level",
      y = "Reliability Level"
    )

  suppressMessages(ggsave(
    here(save_path, "trust_slopes_pairwise_heatmap.png"),
    plot = p_heatmap, device = "png",
    width = 7, height = 6
  ))

  # --- 5. Raw scatter faceted by reliability level with per-level regression line ---
  p_facet <- ggplot(block_summary, aes(x = trust, y = dependence)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.2) +
    facet_wrap(~ reliability_level_f, nrow = 1) +
    theme_minimal() +
    labs(
      title = "Trust-Dependence Relationship by Reliability Level (Increasing)",
      x     = "Trust",
      y     = "Dependence"
    ) +
    xlim(0, 100)

  suppressMessages(ggsave(
    here(save_path, "trust_dependence_by_reliability_level_facet.png"),
    plot = p_facet, device = "png",
    width = 16, height = 5
  ))

  return(invisible(TRUE))
}
