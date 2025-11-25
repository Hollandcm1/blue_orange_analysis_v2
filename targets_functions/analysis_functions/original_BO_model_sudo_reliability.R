# reproduce the original BO model

original_BO_sudo_reliability <- function(data, version, experiment) {

  # save path
  output_folder <- here("output", "original_BO_model_sudo_reliability", paste0(experiment, "_", version))
  sub_folders <- c("trust")
  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  # - Add sudo_reliability (for models)
  # - Add reliability_plot (for figures only) so that 100% blocks in 70% conditions
  #   appear as 100(1), 100(2), 100(3), etc.
  block_summary <- get_block_summary(data) %>%
    dplyr::mutate(
      sudo_reliability = dplyr::case_when(
        # 70% DF / DR: reverse mapping of blocks 1–6
        condition %in% c("70% DF", "70% DR") & block == 6 ~ 1,
        condition %in% c("70% DF", "70% DR") & block == 5 ~ 2,
        condition %in% c("70% DF", "70% DR") & block == 4 ~ 3,
        condition %in% c("70% DF", "70% DR") & block == 3 ~ 4,
        condition %in% c("70% DF", "70% DR") & block == 2 ~ 5,
        condition %in% c("70% DF", "70% DR") & block == 1 ~ 6,

        # 70% IF / ID: keep block as-is
        condition %in% c("70% IF", "70% ID") ~ block,

        # All other conditions: NA
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(p_num, condition) %>%
    dplyr::arrange(block, .by_group = TRUE) %>%
    dplyr::mutate(
      # Flag 100%-reliability blocks within 70% conditions
      is_100_in_70 = reliability_level == 100 &
        condition %in% c("70% DF", "70% DR", "70% IF", "70% ID"),
      # Within each participant × condition, count the 100% blocks
      reliability100_index = dplyr::if_else(
        is_100_in_70,
        cumsum(is_100_in_70),
        NA_integer_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Label for plotting:
      #   - 100% blocks in 70% conditions -> "100(1)", "100(2)", ...
      #   - everything else -> just the reliability_level value
      reliability_plot = dplyr::case_when(
        !is.na(reliability100_index) ~ paste0("100(", reliability100_index, ")"),
        !is.na(reliability_level) ~ as.character(reliability_level),
        TRUE ~ NA_character_
      ),
      reliability_plot = factor(reliability_plot)
    )

  participant_summary <- get_participant_summary(data)

  #############
  ### Model ###
  #############

  # Models remain unchanged: they use sudo_reliability (numeric) and NOT the plot-only labels

  # MODEL 1 — sudo_reliability in the fixed effects
  model <- lmer(
    trust ~ condition * sudo_reliability * confidence + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust", "trust_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust", "trust_LME_formatted.html"))

  # MODEL 2 — remains block-based
  model <- lmer(
    trust ~ condition * block * confidence + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust", "trust_LME_block.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust", "trust_LME_formatted_block.html"))

  #############
  ### Plots ###
  #############
  # Only the figures change: wherever we previously used sudo_reliability
  # for visualization, we now use reliability_plot so that 100% blocks
  # in 70% conditions appear as 100(1), 100(2), 100(3), ...

  # --- Trust by Reliability (plot labels), Confidence, and Condition ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust, color = reliability_plot)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(
      title = "Trust by Reliability (100% blocks split), Confidence, and Condition",
      x = "Confidence", y = "Trust",
      color = "Reliability (plot)"
    ) +
    facet_wrap(~ condition) +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "3_way_trust_by_reliability_plot_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Block, Confidence, and Condition (unchanged) ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust, color = as.factor(block))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(
      title = "Trust by Block, Confidence, and Condition",
      x = "Confidence", y = "Trust",
      color = "Block"
    ) +
    facet_wrap(~ condition) +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "3_way_trust_by_block_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability (plot labels) and Condition ---
  p <- ggplot(block_summary, aes(x = reliability_plot, y = trust, color = as.factor(condition))) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
    geom_smooth(method = "lm", alpha = 0.1, se = FALSE) +
    theme_minimal() +
    labs(
      title = "Trust by Reliability (100% blocks split) and Condition",
      x = "Reliability (plot)", y = "Trust",
      color = "Condition"
    ) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_reliability_plot_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability (plot labels) and Confidence ---
  p <- ggplot(block_summary, aes(x = reliability_plot, y = trust, color = as.factor(confidence))) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
    geom_smooth(method = "lm", alpha = 0.1, se = FALSE) +
    theme_minimal() +
    labs(
      title = "Trust by Reliability (100% blocks split) and Confidence",
      x = "Reliability (plot)", y = "Trust",
      color = "Confidence"
    ) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_reliability_plot_and_confidence.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Confidence and Condition (unchanged) ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust, color = as.factor(condition))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(
      title = "Trust by Confidence and Condition",
      x = "Confidence", y = "Trust",
      color = "Condition"
    ) +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "2_way_trust_by_confidence_and_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Reliability (plot labels) ---
  p <- ggplot(block_summary, aes(x = reliability_plot, y = trust)) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
    geom_smooth(method = "lm", alpha = 0.1, se = FALSE) +
    theme_minimal() +
    labs(
      title = "Trust by Reliability (100% blocks split)",
      x = "Reliability (plot)", y = "Trust"
    ) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_reliability_plot.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Condition (unchanged) ---
  p <- ggplot(block_summary, aes(x = condition, y = trust)) +
    geom_beeswarm(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(
      title = "Trust by Condition",
      x = "Condition", y = "Trust"
    ) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_condition.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

  # --- Trust by Confidence (unchanged) ---
  p <- ggplot(block_summary, aes(x = confidence, y = trust)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", alpha = 0.1) +
    theme_minimal() +
    labs(
      title = "Trust by Confidence",
      x = "Confidence", y = "Trust"
    ) +
    xlim(0, 100) +
    ylim(0, 100)
  suppressMessages(ggsave(
    here(output_folder, "trust", "1_way_trust_by_confidence.png"),
    plot = p, device = "png",
    width = 10, height = 8
  ))

}