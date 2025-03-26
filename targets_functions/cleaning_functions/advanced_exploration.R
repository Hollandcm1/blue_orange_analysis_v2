advanced_exploration <- function(data, version) {
  print(paste("Creating Advanced Exploratory Data Analysis Plots for:", version))

  output_folder <- switch(
    version,
    "feedback" = "output/explitory/feedback",
    "reliance" = "output/explitory/reliance",
    "feedback+reliance" = "output/explitory/feedback+reliance",
    stop("Invalid version.")
  )

  # make continuous version of p_num
  data$p_num <- as.numeric(as.factor(data$p_num))

  dir.create(here(output_folder, "_combined"), recursive = TRUE, showWarnings = FALSE)

  p_nums <- data %>% distinct(p_num)
  p <- ggplot(p_nums, aes(x = seq_len(nrow(p_nums)), y = p_num)) +
    geom_point() +
    labs(y = "Participant") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "p_nums.png"), plot = p))

  data_points <- data %>%
    group_by(p_num) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(data_points, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Data Points") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "data_points.png"), plot = p))

  condition_counts <- data %>%
    group_by(condition) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(condition_counts, aes(x = condition, y = n)) +
    geom_bar(stat = "identity") +
    labs(x = "Condition", y = "Number of Participants") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "condition_counts.png"), plot = p))

  trial_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = n())
  p <- ggplot(trial_counts, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Trials") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "trial_counts.png"), plot = p))

  block_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = n_distinct(block))
  p <- ggplot(block_counts, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Blocks") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "block_counts.png"), plot = p))

  reliabilities <- data %>%
    group_by(p_num, block) %>%
    summarise(mean_reliability = mean(reliability_level, na.rm = TRUE), .groups = "drop")
  p <- ggplot(reliabilities, aes(x = block, y = mean_reliability, color = p_num, group = p_num)) +
    geom_point() +
    geom_line() +
    labs(x = "Block", y = "Mean Reliability Level") +
    theme_minimal() +
    scale_color_gradientn(colors = rainbow(length(unique(data$p_num))))
  suppressMessages(ggsave(here(output_folder, "_combined", "reliabilities.png"), plot = p))

  trust <- data %>%
    group_by(p_num, block) %>%
    summarise(mean_trust = mean(trust, na.rm = TRUE), .groups = "drop")
  p <- ggplot(trust, aes(x = block, y = mean_trust, color = p_num, group = p_num)) +
    geom_point() +
    geom_line() +
    labs(x = "Block", y = "Mean Trust") +
    theme_minimal() +
    scale_color_gradientn(colors = rainbow(length(unique(data$p_num))))
  suppressMessages(ggsave(here(output_folder, "_combined", "trust.png"), plot = p))

  confidence <- data %>%
    group_by(p_num, block) %>%
    summarise(mean_confidence = mean(confidence, na.rm = TRUE), .groups = "drop")
  p <- ggplot(confidence, aes(x = block, y = mean_confidence, color = p_num, group = p_num)) +
    geom_point() +
    geom_line() +
    labs(x = "Block", y = "Mean Confidence") +
    theme_minimal() +
    scale_color_gradientn(colors = rainbow(length(unique(data$p_num))))
  suppressMessages(ggsave(here(output_folder, "_combined", "confidence.png"), plot = p))

  condition_by_participant <- data %>%
    group_by(p_num, condition) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(condition_by_participant, aes(x = p_num, y = n, color = condition)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Trials") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "condition_by_participant.png"), plot = p))

  # number of valid trust scores per participant 
  trust_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = sum(!is.na(trust)), .groups = "drop")
  p <- ggplot(trust_counts, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Trust Scores") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "trust_counts.png"), plot = p))

  # number of valid confidence scores per participant
  confidence_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = sum(!is.na(confidence)), .groups = "drop")
  p <- ggplot(confidence_counts, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Confidence Scores") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "confidence_counts.png"), plot = p))

  # proportion of color 1 (orange vs blue)
  color1_counts <- data %>%
    group_by(p_num, color1) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(color1_counts, aes(x = p_num, y = n, fill = color1)) +
    geom_bar(stat = "identity") +
    labs(x = "Participant", y = "Number of Color1") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "color1_counts.png"), plot = p, width = 20))

  # proportion of color 2 (orange vs blue)
  color2_counts <- data %>%
    group_by(p_num, color2) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(color2_counts, aes(x = p_num, y = n, fill = color2)) +
    geom_bar(stat = "identity") +
    labs(x = "Participant", y = "Number of Color2") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "color2_counts.png"), plot = p, width = 20))

  # number of color changes vs no changes
  color_changes <- data %>%
    group_by(p_num) %>%
    summarise(n = sum(color1 != color2), .groups = "drop")
  p <- ggplot(color_changes, aes(x = p_num, y = n)) +
    geom_point() +
    labs(x = "Participant", y = "Number of Color Changes") +
    theme_minimal() +
    ylim(0, 300)
  suppressMessages(ggsave(here(output_folder, "_combined", "color_changes.png"), plot = p))

  # automation color selection proportions
  color_selections <- data %>%
    group_by(p_num, auto_color) %>%
    summarise(n = n(), .groups = "drop")
  p <- ggplot(color_selections, aes(x = p_num, y = n, fill = auto_color)) +
    geom_bar(stat = "identity") +
    labs(x = "Participant", y = "Number of Color Selections") +
    theme_minimal()
  suppressMessages(ggsave(here(output_folder, "_combined", "color_selections.png"), plot = p, width = 20))


  invisible(NULL)
}