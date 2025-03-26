library(ggplot2)
library(here)
library(dplyr)
library(purrr)
library(readr)

data_formatting_and_corrections <- function(data, version) {
  
  # Define condition replacements by version
  condition_map <- list(
    feedback = c(
      "50%25InF" = "50% IF",
      "50%25DeF" = "50% DF",
      "70%25InF" = "70% IF",
      "70%25DeF" = "70% DF"
    ),
    reliance = c(
      "50%25InR" = "50% IR",
      "50%25DeR" = "50% DR",
      "70%25InR" = "70% IR",
      "70%25DeR" = "70% DR"
    ),
    `feedback+reliance` = c(
      "50%25InFe" = "50% IF+R",
      "50%25DeFe" = "50% DF+R"
    )
  )

  if (!version %in% names(condition_map)) {
    stop("Invalid version.")
  }

  # Replace encoded conditions
  for (pattern in names(condition_map[[version]])) {
    data$condition <- gsub(pattern, condition_map[[version]][[pattern]], data$condition)
  }

  # Assign experiment column
  data$experiment <- version

  # Convert variable types
  data <- data %>%
    mutate(
      p_num = as.numeric(p_num),
      trial_v1 = as.numeric(trial_v1),
      trial_v2 = as.numeric(trial_v2),
      condition = as.factor(condition),
      experiment = as.factor(experiment),
      correct1 = as.numeric(as.character(replace(correct1, is.nan(correct1), NA))),
      correct2 = as.numeric(as.character(replace(correct2, is.nan(correct2), NA)))
    )

  return(data)
}

combine_all_data <- function(df1, df2, df3) {
  combined_data <- rbind(df1, df2, df3)
  return(combined_data)
}

add_composite_values <- function(data) {

  # Check required columns exist
  required_cols <- c(
    "correct1", "correct2", "trial_v2", "color1", "color2", "auto_color",
    "confidence", "trust", "reliability_level", "experiment",
    "block", "condition", "p_num"
  )
  stopifnot(all(required_cols %in% names(data)))

  # --- Percent Correct and Block-Level Accuracy ---
  data <- data %>%
    group_by(p_num, condition) %>%
    mutate(
      percent_correct_condition  = mean(correct1 == 1) * 100,
      percent_correct_condition2 = mean(correct2 == 1) * 100
    ) %>%
    ungroup() %>%
    group_by(p_num, condition, block) %>%
    mutate(
      percent_correct_block  = mean(correct1 == 1) * 100,
      percent_correct_block2 = mean(correct2 == 1) * 100
    ) %>%
    ungroup()

  # --- Cumulative Performance ---
  data <- data %>%
    arrange(p_num, condition, trial_v2) %>%
    group_by(p_num, condition) %>%
    mutate(
      cumulative_correct      = cumsum(correct1 == 1),
      cumulative_trials       = row_number(),
      cumulative_performance  = cumulative_correct / cumulative_trials * 100,
      cumulative_correct2     = cumsum(correct2 == 1),
      cumulative_trials2      = row_number(),
      cumulative_performance2 = cumulative_correct2 / cumulative_trials2 * 100
    ) %>%
    ungroup()

  # --- Dependence Behaviour ---
  data <- data %>%
    mutate(
      dependence_behaviour = as.integer(color1 != auto_color & color1 != color2 & color2 == auto_color),
      dependence_behaviour = ifelse(experiment == "feedback", NA, dependence_behaviour),
      dependence_behaviour_possible = as.integer(color1 != auto_color)
    ) %>%
    group_by(p_num, condition) %>%
    mutate(
      percent_dependence_condition = mean(dependence_behaviour == 1, na.rm = TRUE) * 100,
      percent_dependence_condition_when_possible =
        sum(dependence_behaviour == 1, na.rm = TRUE) /
        sum(dependence_behaviour_possible == 1, na.rm = TRUE) * 100
    ) %>%
    ungroup() %>%
    group_by(p_num, condition, block) %>%
    mutate(
      percent_dependence_block = mean(dependence_behaviour == 1, na.rm = TRUE) * 100,
      percent_dependence_block_when_possible =
        sum(dependence_behaviour == 1, na.rm = TRUE) /
        sum(dependence_behaviour_possible == 1, na.rm = TRUE) * 100
    ) %>%
    ungroup()

  # --- Calibration Metrics ---
  data <- data %>%
    group_by(p_num, condition) %>%
    mutate(
      confidence_calibration  = (confidence / 100) / (percent_correct_condition / 100),
      confidence_calibration2 = (confidence / 100) / (percent_correct_condition2 / 100),
      trust_calibration       = trust / reliability_level
    ) %>%
    ungroup()

  # --- RMS Error ---
  data <- data %>%
    mutate(
      confidence_rms  = sqrt((confidence - percent_correct_condition)^2),
      confidence_rms2 = sqrt((confidence - percent_correct_condition2)^2),
      trust_rms       = sqrt((trust - reliability_level)^2)
    )

  # --- Cumulative Dependence ---
  data <- data %>%
    arrange(p_num, condition, trial_v2) %>%
    group_by(p_num, condition) %>%
    mutate(
      cumulative_dependence         = cumsum(dependence_behaviour == 1),
      cumulative_dependence_trials  = row_number(),
      percent_cumulative_dependence = cumulative_dependence / cumulative_dependence_trials * 100
    ) %>%
    ungroup()

  return(data)
}

data_corrections <- function(data, confidence_cutoff = 57.4) {

  # --- Setup ---
  save_path <- here("output", "removed_participants")
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  # --- Identify Low-Performing Participants ---
  data_altered <- data %>%
    group_by(p_num, experiment, condition) %>%
    slice(1) %>%
    ungroup()

  participants_to_remove <- data_altered %>%
    dplyr::filter(percent_correct_condition < confidence_cutoff) %>%
    dplyr::select(p_num, experiment, condition)

  # make sure it's a dataframe
  participants_to_remove <- as.data.frame(participants_to_remove)

  # sort by experiment
  participants_to_remove <- participants_to_remove %>%
    arrange(experiment)

  # Remove low-performing participants
  data <- data %>%
    dplyr::filter(percent_correct_condition >= confidence_cutoff)

  write.csv(participants_to_remove, here(save_path, "participants_to_remove.csv"))

  # --- Remove Nonsense Condition ---
  nonsense_participant <- data_altered %>%
    dplyr::filter(condition == "RTj6ALs_") %>%
    dplyr::select(p_num, experiment, condition)

  # append the nonsense participant to the participants_to_remove
  participants_to_remove <- rbind(participants_to_remove, nonsense_participant)

  # Remove nonsense condition from data
  data <- data %>%
    dplyr::filter(condition != "RTj6ALs_")


  # --- Generate Summary Table ---
  summary_counts <- participants_to_remove %>%
    group_by(experiment, condition) %>%
    summarise(n_participants = n_distinct(p_num), .groups = "drop")

  formatted_table <- summary_counts %>%
    gt() %>%
    tab_header(
      title = "Participants REMOVED by Experiment and Condition",
      subtitle = "Summary of unique participants across experiments and conditions"
    ) %>%
    cols_label(
      experiment = "Experiment",
      condition = "Condition",
      n_participants = "Number of Participants"
    ) %>%
    fmt_number(
      columns = everything(),
      decimals = 0
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    )

  # --- Save Summary Table ---
  gtsave(formatted_table, here(save_path, "participant_counts_table.html"))

  # --- Return Cleaned Data ---
  return(data)
}