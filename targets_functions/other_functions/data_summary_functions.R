
get_block_summary <- function(data) {

  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE), 
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block),
      .groups = "drop"
    )
}

get_participant_summary <- function(data) {

  participant_summary <- data %>%
    group_by(p_num, condition) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE), 
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_condition),
      confidence_calibration = mean(confidence_calibration, na.rm = TRUE),
      trust_calibration = mean(trust_calibration, na.rm = TRUE),
      .groups = "drop"
    )
}