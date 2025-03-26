data_checks <- function(data, version) {

  stopifnot(is.character(version), length(version) == 1)
  log_message(paste("Running data checks for:", version))

  condition_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = n_distinct(condition))
  if (any(condition_counts$n > 1)) {
    log_message(paste("Participant(s) with more than one condition:", paste(condition_counts$p_num[condition_counts$n > 1], collapse = ", ")))
  }

  trial_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = n())
  if (any(trial_counts$n != 300)) {
    log_message(paste("Participant(s) with non-300 trials:", paste(trial_counts$p_num[trial_counts$n != 300], collapse = ", ")))
  }

  block_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = n_distinct(block))
  if (any(block_counts$n != 6)) {
    log_message(paste("Participant(s) with non-6 blocks:", paste(block_counts$p_num[block_counts$n != 6], collapse = ", ")))
  }

  trust_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = sum(!is.na(trust)))
  if (any(trust_counts$n < 6)) {
    log_message(paste("Participant(s) with less than 6 trust scores:", paste(trust_counts$p_num[trust_counts$n < 6], collapse = ", ")))
  }

  confidence_counts <- data %>%
    group_by(p_num) %>%
    summarise(n = sum(!is.na(confidence)))
  if (any(confidence_counts$n < 6)) {
    log_message(paste("Participant(s) with less than 6 confidence scores:", paste(confidence_counts$p_num[confidence_counts$n < 6], collapse = ", ")))
  }

  color1_counts <- data %>%
    group_by(p_num, color1) %>%
    summarise(n = n(), .groups = "drop")
  if (any(color1_counts$n < 10) || any(color1_counts$n > 300)) {
    log_message(paste("Participant(s) with color1 counts below 10 or above 300:", paste(color1_counts$p_num[color1_counts$n < 10 | color1_counts$n > 300], collapse = ", ")))
  }

  if (version != "feedback") {
    color2_counts <- data %>%
      group_by(p_num, color2) %>%
      summarise(n = n(), .groups = "drop")
    if (any(color2_counts$n < 10) || any(color2_counts$n > 300)) {
      log_message(paste("Participant(s) with color2 counts below 10 or above 300:", paste(color2_counts$p_num[color2_counts$n < 10 | color2_counts$n > 300], collapse = ", ")))
    }
  }

  if (version == "feedback+reliance") {
    auto_color_counts <- data %>%
      group_by(p_num, auto_color) %>%
      summarise(n = n(), .groups = "drop")
    if (any(auto_color_counts$n < 10) || any(auto_color_counts$n > 300)) {
      log_message(paste("Participant(s) with auto_color counts below 10 or above 300:", paste(auto_color_counts$p_num[auto_color_counts$n < 10 | auto_color_counts$n > 300], collapse = ", ")))
    }
  }

  invisible(NULL)

}