

confidence_by_performance_calibration <- function(data, experiment, version) {

  source(here("targets_functions/other_functions/data_summary_functions.R"))

  # Define save path
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "calibrations", version_string)
  sub_folders <- c("confidence_calibration")
  
  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- get_block_summary(data)
  participant_summary <- get_participant_summary(data)

  ################
  ### modeling ###
  ################

  # confidence by performance
  model <- lmer(
    confidence ~ performance + (1 | p_num),
    data = block_summary,
    REML = FALSE
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "confidence_calibration", "confidence_by_performance_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "confidence_calibration", "confidence_LME_formatted.html"))

  # confidence calibration by performance
  model <- lm(
    confidence_calibration ~ performance + (1 | p_num),
    data = participant_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "confidence_calibration", "confidence_calibration_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "confidence_calibration", "confidence_calibration_LME_formatted.html"))


  #############
  ### Plots ###
  #############
  
  # scatterplot of confidence by performance
  p <- ggplot(block_summary, aes(x = performance, y = confidence)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Confidence Calibration on Performance",
          x = "Performance", y = "Confidence") +
    xlim(0, 102) +
    ylim(0, 102) 
  suppressMessages(
    ggsave(
      here(output_folder, "confidence_calibration", "confidence_by_performance.png"), 
      p, width = 6, height = 6
    )
  )

  # scatterplot of confidence by performance grouped by participant
  p <- ggplot(block_summary, aes(x = performance, y = confidence, color = p_num, group = p_num)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(title = "Confidence Calibration on Performance by Participant",
          x = "Performance", y = "Confidence") +
    xlim(0, 102) +
    ylim(0, 102) +
    scale_color_gradientn(colors = rainbow(length(unique(block_summary$p_num))))
  suppressMessages(
    ggsave(
      here(output_folder, "confidence_calibration", "confidence_on_performance_by_participant.png"), 
      p, width = 6, height = 6
    )
  )

  # scatterplot of confidence calibration by performance
  p <- ggplot(participant_summary, aes(x = performance, y = confidence_calibration)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Confidence Calibration on Performance",
          x = "Performance", y = "Confidence Calibration")
  suppressMessages(
    ggsave(
      here(output_folder, "confidence_calibration", "confidence_calibration_on_performance.png"), 
      p, width = 6, height = 6
    )
  )

}


trust_by_reliability_calibration <- function(data, experiment, version) {

  source(here("targets_functions/other_functions/data_summary_functions.R"))

  # Define save path
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "calibrations", version_string)
  sub_folders <- c("trust_calibration")

  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- get_block_summary(data)
  participant_summary <- get_participant_summary(data)

  ################
  ### modeling ###
  ################

  # trust by performance
  model <- lmer(
    trust ~ reliability_level + (1 | p_num),
    data = block_summary,
    REML = FALSE
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust_calibration", "trust_by_reliability_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust_calibration", "trust_LME_formatted.html"))
  
  # trust calibration by performance
  model <- lm(
    trust_calibration ~ performance + (1 | p_num),
    data = participant_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust_calibration", "trust_calibration_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust_calibration", "trust_calibration_LME_formatted.html"))

  #############
  ### Plots ###
  #############

  # scatterplot of trust by reliability
  p <- ggplot(block_summary, aes(x = reliability_level, y = trust)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Trust Calibration on Reliability Level",
          x = "Reliability Level", y = "Trust") +
    xlim(0, 102) +
    ylim(0, 102)
  suppressMessages(
    ggsave(
      here(output_folder, "trust_calibration", "trust_by_reliability.png"), 
      p, width = 6, height = 6
    )
  )

  # scatterplot of trust by reliability grouped by participant
  p <- ggplot(block_summary, aes(x = reliability_level, y = trust, color = p_num, group = p_num)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(title = "Trust Calibration on Reliability Level by Participant",
          x = "Reliability Level", y = "Trust") +
    xlim(0, 102) +
    ylim(0, 102) +
    scale_color_gradientn(colors = rainbow(length(unique(block_summary$p_num))))
  suppressMessages(
    ggsave(
      here(output_folder, "trust_calibration", "trust_on_reliability_by_participant.png"), 
      p, width = 6, height = 6
    )
  )
  
  # scatterplot of trust calibration by performance
  p <- ggplot(participant_summary, aes(x = performance, y = trust_calibration)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Trust Calibration on Performance",
          x = "Performance", y = "Trust Calibration")
  suppressMessages(
    ggsave(
      here(output_folder, "trust_calibration", "trust_calibration_on_performance.png"), 
      p, width = 6, height = 6
    )
  )

}
