# Runs generic LMEs that work on all versions of the experiment

simple_LMEs <- function(data, experiment, version) {

  # Define save paths
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "general_analysis", version_string, "LMEs")
  sub_folders <- c("performance", "trust", "confidence")

  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE), 
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block),
      .groups = "drop"
    )

  #############
  ### Trust ###
  #############

  model <- lmer(
    trust ~ condition * reliability_level * confidence + (1 | p_num),
    block_summary
  )


}