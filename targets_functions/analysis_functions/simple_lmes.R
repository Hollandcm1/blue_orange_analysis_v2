# Runs generic LMEs that work on all versions of the experiment

simple_LMEs <- function(data, experiment, version) {

  # print(paste0("Running LMEs for ", experiment, " ", version))

  # Define save paths
  version_string <- paste0(experiment, "_", version)
  output_folder <- here("output", "general analysis", version_string, "LMEs")
  sub_folders <- c("performance", "trust", "confidence", "dependence")

  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- data %>%
    group_by(p_num, condition, block) %>%
    summarise(
      trust = mean(trust, na.rm = TRUE), 
      confidence = mean(confidence, na.rm = TRUE),
      reliability_level = mean(reliability_level, na.rm = TRUE),
      performance = mean(percent_correct_block),
      dependence = mean(percent_dependence_block_when_possible, na.rm = TRUE),
      .groups = "drop"
    )


  #############
  ### Trust ###
  #############

  model <- lmer(
    trust ~ condition * reliability_level * confidence * performance + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust", "trust_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust", "trust_LME_formatted.html"))


  ##################
  ### Confidence ###
  ##################

  model <- lmer(
    confidence ~ condition * reliability_level * trust * performance + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "confidence", "confidence_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "confidence", "confidence_LME_formatted.html"))


  ###################
  ### Performance ###
  ###################

  model <- lmer(
    performance ~ condition * reliability_level * trust * confidence + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "performance", "performance_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "performance", "performance_LME_formatted.html"))

  
  ##################
  ### Dependence ###
  ##################

  if (experiment == "dependence") {
    model <- lmer(
      dependence ~ condition * reliability_level * trust * confidence + (1 | p_num),
      block_summary
    )
    model_summary <- capture.output(summary(model))
    writeLines(as.character(model_summary), here(output_folder, "dependence", "dependence_LME.txt"))
    model_summary_formated <- tab_model(model)
    writeLines(as.character(model_summary_formated$knitr), here(output_folder, "dependence", "dependence_LME_formatted.html"))
  }

}