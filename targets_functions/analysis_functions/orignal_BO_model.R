# reproduce the original BO model

original_BO <- function(data, version, experiment) {

  # save path
  output_folder <- here("output", experiment, version)
  sub_folders <- c("trust")
  walk(sub_folders, ~dir.create(here(output_folder, .x), recursive = TRUE, showWarnings = FALSE))

  # Summarize data
  block_summary <- get_block_summary(data)
  participant_summary <- get_participant_summary(data)

  #############
  ### Model ###
  #############

  model <- lmer(
    trust ~ condition * reliability_level * confidence + (1 | p_num),
    block_summary
  )
  model_summary <- capture.output(summary(model))
  writeLines(as.character(model_summary), here(output_folder, "trust", "trust_LME.txt"))
  model_summary_formated <- tab_model(model)
  writeLines(as.character(model_summary_formated$knitr), here(output_folder, "trust", "trust_LME_formatted.html"))



}