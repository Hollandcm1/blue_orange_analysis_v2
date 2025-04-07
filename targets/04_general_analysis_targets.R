list(

  tar_target(
    name = summary_counts_before_cleaning,
    command = summary_counts(data_all_and_composites, before_cleaning = TRUE)
  ),
  tar_target(
    name = summary_counts_after_cleaning,
    command = summary_counts(data_corrected, before_cleaning = FALSE)
  )

  # Not working unfortunately...

  # tar_target(
  #   param_grid_anova,
  #   list(
  #     list(data = data_feedback_50, experiment = "feedback", version = "50"),
  #     list(data = data_feedback_70, experiment = "feedback", version = "70"),
  #     list(data = data_reliance_50, experiment = "reliance", version = "50"),
  #     list(data = data_reliance_70, experiment = "reliance", version = "70")
  #   ),
  #   iteration = "list"
  # ),
  # tar_target(
  #   anova_results,
  #   {
  #     function(param) {
  #       simple_ANOVAs(
  #         data = param$data,
  #         experiment = param$experiment,
  #         version = param$version
  #       )
  #     }(param)
  #   },
  #   pattern = map(param = param_grid_anova),
  #   iteration = "list"
  # ),
  # tar_target(
  #   param_grid_lme,
  #   list(
  #     list(data = data_feedback_50, experiment = "feedback", version = "50"),
  #     list(data = data_feedback_70, experiment = "feedback", version = "70"),
  #     list(data = data_reliance_50, experiment = "reliance", version = "50"),
  #     list(data = data_reliance_70, experiment = "reliance", version = "70")
  #   ),
  #   iteration = "list"
  # ),
  # tar_target(
  #   lme_results,
  #   {
  #     function(param) {
  #       simple_LMEs(
  #         data = param$data,
  #         experiment = param$experiment,
  #         version = param$version
  #       )
  #     }(param)
  #   },
  #   pattern = map(param = param_grid_lme),
  #   iteration = "list"
  # )

)