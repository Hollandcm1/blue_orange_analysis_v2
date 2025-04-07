list(
  tar_target(
    name = feedback_50_general_ANOVAs,
    command = simple_ANOVAs(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_general_LMEs,
    command = simple_LMEs(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_general_figures,
    command = simple_figures(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_general_confidence_by_performance_calibration,
    command = confidence_by_performance_calibration(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_general_trust_by_reliability_calibration,
    command = trust_by_reliability_calibration(data_feedback_50, experiment = "feedback", version = "50")
  )
)