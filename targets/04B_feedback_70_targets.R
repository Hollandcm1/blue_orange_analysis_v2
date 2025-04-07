list(

  tar_target(
    feedback_70_general_ANOVAs,
    simple_ANOVAs(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_general_LMEs,
    simple_LMEs(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_general_figures,
    simple_figures(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_general_confidence_by_performance_calibration,
    confidence_by_performance_calibration(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_general_trust_by_reliability_calibration,
    trust_by_reliability_calibration(data_feedback_70, experiment = "feedback", version = '70')
  )
)