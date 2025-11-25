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
    feedback_70_general_confidence_by_performance_slopes,
    confidence_by_performance_slopes(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_general_trust_by_reliability_slopes,
    trust_by_reliability_slopes(data_feedback_70, experiment = "feedback", version = '70')
  ),
  tar_target(
    feedback_70_original_BO_model,
    original_BO(data_feedback_70, version = "70", experiment = "feedback")
  ),
  tar_target(
    feedback_70_original_BO_model_sudo_reliability,
    original_BO_sudo_reliability(data_feedback_70, version = "70", experiment = "feedback")
  ),

  tar_target(
    data_feedback_70_specific,
    data_feedback_70
  )
)