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
    name = feedback_50_general_confidence_by_performance_slopes,
    command = confidence_by_performance_slopes(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_general_trust_by_reliability_slopes,
    command = trust_by_reliability_slopes(data_feedback_50, experiment = "feedback", version = "50")
  ),
  tar_target(
    name = feedback_50_original_BO_model,
    command = original_BO(data_feedback_50, version = "50", experiment = "feedback")
  ),
  tar_target(
    name = feedback_50_original_BO_model_sudo_reliability,
    command = original_BO_sudo_reliability(data_feedback_50, version = "50", experiment = "feedback")
  ),
  
  tar_target(
    name = data_feedback_50_specific,
    command = data_feedback_50
  )
)