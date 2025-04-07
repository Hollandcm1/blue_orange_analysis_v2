list(
  tar_target(
    reliance_70_general_ANOVAs,
    simple_ANOVAs(data_reliance_70, experiment = "dependence", version = '70')
  ),
  tar_target(
    reliance_70_general_LMEs,
    simple_LMEs(data_reliance_70, experiment = "dependence", version = '70')
  ),
  tar_target(
    reliance_70_general_figures,
    simple_figures(data_reliance_70, experiment = "dependence", version = '70')
  ),
  tar_target(
    reliance_70_general_confidence_by_performance_calibration,
    confidence_by_performance_calibration(data_reliance_70, experiment = "dependence", version = '70')
  ),
  tar_target(
    reliance_70_general_trust_by_reliability_calibration,
    trust_by_reliability_calibration(data_reliance_70, experiment = "dependence", version = '70')
  )
)