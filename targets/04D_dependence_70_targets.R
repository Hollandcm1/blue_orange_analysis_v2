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
    reliance_70_general_confidence_by_performance_slopes,
    confidence_by_performance_slopes(data_reliance_70, experiment = "dependence", version = '70')
  ),
  tar_target(
    reliance_70_general_trust_by_reliability_slopes,
    trust_by_reliability_slopes(data_reliance_70, experiment = "dependence", version = '70')
  )
)