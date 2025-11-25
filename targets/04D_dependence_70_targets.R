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
  ),
  tar_target(
    reliance_70_original_BO_model,
    original_BO(data_reliance_70, version = "70", experiment = "dependence")
  ),
  tar_target(
    reliance_70_original_BO_model_sudo_reliability,
    original_BO_sudo_reliability(data_reliance_70, version = "70", experiment = "dependence")
  ),
  
  tar_target(
    data_reliance_70_specific,
    data_reliance_70
  ),

  tar_target(
        seperate_increasing_vs_decreasing_analysis_70,
        seperate_increasing_vs_decreasing_LMES_70(data_reliance_70_specific)
    ), 
    tar_target(
        dependence_general_LME_70, 
        dependence_LME_70(data_reliance_70_specific, version = "70")
    )
)