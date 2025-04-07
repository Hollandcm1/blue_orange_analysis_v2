list(

    tar_target(
        reliance_50_general_ANOVAs,
        simple_ANOVAs(data_reliance_50, experiment = "dependence", version = "50")
    ),
    tar_target(
        reliance_50_general_LMEs,
        simple_LMEs(data_reliance_50, experiment = "dependence", version = "50")
    ),
    tar_target(
        reliance_50_general_figures,
        simple_figures(data_reliance_50, experiment = "dependence", version = "50")
    ),
    tar_target(
        reliance_50_general_confidence_by_performance_calibration,
        confidence_by_performance_calibration(data_reliance_50, experiment = "dependence", version = "50")
    ),
    tar_target(
        reliance_50_general_trust_by_reliability_calibration,
        trust_by_reliability_calibration(data_reliance_50, experiment = "dependence", version = "50")
    )
)