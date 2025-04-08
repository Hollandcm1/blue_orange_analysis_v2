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
        reliance_50_general_confidence_by_performance_slopes,
        confidence_by_performance_slopes(data_reliance_50, experiment = "dependence", version = "50")
    ),
    tar_target(
        reliance_50_general_trust_by_reliability_slopes,
        trust_by_reliability_slopes(data_reliance_50, experiment = "dependence", version = "50")
    )
)