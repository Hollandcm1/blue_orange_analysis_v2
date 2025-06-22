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
    ),
    tar_target(
        reliance_50_original_BO_model,
        original_BO(data_reliance_50, version = "50", experiment = "dependence")
    ),

    tar_target(
        data_reliance_50_specific, #this is to make a new layers in tar vis
        data_reliance_50
    ),

    tar_target(
        seperate_increasing_vs_decreasing_analysis_50,
        seperate_increasing_vs_decreasing_LMES_50(data_reliance_50_specific)
    ), 
    tar_target(
        dependence_general_LME_50, 
        dependence_LME_50(data_reliance_50_specific, version = "50")
    )


)