list(



  tar_target(
    data_feedback,
    {
      # long_data
      load_data_for_r('feedback')
    }
  ),

  tar_target(
    data_reliance,
    {
      # long_data
      load_data_for_r('reliance')
    }
  ),

  tar_target(
    data_feedback_and_reliance,
    {
      # long_data
      load_data_for_r('feedback+reliance')
    }
  ),

  tar_target(
    data_feedback_corrected,
    data_formatting_and_corrections(data_feedback, version = 'feedback')
  ),

  tar_target(
    data_reliance_corrected,
    data_formatting_and_corrections(data_reliance, version = 'reliance')
  ),

  tar_target(
    data_feedback_and_reliance_corrected,
    data_formatting_and_corrections(data_feedback_and_reliance, version = 'feedback+reliance')
  ),

  tar_target(
    data_all,
    combine_all_data(
      data_feedback_corrected, 
      data_reliance_corrected, 
      data_feedback_and_reliance_corrected
    )
  ),

  tar_target(
    data_all_and_composites,
    add_composite_values(data_all)
  ),

  tar_target(
    data_corrected,
    data_corrections(data_all_and_composites)
  ),

  tar_target(
    data_reliance_50,
    data_corrected %>% filter(experiment == "reliance", condition %in% c("50% IR", "50% DR"))
  ),

  tar_target(
    data_reliance_70,
    data_corrected %>% filter(experiment == "reliance", condition %in% c("70% IR", "70% DR"))
  ),

  tar_target(
    data_feedback_50,
    data_corrected %>% filter(experiment == "feedback", condition %in% c("50% IF", "50% DF"))
  ),

  tar_target(
    data_feedback_70,
    data_corrected %>% filter(experiment == "feedback", condition %in% c("70% IF", "70% DF"))
  )

)
