list (

  # --- Raw Data Exploration ---

  tar_target(
    data_feedback_explored,
    basic_exploration(data_feedback_corrected, version = 'feedback')
  ),

  tar_target(
    data_reliance_explored,
    basic_exploration(data_reliance_corrected, version = 'reliance')
  ),

  tar_target(
    data_feedback_and_reliance_explored,
    basic_exploration(data_feedback_and_reliance_corrected, version = 'feedback+reliance')
  ),

  tar_target(
    data_feedback_explored_advanced,
    advanced_exploration(data_feedback_corrected, version = 'feedback')
  ),

  tar_target(
    data_reliance_explored_advanced,
    advanced_exploration(data_reliance_corrected, version = 'reliance')
  ),

  tar_target(
    data_feedback_and_reliance_explored_advanced,
    advanced_exploration(data_feedback_and_reliance_corrected, version = 'feedback+reliance')
  ),

  tar_target(
    data_feedback_checked,
    data_checks(data_feedback_corrected, version = 'feedback')
  ),

  tar_target(
    data_reliance_checked,
    data_checks(data_reliance_corrected, version = 'reliance')
  ),

  tar_target(
    data_feedback_and_reliance_checked,
    data_checks(data_feedback_and_reliance_corrected, version = 'feedback+reliance')
  ),

  # --- Composite Data Exploration ---

  tar_target(
    name = explore_composites_feedback,
    command = explore_composites(data_all_and_composites, version = 'feedback')
  ),

  tar_target(
    name = explore_composites_reliance,
    command = explore_composites(data_all_and_composites, version = 'reliance')
  ),

  tar_target(
    name = explore_composites_feedback_and_reliance,
    command = explore_composites(data_all_and_composites, version = 'feedback+reliance')
  ),

  # --- Corrected Composite Data Exploration (Cleaned) ---

  tar_target(
    name = explore_composites_feedback_cleaned,
    command = explore_composites(data_corrected, version = 'feedback', before_cleaning = FALSE)
  ),

  tar_target(
    name = explore_composites_reliance_cleaned,
    command = explore_composites(data_corrected, version = 'reliance', before_cleaning = FALSE)
  ),

  tar_target(
    name = explore_composites_feedback_and_reliance_cleaned,
    command = explore_composites(data_corrected, version = 'feedback+reliance', before_cleaning = FALSE)
  )

)