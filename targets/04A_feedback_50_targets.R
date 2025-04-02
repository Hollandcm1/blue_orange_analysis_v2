list(
  tar_target(
    name = feedback_50_ANOVAs,
    command = simple_ANOVAs(data_feedback_50, experiment = "feedback", version = "50")
  )
)