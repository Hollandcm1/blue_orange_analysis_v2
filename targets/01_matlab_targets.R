list(

  # Copy MATLAB data
  tar_target(
    matlab_data_copied,
    copy_matlab_data()
  ),

  # Format data using MATLAB script
  tar_target(
    format_data,
    {
      # Explicit dependency calls
      matlab_data_copied
      tar_deps(c("run_format_all_data.m", "func_format_all_data_v2.m"))

      output_file <- "data/processed/pre_processed_data_feedback.mat"

      matlab_command <- paste0(
        "matlab -batch \"",
        "try, run('targets_functions/run_format_all_data.m'), ",
        "catch, exit(1), end; exit;\""
      )

      result <- system(matlab_command)

      if (result != 0) stop("MATLAB script run_format_all_data.m failed.")

      output_file
    },
    format = "file"
  ),

  # Convert data to long format using MATLAB script
  tar_target(
    long_data,
    {
      format_data
      tar_deps(c("run_make_data_long.m", "func_make_data_long.m"))

      output_file <- "data/processed/data_long_feedback.csv"

      matlab_command <- paste0(
        "matlab -batch \"",
        "try, run('targets_functions/run_make_data_long.m'), ",
        "catch, exit(1), end; exit;\""
      )

      result <- system(matlab_command)

      if (result != 0) stop("MATLAB script run_make_data_long.m failed.")

      output_file
    },
    format = "file"
  )

)