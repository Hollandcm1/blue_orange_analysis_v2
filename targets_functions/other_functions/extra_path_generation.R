library(here)

generate_extra_paths <- function() {

  ###############
  ### General ###
  ###############
  
  # Define relative paths using 'here' for portability
  path_list <- list(
    here("data", "raw"),
    here("data", "processed")
  )

  # Check if directories exist; if not, create them
  for (dir_path in path_list) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Created missing directory: %s", dir_path))
    }
  }

   # Define relative paths using 'here' for portability
  path_list <- list(
    here("data", "predefined")
  )

  # Check if directories exist; if not, create them
  for (dir_path in path_list) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Created missing directory: %s", dir_path))
      message(sprintf("This path must be populated manually!"))
    }
  }

  return(path_list)

}
