# _targets.R
source("setup.R")
source("wrappers.R")

library(here)
library(conflicted)
library(targets)
library(tibble)


packages <- get_dependencies()

tar_option_set(
  packages = get_dependencies(),
  error = "continue"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
# conflict_prefer("tibble", "tibble")

tar_source(files = here("targets_functions"))

# extra setup
generate_extra_paths()

# Explicitly source and combine your modular scripts:
c(
  source("targets/00_logs_targets.R")$value,
  # source("targets/01_matlab_targets.R")$value,
  source("targets/02_data_processing_targets.R")$value,
  source("targets/03_exploration_targets.R")$value,
  source("targets/04A_feedback_50_targets.R")$value,
  source("targets/04B_feedback_70_targets.R")$value,
  source("targets/04C_dependence_50_targets.R")$value,
  source("targets/04D_dependence_70_targets.R")$value,
  source("targets/04_general_analysis_targets.R")$value
)
