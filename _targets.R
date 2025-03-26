# _targets.R
source("setup.R")
library(here)
library(conflicted)

packages <- get_dependencies()

library(targets)
tar_option_set(
  packages = get_dependencies(),
  error = "continue"
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("tibble", "tibble")

# tar_source("targets_functions/")
tar_source(files = here("targets_functions"))
# tar_source(files = here("targets_functions/matlab_scripts"))

# extra setup
generate_extra_paths()

# Explicitly source and combine your modular scripts:
c(
  source("targets/00_logs_targets.R")$value,
  # source("targets/01_matlab_targets.R")$value,
  source("targets/02_data_processing_targets.R")$value,
  source("targets/03_exploration_targets.R")$value
)
