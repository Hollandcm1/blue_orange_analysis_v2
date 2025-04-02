# setup.R
# renv::activate()

library(yaml)

get_dependencies <- function(yaml_path = "dependencies.yml") {
  deps <- yaml::read_yaml(yaml_path)
  unlist(deps$packages)
}