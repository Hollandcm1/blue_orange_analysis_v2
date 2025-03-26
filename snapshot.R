# snapshot.R
source("setup.R")

packages <- get_dependencies()

install.packages(packages)

# snapshot exact package versions
renv::snapshot()
