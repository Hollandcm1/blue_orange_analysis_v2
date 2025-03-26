library(here)
library(dplyr)

load_data_for_r <- function(version) {
  filename <- "data_long_" %>% paste0(version, ".csv")
  filepath <- here("data", "processed", filename)
  data <- read.csv(filepath)
  return(data)
}