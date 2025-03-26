library(targets)
library(visNetwork)
library(htmlwidgets)
library(igraph)

tv <- function() {
  tar_visnetwork(
    targets_only = FALSE,
    physics = TRUE
  ) %>%
  visHierarchicalLayout(
    direction = "LR" #,       # UD = up-down (top-to-bottom)
    #levelSeparation = 150,   # increase/decrease vertical spacing
    #nodeSpacing = 150
  ) %>%
  visPhysics(stabilization = FALSE)
}

tv2 <- function() {
  tar_visnetwork(
    targets_only = TRUE,
    physics = TRUE
  ) %>%
  visHierarchicalLayout(
    direction = "LR" #,       # UD = up-down (top-to-bottom)
    #levelSeparation = 150,   # increase/decrease vertical spacing
    #nodeSpacing = 150
  ) %>%
  visPhysics(stabilization = FALSE)
}

tv_feedback <- function() {
}

tm <- function() {
  tar_make(
  )
}

tr <- function(...) {
  tar_read(...)  
}

V <- function(...) {
  View(...)
}

ti <- function() {
  tar_invalidate(everything())
}

trV <- function(...) {
  tar_read(...) %>% View()
}
