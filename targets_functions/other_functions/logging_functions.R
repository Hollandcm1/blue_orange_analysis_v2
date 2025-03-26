log_message <- function(msg, log_file = here("output", "logs", paste0("data_check_log_", Sys.Date(), ".txt"))) {
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
  full_msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
  cat(full_msg, "\n")
  cat(full_msg, "\n", file = log_file, append = TRUE)
}