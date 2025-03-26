list(
    
  tar_target(
    log_file,
    here::here("output", "logs", paste0("data_check_log_", Sys.Date(), ".txt")),
    format = "file"
  )
  
)