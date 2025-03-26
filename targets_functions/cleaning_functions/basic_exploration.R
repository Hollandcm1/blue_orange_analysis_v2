basic_exploration <- function(data, version) {

  # Set output folder
  output_folder <- switch(
    version,
    "feedback" = "output/explitory/feedback",
    "reliance" = "output/explitory/reliance",
    "feedback+reliance" = "output/explitory/feedback+reliance",
    stop("Invalid version.")
  )

  # Ensure subfolders exist
  subfolders <- c("trial_v1", "trial_v2", "block", "trust", "confidence", 
                  "reliability_level", "response_time1", "response_time2",
                  "correct1", "correct2")
  walk(subfolders, ~ dir.create(here(output_folder, .x),
                                recursive = TRUE,
                                showWarnings = FALSE))

  print(paste("Creating Exploratory Data Analysis Plots for:", version))

  # Helper to plot participant variable over trial
  plot_participant_var <- function(data, varname, subfolder, ylab, ylimits = NULL) {
    print(paste("Creating plots for", varname, "..."))
    walk(unique(data$p_num), function(i) {
      data_i <- data %>% filter(p_num == i)
      p <- ggplot(data_i, aes(x = seq_len(nrow(data_i)), y = .data[[varname]])) +
        geom_point() +
        labs(x = "Trial", y = ylab) +
        theme_minimal()
      if (!is.null(ylimits)) p <- p + ylim(ylimits)
      suppressMessages(ggsave(
        here(output_folder, subfolder, paste0(varname, "_p_num_", i, ".png")),
        plot = p, device = "png"
      ))
    })
  }

  plot_participant_var(data, "correct1", "correct1", "Correct 1", c(0, 1))
  # only if version is not feedback
  if (version != "feedback") {
    plot_participant_var(data, "correct2", "correct2", "Correct 2", c(0, 1))
  }

  # Helper to compare participant variable with standard (p_num == 1)
  compare_with_standard <- function(data, varname, subfolder, ylab) {
    print(paste("Comparing", varname, "with standard..."))
    data_standard <- data %>% filter(p_num == 1)
    walk(unique(data$p_num), function(i) {
      data_i <- data %>% filter(p_num == i)

      p <- ggplot(data_i, aes(x = seq_len(nrow(data_i)), y = .data[[varname]])) +
        geom_point() +
        labs(x = "Trial", y = ylab) +
        theme_minimal()

      suppressMessages(ggsave(
        here(output_folder, subfolder, paste0(varname, "_p_num_", i, ".png")),
        plot = p, device = "png"
      ))

      if (i != 1 && any(data_i[[varname]] != data_standard[[varname]], na.rm = TRUE)) {
        warning(paste0("Participant ", i, " has different ", varname, " values compared to the standard."))
      }
    })
  }

  compare_with_standard(data, "trial_v1", "trial_v1", "Trial V1")
  compare_with_standard(data, "trial_v2", "trial_v2", "Trial V2")
  compare_with_standard(data, "block", "block", "Block")


  plot_participant_block_avg <- function(data, varname, subfolder, ylab) {
    print(paste("Creating block average plots for", varname, "..."))
    walk(unique(data$p_num), function(i) {
      data_i <- data %>%
        filter(p_num == i) %>%
        group_by(block) %>%
        summarise(val = mean(.data[[varname]], na.rm = TRUE), .groups = "drop")

      p <- ggplot(data_i, aes(x = block, y = val)) +
        geom_point() + geom_line() +
        labs(x = "Block", y = ylab) +
        theme_minimal() +
        ylim(0, 100)

      suppressMessages(ggsave(
        here(output_folder, subfolder, paste0(varname, "_p_num_", i, ".png")),
        plot = p, device = "png"
      ))
    })
  }

  plot_participant_block_avg(data, "reliability_level", "reliability_level", "Reliability Level")
  plot_participant_block_avg(data, "trust", "trust", "Trust")
  plot_participant_block_avg(data, "confidence", "confidence", "Confidence")

  # Response time plots
  plot_participant_response_times <- function(data, varname, subfolder, ylab) {
    print(paste("Creating response time plots for", varname, "..."))

    too_quick <- 300
    too_slow <- 10000
    h_line1 <- 300
    h_line2 <- 1000

    walk(unique(data$p_num), function(i) {
      data_i <- data %>% filter(p_num == i)

      too_slow_count <- sum(data_i[[varname]] > too_slow, na.rm = TRUE)
      too_quick_count <- sum(data_i[[varname]] < too_quick, na.rm = TRUE)

      p <- ggplot(data_i, aes(x = seq_len(nrow(data_i)), y = .data[[varname]])) +
        geom_point() +
        labs(x = "Trial", y = ylab) +
        theme_minimal() +
        ylim(0, 10000) +
        geom_hline(yintercept = too_quick, linetype = "dashed", color = "red") +
        geom_hline(yintercept = too_slow, linetype = "dashed", color = "red") +
        geom_hline(yintercept = h_line1, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = h_line2, linetype = "dashed", color = "blue") +
        annotate("text", x = 1, y = too_quick, label = "Too Quick", color = "red") +
        annotate("text", x = 1, y = too_slow, label = "Too Slow", color = "red") +
        annotate("text", x = 1, y = h_line1, label = "300ms", color = "blue") +
        annotate("text", x = 1, y = h_line2, label = "1000ms", color = "blue") +
        annotate("text", x = 1, y = 0, label = paste0("Too Quick: ", too_quick_count)) +
        annotate("text", x = 1, y = 9000, label = paste0("Too Slow: ", too_slow_count))

      suppressMessages(ggsave(
        here(output_folder, subfolder, paste0(varname, "_p_num_", i, ".png")),
        plot = p, device = "png"
      ))
    })
  }

  plot_participant_response_times(data, "response_time1", "response_time1", "Response Time 1")
  # only if version is not feedback
  if (version != "feedback") {
    plot_participant_response_times(data, "response_time2", "response_time2", "Response Time 2")
  }

  invisible(NULL)
}