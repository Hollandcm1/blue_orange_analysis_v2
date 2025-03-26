# Copy the All_Data.mat file from the MATLAB data folder 

copy_matlab_data <- function() {
  # Fedback+Reliance data
  file.copy(
    from = paste0(
      "/Users/christopherholland/Documents/",
      "MATLAB/Blue_Orange_Feedback/All_Data.mat"
    ),
    to = "data/raw/All_Data_Feedback+Reliance.mat",
    overwrite = TRUE
  )

  # Feedback data
  file.copy(
    from = paste0(
      "/Users/christopherholland/Documents/",
      "MATLAB/Blue_Orange_Feed/All_Data.mat"
    ),
    to = "data/raw/All_Data_Feedback.mat",
    overwrite = TRUE
  )

  # Reliance data
  file.copy(
    from = paste0(
      "/Users/christopherholland/Documents/",
      "MATLAB/Blue_Orange_Reliance/All_Data.mat"
    ),
    to = "data/raw/All_Data_Reliance.mat",
    overwrite = TRUE
  )

  # Hard copies of the below info exists in this project,
  # do NOT update from matlab

  # # Flat List
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/flat_list.mat"
  #   ),
  #   to = "data/predefined/feedback/flat_List.mat",
  #   overwrite = TRUE
  # )
  #
  # # Feedback Sender IDs
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_list_1.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_list_1.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_list_2.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_list_2.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_trust.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_trust.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_confidence.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_confidence.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_order.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_order.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_color_selection1.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_color_selection_1.mat",
  #   overwrite = TRUE
  # )
  # file.copy(
  #   from = paste0(
  #     "/Users/christopherholland/Documents/",
  #     "MATLAB/Blue_Orange_Feed/sender_id_color_selection2.mat"
  #   ),
  #   to = "data/predefined/feedback/sender_id_color_selection_2.mat",
  #   overwrite = TRUE
  # )

  return(NULL)

}
