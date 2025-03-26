
% Feedback
load("data/predefined/feedback/sender_id_color_selection1.mat")
load("data/predefined/feedback/sender_id_color_selection2.mat")
sender_id_color_selection_1 = sender_id_color_selection1;
sender_id_color_selection_2 = sender_id_color_selection2;
save("data/predefined/feedback/sender_id_color_selection_1.mat", "sender_id_color_selection_1")
save("data/predefined/feedback/sender_id_color_selection_2.mat", "sender_id_color_selection_2")

% Reliance
load("data/predefined/reliance/sender_id_color_selection1.mat")
load("data/predefined/reliance/sender_id_color_selection2.mat")
sender_id_color_selection_1 = sender_id_color_selection1;
sender_id_color_selection_2 = sender_id_color_selection2;
save("data/predefined/reliance/sender_id_color_selection_1.mat", "sender_id_color_selection_1")
save("data/predefined/reliance/sender_id_color_selection_2.mat", "sender_id_color_selection_2")

% check that it worked
% load("data/predefined/feedback/sender_id_color_selection_1.mat")