
% load sender_id_list_1.mat

% This was used just to check the sender_ids for reliance could also
% be used for feedback+reliance

% check if All_Data exists
if ~exist('All_Data', 'var')
    data_file_path = "data/raw/All_Data_Feedback+Reliance.mat";
    load(data_file_path);
end

file_path = "data/predefined/reliance";
load(fullfile(file_path, "sender_id_color_selection_1.mat"));
load(fullfile(file_path, "sender_id_color_selection_2.mat"));
load(fullfile(file_path, "sender_id_list_1.mat"));
load(fullfile(file_path, "sender_id_list_2.mat"));
load(fullfile(file_path, "sender_id_order.mat"));
load(fullfile(file_path, "sender_id_trust.mat"));
load(fullfile(file_path, "sender_id_confidence.mat"));

% note that feedback and reliance have different versions of this... not sure why
file_path = "data/predefined/reliance";
f_r = load(fullfile(file_path, "flat_list.mat")); 
file_path = "data/predefined/feedback";
f_f = load(fullfile(file_path, "flat_list.mat"));