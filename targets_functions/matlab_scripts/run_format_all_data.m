% run_format_all_data.m

cd '/Users/christopherholland/Documents/A1_Current/PhD/Research Projects/Blue_Orange_Feedback/blue_orange_analysis_v2/'

% Add the path to the directory containing the function
addpath('targets_functions');
addpath('targets_functions/matlab_scripts');

%%%%%%%%%%%%%%%%
%%% Feedback %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/pre_processed_data_feedback.mat';
file = 'data/raw/All_Data_Feedback.mat';
version = "feedback";
disp('====================================================');
disp(strcat("Running func_format_all_data_v2: ", version));
pre_processed_data = func_format_all_data_v2(file, version);
disp("Saving...")
save(output_file, 'pre_processed_data');

%%%%%%%%%%%%%%%%
%%% Reliance %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/pre_processed_data_reliance.mat';
file = 'data/raw/All_Data_Reliance.mat';
version = "reliance";
disp('====================================================');
disp(strcat("Running func_format_all_data_v2: ", version));
pre_processed_data = func_format_all_data_v2(file, version);
disp("Saving...")
save(output_file, 'pre_processed_data');

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Feedback+Reliance %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% warning('SKIPPING RELIANCE+FEEDBACK');
output_file = 'data/processed/pre_processed_data_feedback+reliance.mat';
file = 'data/raw/All_Data_Feedback+Reliance.mat';
version = "feedback+reliance";
disp('====================================================');
disp(strcat("Running func_format_all_data_v2: ", version));
pre_processed_data = func_format_all_data_v2(file, version);
disp("Saving...")
save(output_file, 'pre_processed_data');


