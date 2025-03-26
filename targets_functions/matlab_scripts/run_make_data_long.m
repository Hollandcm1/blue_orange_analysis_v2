cd '/Users/christopherholland/Documents/A1_Current/PhD/Research Projects/Blue_Orange_Feedback/targets_analysis'

% Add the path to the directory containing the function
addpath('targets_functions');

%%%%%%%%%%%%%%%%
%%% Feedback %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/data_long_feedback.csv';
file = 'data/processed/pre_processed_data_feedback.mat';
version = "feedback";
disp('====================================================');
disp(strcat("Running func_make_data_long: ", version));
data_long = func_make_data_long(file,version);
disp("Saving...")
writetable(data_long, output_file);

%%%%%%%%%%%%%%%%
%%% Reliance %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/data_long_reliance.csv';
file = 'data/processed/pre_processed_data_reliance.mat';
version = "reliance";
disp('====================================================');
disp(strcat("Running func_make_data_long: ", version));
data_long = func_make_data_long(file, version);
disp("Saving...")
writetable(data_long, output_file);

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Feedback+Reliance %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% warning('SKIPPING RELIANCE+FEEDBACK')
output_file = 'data/processed/data_long_feedback+reliance.csv';
file = 'data/processed/pre_processed_data_feedback+reliance.mat';
version = "feedback+reliance";
disp('====================================================');
disp(strcat("Running func_make_data_long: ", version));
data_long = func_make_data_long(file, version);
disp("Saving...")
writetable(data_long, output_file);