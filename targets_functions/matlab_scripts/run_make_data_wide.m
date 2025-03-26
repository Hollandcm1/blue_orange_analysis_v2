% run_make_data_wide.m

cd '/Users/christopherholland/Documents/A1_Current/PhD/Research Projects/Blue_Orange_Feedback/targets_analysis'

% Add the path to the directory containing the function
addpath('targets_functions');

%%%%%%%%%%%%%%%%
%%% Feedback %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/data_wide_feedback.csv';
file = 'data/processed/pre_processed_data_feedback.mat';
data_wide = func_make_data_wide(file);
writetable(data_wide, output_file);

%%%%%%%%%%%%%%%%
%%% Reliance %%%
%%%%%%%%%%%%%%%%
output_file = 'data/processed/data_wide_reliance.csv';
file = 'data/processed/pre_processed_data_reliance.mat';
data_wide = func_make_data_wide(file);
writetable(data_wide, output_file);

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Feedback+Reliance %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
output_file = 'data/processed/data_wide_feedback+reliance.csv';
file = 'data/processed/pre_processed_data_feedback+reliance.mat';
data_wide = func_make_data_wide(file);
writetable(data_wide, output_file);


