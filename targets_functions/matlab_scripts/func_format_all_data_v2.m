function [pre_processed_data] = func_format_all_data_v2(file, version)

    % load("data/raw/All_Data_Feedback.mat")
    disp("loading predefined paramters...")
    switch version 
        case "feedback"
            load('data/predefined/feedback/sender_id_list_1.mat')
            load('data/predefined/feedback/sender_id_list_2.mat')
            load("data/predefined/feedback/sender_id_trust.mat");
            load("data/predefined/feedback/sender_id_confidence.mat");
            load("data/predefined/feedback/sender_id_color_selection_1.mat")
            load('data/predefined/feedback/sender_id_color_selection_2.mat')
            load("data/predefined/feedback/flat_list.mat")
        case "reliance"
            load('data/predefined/reliance/sender_id_list_1.mat')
            load('data/predefined/reliance/sender_id_list_2.mat')
            load("data/predefined/reliance/sender_id_trust.mat");
            load("data/predefined/reliance/sender_id_confidence.mat");
            load("data/predefined/reliance/sender_id_color_selection_1.mat")
            load('data/predefined/reliance/sender_id_color_selection_2.mat')
            load("data/predefined/reliance/flat_list.mat")
        case "feedback+reliance"
            load('data/predefined/feedback_and_reliance/sender_id_list_1.mat')
            load('data/predefined/feedback_and_reliance/sender_id_list_2.mat')
            load("data/predefined/feedback_and_reliance/sender_id_trust.mat");
            load("data/predefined/feedback_and_reliance/sender_id_confidence.mat");
            load("data/predefined/feedback_and_reliance/sender_id_color_selection_1.mat")
            load('data/predefined/feedback_and_reliance/sender_id_color_selection_2.mat')
            load("data/predefined/feedback_and_reliance/flat_list.mat")
    end

    % file = "data/raw/All_Data_Feedback.mat"
    disp("loading data...")
    load(file)

    for i = 1:size(All_Data,1)

        disp(strcat("Processing: ", num2str(i), " of ", num2str(size(All_Data,1))))

        % pull the subset
        subset = All_Data(i).subset;

        % convert vars as needed
        subset = convert_vars(subset);

        %%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Unique Identifier %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%
        index = find(subset.sender == "Survey Completion Code");
        All_Data(i).randomNum = subset.randomNum(index);
        % Remove condition from subset data so it can be clearer more easy later
        All_Data(i).subset.randomNum(index) = "NA";

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Informed Consent Agreement %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        index = find(subset.sender == "I Agree");
        All_Data(i).agree = subset.Agree_to_participate(index);

        %%%%%%%%%%%
        %%% Age %%%
        %%%%%%%%%%%
        index = find(subset.sender == "Demographics");
        All_Data(i).age = subset.age(index);

        %%%%%%%%%%%%%%
        %%% Gender %%%
        %%%%%%%%%%%%%%
        index = find(subset.sender == "Demographics");
        All_Data(i).gender = subset.gender(index);

        %%%%%%%%%%%%%%%%%
        %%% Condition %%%
        %%%%%%%%%%%%%%%%%
        all_vals = unique(subset.meta_location);
        unique_vals = all_vals(all_vals ~= 'NA');
        char_val = char(unique_vals(1));
        if version == "feedback+reliance"
            last_chars = char_val(end -9: end -1);
        else
            last_chars = char_val(end -8: end -1);
        end
        All_Data(i).condition = last_chars;

        %%%%%%%%%%%%%%%%%%%
        %%% Performance %%%
        %%%%%%%%%%%%%%%%%%%
        All_Data = pull_performance(All_Data, subset, i, sender_id_list_1, sender_id_list_2);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Trust and Confidence %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_trust_and_confidence(All_Data, subset, i, sender_id_trust, sender_id_confidence);

        %%%%%%%%%%%%%%%%%%%%%%
        %%% Percent Orange %%%
        %%%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_percent_orange(All_Data, subset, i, sender_id_color_selection_1, sender_id_color_selection_2);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Automation Recomendation %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_automation_recommendation(All_Data, subset, i, sender_id_color_selection_2);

        %%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Reliance Behaviour %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_reliance_behaviour(All_Data, subset, i);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Correct Color Selection %%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_correct_color(All_Data, subset, i, sender_id_list_1);

        %%%%%%%%%%%%%%%%%%%%%
        %%% Response Time %%%
        %%%%%%%%%%%%%%%%%%%%%
        All_Data = pull_response_time(All_Data, subset, i, sender_id_list_1, sender_id_list_2);

    end

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Condition Error Correction %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    All_Data = condition_error_correction(All_Data, flat_list);

    % Remove identifiers
    All_Data = rmfield(All_Data, 'randomNum');
    
    % output
    pre_processed_data = All_Data;

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [subset] = convert_vars(subset)

    subset.sender_id = string(subset.sender_id);
    subset.sender = string(subset.sender);
    subset.correct = string(subset.correct);
    subset.meta_location = string(subset.meta_location);
    subset.correctColor = string(subset.correctColor);

end

function [All_Data] = pull_performance(All_Data, subset, i, sender_id_list_1, sender_id_list_2)
    
    % pull performance
    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    selection1 = sub_subset(sub_subset.sender == 'Select Color', :); % first selection
    selection2 = sub_subset(sub_subset.sender == 'Automation Recommendation', :); % second selection
    % match to the expected list
    sender_id_list_1(:,2) = NaN;
    sender_id_list_2(:,2) = NaN;
    for j = 1:size(selection1)
        index = find(sender_id_list_1 == selection1.sender_id(j)); % find where it should go
        sender_id_list_1(index,2) = selection1.correct(j); % put the true or false in there
    end
    for j = 1:size(selection2)
        index = find(sender_id_list_2 == selection2.sender_id(j)); % find where it should go
        sender_id_list_2(index,2) = selection2.correct(j); % put the true or false in there
    end
    % add to data table
    All_Data(i).correct1 = sender_id_list_1(:,2);
    All_Data(i).correct2 = sender_id_list_2(:,2);

    % missing data flag
    if size(selection1,1) < 300 || size(selection2,1) < 300
        All_Data(i).missing_data_flag = true;
    end

    % performance calculation
    correct1_performance = sum(selection1.correct == 'TRUE') / size(selection1.correct,1);
    correct2_performance = sum(selection2.correct == 'TRUE') / size(selection2.correct,1);
    All_Data(i).correct1_performance = correct1_performance;
    All_Data(i).correct2_performance = correct2_performance;

    % low performance flag
    performance_val = 0.55;
    if correct1_performance < performance_val
        All_Data(i).p1_flag = true;
    end
    if correct2_performance < performance_val
        All_Data(i).p2_flag = true;
    end
end

function [All_Data] = pull_trust_and_confidence(All_Data, subset, i, sender_id_trust, sender_id_confidence)

    % pull values
    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    pre_trust = sub_subset(sub_subset.sender == "Trust Question (Pre)", :);
    pre_confidence = sub_subset(sub_subset.sender == "Confidence Question (Pre)", :);
    trust = sub_subset(sub_subset.sender == "Trust Question", :);
    confidence = sub_subset(sub_subset.sender == "Confidence Question", :);
    % match to the expected list
    sender_id_trust(:,2) = NaN;
    sender_id_confidence(:,2) = NaN;
    for j = 1:size(trust)
        index = find(sender_id_trust == trust.sender_id(j)); % find where it should go
        sender_id_trust(index,2) = trust.trust(j); % put the true or false in there
    end
    for j = 1:size(confidence)
        index = find(sender_id_confidence == confidence.sender_id(j)); % find where it should go
        sender_id_confidence(index,2) = confidence.confidence(j); % put the true or false in there
    end
    % correction for missing data
    if size(pre_trust,1) < 1 
        pre_trust = [];
        pre_trust.trust = {NaN};
    end
    if size(pre_confidence,1) < 1
        pre_confidence = [];
        pre_confidence.confidence = {NaN};
    end
    % add to data table
    All_Data(i).pre_trust = pre_trust.trust{:};
    All_Data(i).trust = sender_id_trust(:,2);
    All_Data(i).pre_confidence = pre_confidence.confidence{:};
    All_Data(i).confidence  = sender_id_confidence(:,2);

end

function [All_Data] = pull_percent_orange(All_Data, subset, i, sender_id_color_selection1, sender_id_color_selection2)

    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    color_selection1 = sub_subset(sub_subset.sender == "Select Color", :);
    color_selection2 = sub_subset(sub_subset.sender == "Automation Recommendation", :);
    sender_id_color_selection1(:,2) = NaN;
    sender_id_color_selection2(:,2) = NaN;
    for j = 1:size(color_selection1)
        index = find(sender_id_color_selection1 == color_selection1.sender_id(j)); % find where it should go
        sender_id_color_selection1(index,2) = color_selection1.response(j); % put the true or false in there
    end
    for j = 1:size(color_selection2)
        index = find(sender_id_color_selection2 == color_selection2.sender_id(j)); % find where it should go
        sender_id_color_selection2(index,2) = color_selection2.response(j); % put the true or false in there
    end
    % add to data table
    All_Data(i).color_selection1 = sender_id_color_selection1(:,2);
    All_Data(i).color_selection2 = sender_id_color_selection2(:,2);

    % percent calculation
    percent1 = sum(sender_id_color_selection1(:,2) == 'orange') / length(sender_id_color_selection1);
    percent2 = sum(sender_id_color_selection2(:,2) == 'orange') / length(sender_id_color_selection2);

    % add to data table
    All_Data(i).color_percentage1 = percent1;
    All_Data(i).color_percentage2 = percent2;

    % percentage flag
    if percent1 > 0.60 || percent1 < 0.40 || percent2 > 0.60 || percent2 < 0.40
        All_Data(i).color_selecction_flag = 1;
    end

end

function [All_Data] = pull_automation_recommendation(All_Data, subset, i, sender_id_color_selection2)

    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    auto_recommendation = sub_subset(sub_subset.sender == "Automation Recommendation", :);
    sender_id_color_selection2(:,2) = NaN;
    sender_id_correct_recommendation = sender_id_color_selection2;
    for j = 1:size(auto_recommendation)
        index = find(sender_id_color_selection2 == auto_recommendation.sender_id(j)); % find where it should go
        sender_id_color_selection2(index,2) = auto_recommendation.recommendation(j); % put the true or false in there
    end
    for j = 1:size(auto_recommendation)
        index = find(sender_id_correct_recommendation == auto_recommendation.sender_id(j)); % find where it should go
        if auto_recommendation.recommendation(j) == auto_recommendation.correctColor(j)
            correct = 1;
        else
            correct = 0;
        end
        sender_id_correct_recommendation(index,2) = correct; % put the true or false in there
    end

    % add to data table
    All_Data(i).auto_color_recommendation = sender_id_color_selection2(:,2);
    All_Data(i).correct_recommendation = sender_id_correct_recommendation(:,2);
    
    % percent calcualtion
    percent1 = sum(sender_id_color_selection2(:,2) == 'orange') / length(sender_id_color_selection2);
    percent2 = sum(sender_id_correct_recommendation(:,2) == '1') / length(sender_id_correct_recommendation);

    % add to data table
    All_Data(i).auto_orange_recommendation_percentage = percent1;
    All_Data(i).correct_recommendation_percent = percent2;

end

function [All_Data] = pull_reliance_behaviour(All_Data, subset, i)

    % user_reconmendation_final (correct1, correct_recommendation, correct2)
    correct1 = All_Data(i).correct1;
    correct_recommendation = All_Data(i).correct_recommendation;
    correct2 = All_Data(i).correct2;
    collector = string();
    for j = 1:size(correct1)

        total = "";

        if correct1(j) == "TRUE"
            total = total + "C";
        elseif correct1(j) == "FALSE"
            total = total + "I";
        end

        if correct_recommendation(j) == "1"
            total = total + "C";
        elseif correct_recommendation(j) == "0"
            total = total + "I";
        end

        if correct2(j) == "TRUE"
            total = total + "C";
        elseif correct2(j) == "FALSE"
            total = total + "I";
        end

        collector(j,1) = total;

    end

    % add to data table
    All_Data(i).reliance_behaviour = collector;

end

function [All_Data] = condition_error_correction(All_Data, flat_list)

    to_remove = [];
    counter = 1;
    for i = 1:size(All_Data, 1)

        if sum(ismember(flat_list, All_Data(counter).randomNum)) == 1 
            disp(strcat("Renaming: ", num2str(All_Data(counter).randomNum), " to 50%25Fl"))
            All_Data(counter).condition = '50%25Fl';
        elseif sum(ismember(flat_list, All_Data(counter).randomNum)) == 2
            % I think this is removing duplicate data sets, but honestly I'm not sure...
            All_Data(counter) = [];
            flat_list(counter) = [];
            counter = counter - 1;
        else
            % I think this actually means that everything is fine? 
            %warning('Some kind of issue')
            %disp(counter)
        end

        counter = counter+1;

    end

    

end

function [All_Data] = pull_correct_color(All_Data, subset, i,  sender_id_list_1)

    % pull performance
    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    selection1 = sub_subset(sub_subset.sender == 'Select Color', :); % first selection
    % match to the expected list
    sender_id_list_1(:,2) = NaN;
    for j = 1:size(selection1)
        index = find(sender_id_list_1 == selection1.sender_id(j)); % find where it should go
        sender_id_list_1(index,2) = selection1.correctColor(j); % put the true or false in there
    end
    % add to data table
    All_Data(i).correct_color = sender_id_list_1(:,2);

end

function [All_Data] = pull_response_time(All_Data, subset, i, sender_id_list_1, sender_id_list_2)

    % pull performance
    index = find(subset.sender_id == "13"); % where practice trials stop
    sub_subset = subset(index:end,:);
    selection1 = sub_subset(sub_subset.sender == 'Select Color', :); % first selection
    selection2 = sub_subset(sub_subset.sender == 'Automation Recommendation', :); % second selection
    % match to the expected list
    sender_id_list_1(:,2) = NaN;
    sender_id_list_2(:,2) = NaN;
    for j = 1:size(selection1)
        index = find(sender_id_list_1 == selection1.sender_id(j)); % find where it should go
        sender_id_list_1(index,2) = selection1.duration(j); % put the time in there
    end
    for j = 1:size(selection2)
        index = find(sender_id_list_2 == selection2.sender_id(j)); % find where it should go
        sender_id_list_2(index,2) = selection2.duration(j); % put the time in there
    end
    % add to data table
    All_Data(i).response_time1 = sender_id_list_1(:,2);
    All_Data(i).response_time2 = sender_id_list_2(:,2);

end