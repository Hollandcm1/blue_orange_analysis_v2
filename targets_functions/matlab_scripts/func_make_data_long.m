function [data_long] = func_make_data_long(file, version)

    disp("Loading data...")
    load(file);
    All_Data = pre_processed_data;

    % warning("Warnings are off temporarily!!")
    % warning('off', 'all');
    RTj6ALs_warning_sent = false;

    df_LME = table();
    counter_df_LME = 1;

    % loop through all participants
    for counter_All_Data = 1:size(All_Data, 1)

        disp(strcat("Processing participant: ", num2str(counter_All_Data), " of ", num2str(size(All_Data, 1))));

        % lopp through needed length
        for i = 1:size(All_Data(counter_All_Data).correct1)

            % participant_number
            participant_number = counter_All_Data;

            % agreement 
            agreement = string(All_Data(counter_All_Data).agree);
            % if more than one value, take the first one
            if length(agreement) > 1
                agreement = agreement(1);
            end

            % age 
            age = str2double(All_Data(counter_All_Data).age);
            % if more than one value, take the first one
            if length(age) > 1
                age = age(1);
            end

            % gender
            gender = string(All_Data(counter_All_Data).gender);
            % if more than one value, take the first one
            if length(gender) > 1
                gender = gender(1);
            end

            % condition
            condition = string(All_Data(counter_All_Data).condition);

            % trial v1 
            trial_v1 = mod(i-1, 50) + 1;

            % trial v2
            trial_v2 = i;

            % block
            block = ceil(i/50);

            % reliability
            [condition, reliability, RTj6ALs_warning_sent] = func_condition_block(condition, block, version, RTj6ALs_warning_sent);

            % correct1 - performance before automated aid
            correct1 = All_Data(counter_All_Data).correct1(i);
            if correct1 == "TRUE", correct1 = 1; else, correct1 = 0; end

            % correct2 - performance after automated aid
            correct2 = All_Data(counter_All_Data).correct2(i);
            switch version
                case "feedback"
                    correct2 = NaN; % i.e., not possible in this version
                case "reliance"
                    if correct2 == "TRUE", correct2 = 1; else, correct2 = 0; end
                case "feedback+reliance"
                    if correct2 == "TRUE", correct2 = 1; else, correct2 = 0; end
                otherwise
                    error('Version not implemented')
            end

            % trust
            trust = str2double(All_Data(counter_All_Data).trust(ceil(i/50)));

            % confidence
            confidence = str2double(All_Data(counter_All_Data).confidence(ceil(i/50)));

            % reliance
            % reliance = string(All_Data(counter_All_Data).reliance_behaviour(i));

            % color1
            color1 = All_Data(counter_All_Data).color_selection1(i);

            % color2
            color2 = All_Data(counter_All_Data).color_selection2(i);

            % auto_color
            auto_color = All_Data(counter_All_Data).auto_color_recommendation(i);

            % correct color
            correct_color = All_Data(counter_All_Data).correct_color(i);

            % correct_recommendation
            correct_recommendation = All_Data(counter_All_Data).correct_recommendation(i);

            % dependence
            % dependence = 
            %{
            reliance = All_Data(counter_All_Data).reliance_behaviour;
            reliance = reshape(reliance, [50 6]);
            for counter_block = 0:5
                dependence ...
                    ( sum(count(reliance(:,counter_block+1), "CII")) + sum(count(reliance(:,counter_block+1), "ICC"))) ...
                    / ( sum(count(reliance(:,counter_block+1), "CII")) + sum(count(reliance(:,counter_block+1), "ICC")) + ...
                    sum(count(reliance(:,counter_block+1), "CIC")) + sum(count(reliance(:,counter_block+1), "ICI")) ) ;
                if df_grace(counter_All_Data, column.reliance + counter_block) > 1
                    error('Greater than 1 ratio')
                end
            end
            %}

            % pre_trust
            pre_trust = str2double(All_Data(counter_All_Data).pre_trust);

            % pre_confidence
            pre_confidence = str2double(All_Data(counter_All_Data).pre_confidence);

            % response_time1
            response_time1 = All_Data(counter_All_Data).response_time1(i);

            % response_time2
            response_time2 = All_Data(counter_All_Data).response_time2(i);

            % store everything in table
            warning('off', 'all');
            df_LME.p_num(counter_df_LME) = participant_number;
            % df_LME.agreement(counter_df_LME) = agreement;
            df_LME.age(counter_df_LME) = age;
            df_LME.gender(counter_df_LME) = gender;
            df_LME.condition(counter_df_LME) = condition;
            df_LME.trial_v1(counter_df_LME) = trial_v1;
            df_LME.trial_v2(counter_df_LME) = trial_v2;
            df_LME.block(counter_df_LME) = block;
            df_LME.correct1(counter_df_LME) = correct1;
            df_LME.correct2(counter_df_LME) = correct2;
            df_LME.trust(counter_df_LME) = trust;
            % df_LME.reliance(counter_df_LME) = reliance;
            df_LME.pre_trust(counter_df_LME) = pre_trust;
            df_LME.pre_confidence(counter_df_LME) = pre_confidence;
            df_LME.confidence(counter_df_LME) = confidence;
            df_LME.reliability_level(counter_df_LME) = reliability;
            df_LME.color1(counter_df_LME) = color1;
            df_LME.color2(counter_df_LME) = color2;
            df_LME.auto_color(counter_df_LME) = auto_color;
            df_LME.correct_color(counter_df_LME) = correct_color;
            df_LME.correct_recommendation(counter_df_LME) = correct_recommendation;
            df_LME.response_time1(counter_df_LME) = response_time1;
            df_LME.response_time2(counter_df_LME) = response_time2;
            warning('on', 'all');

            % itterate through the output table
            counter_df_LME = counter_df_LME + 1;

        end

    end

    data_long = df_LME;

end

function [condition, reliability, RTj6ALs_warning_sent] = func_condition_block(condition, block, version, RTj6ALs_warning_sent)
    
    switch version
        case "feedback"
            switch condition
                case "50%25DeF"
                    reliability_levels = [100, 90, 80, 70, 60, 50];
                case "50%25InF"
                    reliability_levels = [50, 60, 70, 80, 90, 100];
                case "70%25DeF"
                    reliability_levels = [100, 90, 80, 70, 70, 70];
                case "70%25InF"
                    reliability_levels = [70, 80, 90, 100, 100, 100];
                case "RTj6ALs_"
                    if RTj6ALs_warning_sent == false
                        RTj6ALs_warning_sent = true;
                        warning("Participant RTj6ALs_ being ignored for block. Please remove this participant later.")
                    end
                    reliability = NaN;
                    return;
                otherwise
                    warning('Condition that cant be identified')
                    warning(condition)
                    reliability = NaN;
                    return;
            end
            reliability = reliability_levels(block);
        case "reliance"
            switch condition
                case "50%25DeR" 
                    reliability_levels = [100, 90, 80, 70, 60, 50];
                case "50%25InR"
                    reliability_levels = [50, 60, 70, 80, 90, 100];
                case "70%25DeR"
                    reliability_levels = [100, 90, 80, 70, 70, 70];
                case "70%25InR"
                    reliability_levels = [70, 80, 90, 100, 100, 100];
                otherwise
                    warning('Condition that cant be identified')
                    warning(condition)
                    reliability = NaN;
                    return;
            end
            reliability = reliability_levels(block);
        case "feedback+reliance"
            % error('Not implemented')
            switch condition
                case "50%25DeFe"
                    reliability_levels = [100, 90, 80, 70, 60, 50];
                case "50%25InFe"
                    reliability_levels = [50, 60, 70, 80, 90, 100];
                case "70%25DeFe"
                    reliability_levels = [100, 90, 80, 70, 70, 70];
                case "70%25InFe"
                    reliability_levels = [70, 80, 90, 100, 100, 100];
                otherwise
                    warning('Condition that cant be identified')
                    warning(condition)
                    reliability = NaN;
                    return;
            end
            reliability = reliability_levels(block);
    end
end