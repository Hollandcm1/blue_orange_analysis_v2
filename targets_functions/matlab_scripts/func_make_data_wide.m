function [data_wide] = func_make_data_wide(file)
    
    disp("Converting to wide format")

    % load data
    % load("data/processed/pre_processed_data.mat")
    load(file)

    All_Data = pre_processed_data;

    df_grace = [];

    column.participant_number = 1;
    column.condition = 2;
    %column.block = 3;
    column.reliance = 3; % through 8
    column.performance1 = 9; % through 14
    column.performance2 = 15; % through 20
    column.trust = 21; % through 26
    column.confidence = 27; % through 32
    column.reliance_old = 33; % through 38
    column.reliability = 39;

    %warning('Using hack way of dividing groups')
    %condition1_ps = floor(size(All_Data,1) / 2);

    adjust = 0;
    for counter_All_Data = 1:size(All_Data,1)

        disp(strcat("working on participant: ", num2str(counter_All_Data)))

        counter_All_Data = counter_All_Data - adjust; % correct for bad participant data 

        %%%% participant number %%%%
        df_grace(counter_All_Data, column.participant_number) = counter_All_Data;

        %%%% condition %%%%
        if All_Data(counter_All_Data).condition == '50%25DeF'
            condition = 1; % decreasing reliance
        elseif All_Data(counter_All_Data).condition == '50%25InF'
            condition = 2; % increasing reliance
        elseif All_Data(counter_All_Data).condition == '70%25DeF'
            condition = 3;
        elseif All_Data(counter_All_Data).condition == '70%25InF'
            condition = 4;
        else
            warning('Removing problematic condition')
            All_Data(counter_All_Data) = [];
            adjust = adjust + 1;
        end
        df_grace(counter_All_Data, column.condition) = condition;

        if condition == 1
            df_grace(counter_All_Data, column.reliability:column.reliability+5) = 100:-10:50;
        elseif condition == 2
            df_grace(counter_All_Data, column.reliability:column.reliability+5) = 50:10:100;
        elseif condition == 3
            df_grace(counter_All_Data, column.reliability:column.reliability+5) = [100 90 80 70 70 70];
        elseif condition == 4
            df_grace(counter_All_Data, column.reliability:column.reliability+5) = [70 80 90 100 100 100];
        end

        %{
        %%%% reliance %%%%
        reliance = All_Data(counter_All_Data).reliance_behaviour;
        reliance = reshape(reliance, [50 6]);
        for counter_block = 0:5
            df_grace(counter_All_Data, column.reliance + counter_block) = ...
                ( sum(count(reliance(:,counter_block+1), "CII")) + sum(count(reliance(:,counter_block+1), "ICC"))) ...
                / ( sum(count(reliance(:,counter_block+1), "CII")) + sum(count(reliance(:,counter_block+1), "ICC")) + ...
                sum(count(reliance(:,counter_block+1), "CIC")) + sum(count(reliance(:,counter_block+1), "ICI")) ) ;
            if df_grace(counter_All_Data, column.reliance + counter_block) > 1
                error('Greater than 1 ratio')
            end
        end

        reliance_old = All_Data(counter_All_Data).reliance_behaviour;
        reliance_old = reshape(reliance_old, [50 6]);
        for counter_block = 0:5
            df_grace(counter_All_Data, column.reliance_old + counter_block) = ...
                ( sum(count(reliance_old(:,counter_block+1), "CII")) + sum(count(reliance_old(:,counter_block+1), "ICC")));
        end
        %}

        %%%% performance %%%
        performance = All_Data(counter_All_Data).correct1;
        performance = reshape(performance, [50, 6]);
        for counter_block = 0:5 
            df_grace(counter_All_Data, column.performance1 + counter_block) = ...
                sum(count(performance(:,counter_block+1), "TRUE"));
        end

        %{
        performance = All_Data(counter_All_Data).correct2;
        performance = reshape(performance, [50, 6]);
        for counter_block = 0:5 
            df_grace(counter_All_Data, column.performance2 + counter_block) = ...
                sum(count(performance(:,counter_block+1), "TRUE"));
        end
        %}
        
        %%%% tust %%%%
        trust = All_Data(counter_All_Data).trust;
        trust = reshape(trust, [5, 6]);
        trust = str2double(trust);
        for counter_block = 0:5
            df_grace(counter_All_Data, column.trust + counter_block) = ...
                mean(trust(:, counter_block+1), "omitnan");
        end

        %%%% confidence %%%%
        confidence = All_Data(counter_All_Data).confidence;
        confidence = reshape(confidence, [5, 6]);
        confidence = str2double(confidence);
        for counter_block = 0:5
            df_grace(counter_All_Data, column.confidence + counter_block) = ...
                mean(confidence(:, counter_block+1), "omitnan");
        end

    end

    table_grace = array2table(df_grace);
    %table_grace.Properties.VariableNames = ["participant number", "condition", "r1", "r2", "r3", "r4", "r5" "r6", "p1_before","p2_before","p3_before","p4_before","p5_before","p6_before","p1_after","p2_after","p3_after","p4_after","p5_after","p6_after","t1","t2","t3","t4","t5","t6", "c1","c2","c3","c4","c5","c6","or1", "or2", "or3", "or4", "or5" "or6", 'ar1', 'ar2', 'ar3', 'ar4', 'ar5', 'ar6'];
    table_grace.Properties.VariableNames = ["participant number", "condition", "r1", "r2", "r3", "r4", "r5" "r6", "p1_before","p2_before","p3_before","p4_before","p5_before","p6_before","p1_after","p2_after","p3_after","p4_after","p5_after","p6_after","t1","t2","t3","t4","t5","t6", "c1","c2","c3","c4","c5","c6","or1", "or2", "or3", "or4", "or5" "or6", 'ar1', 'ar2', 'ar3', 'ar4', 'ar5', 'ar6'];
    %table_grace = stack(table_grace, 3:8);
    %table_grace.Properties.VariableNames = ["participant number", "condtion", "block", "reliance"];

    height_table = size(table_grace, 1);
    table_grace.r1 = NaN([height_table 1]);
    table_grace.r2 = NaN([height_table 1]);
    table_grace.r3 = NaN([height_table 1]);
    table_grace.r4 = NaN([height_table 1]);
    table_grace.r5 = NaN([height_table 1]);
    table_grace.r6 = NaN([height_table 1]);

    table_grace.p1_after = NaN([height_table 1]);
    table_grace.p2_after = NaN([height_table 1]);
    table_grace.p3_after = NaN([height_table 1]);
    table_grace.p4_after = NaN([height_table 1]);
    table_grace.p5_after = NaN([height_table 1]);
    table_grace.p6_after = NaN([height_table 1]);

    table_grace.or1 = NaN([height_table 1]);
    table_grace.or2 = NaN([height_table 1]);
    table_grace.or3 = NaN([height_table 1]);
    table_grace.or4 = NaN([height_table 1]);
    table_grace.or5 = NaN([height_table 1]);
    table_grace.or6 = NaN([height_table 1]);

    data_wide = table_grace;

    % writetable(df_LME, 'LME_Data.csv')
end