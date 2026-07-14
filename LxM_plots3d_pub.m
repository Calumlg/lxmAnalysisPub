%% LxM 3D bar plots pub
    % This code will generate Figure 3 in the main manuscript, 3D bar plots
    % of participants' probability of accepting varying as a function of
    % points available and valence-probability

    % By Calum Guinea, 2025

close all 

%% clean or full
for clean = 1:2

    if clean == 1
        dString = 'clean/';
    else
        dString = 'full/';
    end
    
    cd('/Users/calum/lxmAnalysisPub/data'); % add your own path
    data = readtable('choice_trialByTrial.csv');
    
%% organize the data
    effortChoice = data.accepted; % 1 for effort, 0 for no effort
    effortLevel = data.effPrp;
    pointsAvailable = data.outMag;
    probability = data.outProb;
    valence = data.valence;

%% Define unique conditions
    uniqueEffortLevels = unique(effortLevel);
    uniquePointsAvailable = unique(pointsAvailable);
    uniqueProbability = unique(probability);
    combinedConditions = [0.75 -1; 0.25 -1; 0.25 1; 0.75 1]; % [probability, valence]

%% Plot 1: Effort Level x Outcome Magnitude
        uniqueEffortLevels = unique(effortLevel);
        uniquePointsAvailable = unique(pointsAvailable);
        probabilityMatrix = zeros(length(uniqueEffortLevels), length(uniquePointsAvailable));
        
        for i = 1:length(uniqueEffortLevels)
            for j = 1:length(uniquePointsAvailable)
                mask = effortLevel == uniqueEffortLevels(i) & pointsAvailable == uniquePointsAvailable(j);
                totalTrials = sum(mask);
                if totalTrials > 0
                    probabilityMatrix(i, j) = sum(effortChoice(mask) == 1) / totalTrials;
                end
            end
        end
        probabilityMatrix
        
        figure;
        b = bar3(probabilityMatrix);
        xlabel('Points Available', 'FontSize', 16); ylabel('Effort Level','FontSize',16); zlabel('p(Accept)', 'FontSize', 18); 
        xticklabels(uniquePointsAvailable);
        yticklabels({'Low','Medium','High'});
        colormap([0.52, 1.0, 0.79; 0.67, 0.7, 1.0; 0.92, 0.63, 1.0; 1.0, 1.0, 0.0]);
        ax = gca;
        ax.XAxis.FontSize = 16; 
        ax.YAxis.FontSize = 16;
        ax.ZAxis.FontSize = 16;
        
        f = gcf;
        exportgraphics(f,['bar3d_pAcceptByMagAndEffort.png'],'Resolution',400)
        close all

%% Plot 2: Effort Level x Outcome Probability
        uniqueProbability = unique(probability);
        probabilityMatrix = zeros(length(uniqueEffortLevels), length(uniqueProbability));
        
        for i = 1:length(uniqueEffortLevels)
            for j = 1:length(uniqueProbability)
                mask = effortLevel == uniqueEffortLevels(i) & probability == uniqueProbability(j);
                totalTrials = sum(mask);
                if totalTrials > 0
                    probabilityMatrix(i, j) = sum(effortChoice(mask) == 1) / totalTrials;
                end
            end
        end
        
        figure;
        b = bar3(probabilityMatrix);
        xlabel('Outcome Probability','FontSize', 16); ylabel('Effort Level','FontSize',16); zlabel('p(Accept)', 'FontSize', 18); 
        xticklabels(uniqueProbability)
        yticklabels({'Low','Medium','High'});
        colormap([0.2, 0.6, 1.0; 1.0, 0.5, 0.0]);    
        ax = gca;
        ax.XAxis.FontSize = 16; 
        ax.YAxis.FontSize = 16;
        ax.ZAxis.FontSize = 16;

        f = gcf;
        exportgraphics(f,['bar3d_pAcceptByProbAndEffort.png'],'Resolution',400)
        close all

%% Plot 3: Effort Level x Combined Valence & Probability
        combinedConditions = [0.75 -1; 0.25 -1; 0.25 1; 0.75 1]; % [probability, valence]
        probabilityMatrix = zeros(length(uniqueEffortLevels), size(combinedConditions, 1));
        
        for i = 1:length(uniqueEffortLevels)
            for j = 1:size(combinedConditions, 1)
                mask = effortLevel == uniqueEffortLevels(i) & probability == combinedConditions(j, 1) & valence == combinedConditions(j, 2);
                totalTrials = sum(mask);
                if totalTrials > 0
                    probabilityMatrix(i, j) = sum(effortChoice(mask) == 1) / totalTrials;
                end
            end
        end
        probabilityMatrix
        
        figure;
        b = bar3(probabilityMatrix);
        xlabel('Valence-Prob.', 'FontSize', 16); ylabel('Effort Level','FontSize',16); zlabel('p(Accept)', 'FontSize', 18); 
        xticklabels({'Loss-High prob.', 'Loss-Low prob.', 'Rew-Low prob.', 'Rew-High prob.'});
        yticklabels({'Low','Medium','High'});
        colormap([
            0.9333, 0.3765, 0.3765; % Light Red (#EE6060)
            0.6980, 0.1333, 0.1333; % Dark Red (#B22222)
            0.3961, 0.6902, 0.3961; % Light Green (#65B064)
            0.1020, 0.3255, 0.0980  % Dark Green (#1A5319)
        ]);        
        ax = gca;
        ax.XAxis.FontSize = 16; 
        ax.YAxis.FontSize = 16;
        ax.ZAxis.FontSize = 16;
        ax.XTickLabelRotation = 340;

        f = gcf;
        exportgraphics(f,['bar3d_pAcceptByProbValAndEffort.png'],'Resolution',400)
        close all

end