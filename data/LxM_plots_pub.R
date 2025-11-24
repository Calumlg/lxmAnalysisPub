# =============================================================================
# LxM Publication Plots - Standalone Script
# =============================================================================
# This script generates all publication-ready figures from the LxM manuscript

# HOW TO USE:
    # - install any of the required packages by uncommenting INSTALL REQUIRED PACKAGES code
    # - ensure base_dir in LxM_load_and_preprocess is set to the folder where the
    # repo was cloned or the data and code was downloaded

# Data figures from the manuscript can be reproduced after loading and pre-processing
# the data - search FIGURE to see find the code for each figure

# INSTALL REQUIRED PACKAGES (run once) ====
# Uncomment the following lines if you need to install packages:
# install.packages(c("dplyr", "ggplot2", "ggdist", "gghalves", "ggbeeswarm",
#                    "cowplot", "ggpubr", "RColorBrewer", "reshape2", 
#                    "colorspace", "ggsci"))

# LOAD REQUIRED PACKAGES ====

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(gghalves)
library(ggbeeswarm)
library(cowplot)
library(ggpubr)
library(RColorBrewer)
library(reshape2)
library(colorspace)
library(ggsci)

# LOAD DATA ====

setwd('/Users/calum/lxmAnalysisPub/data/')

source('LxM_load_and_preprocess.R')

data <- lxm_load_preprocess(clean = 1, tourn_remove = 1)

tbtDf <- data$tbtDf
summDf <- data$summDf

# FIGURE 2 - RATING TRAJECTORY PLOTS ====

data <- read.csv("allMeanTbtEst.csv")
rawEstData <- read.csv("allEstsByStim.csv")

# Convert raw matrix to tibble
df <- as_tibble(rawEstData)

# Rename columns for clarity
colnames(df) <- c("stimulus", paste0("t", 1:(ncol(df)-1)))

# Pivot to long format: one row per participant × trial
long_df <- df %>%
  pivot_longer(
    cols = starts_with("t"),
    names_to = "trial",
    names_prefix = "t",
    names_transform = list(trial = as.integer),
    values_to = "estimate"
  )

# Calculate mean and SE per label × timepoint
summary_df <- long_df %>%
  group_by(stimulus, trial) %>%
  summarize(
    mean_est = mean(estimate, na.rm = TRUE),
    se_est = sd(estimate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p <- ggplot(summary_df, aes(x = trial, y = mean_est, color = factor(stimulus), group = factor(stimulus))) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = mean_est - se_est, ymax = mean_est + se_est, fill = factor(stimulus)), alpha = 0.4, color = NA) +  # Keep fill as the same as the group color
  geom_hline(data = data.frame(
    stimulus = 1:8,
    h_line = ifelse(c(1:8) %in% c(1, 3, 5, 7), 0.25, 0.75)), aes(yintercept = h_line), linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(1, 16, by = 3)) +
  # scale_color_manual(values = manValues, labels = c("Control", "Clinical")) +
  # scale_fill_manual(values = manValues, labels = c("Control", "Clinical")) +  # Set the same fill color as line/scatter
  facet_wrap(~ stimulus, ncol = 2, labeller = labeller(stimulus = function(x) paste("Stimulus", x))) +
  ylim(0, 1) +
  labs(
    x = "Trial",
    y = "Average Rating",
    color = "Stimulus",
    fill = "Stimulus"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )
# View the plot with SE
print(p)

# FIGURE 2 - STIMULUS ESTIMATE BOXPLOTS ====

# Reorder variables: odd numbers (1,3,5,7) then even numbers (2,4,6,8)
variables_to_mean <- c("finLearningEst_1", "finLearningEst_3", "finLearningEst_5", "finLearningEst_7",
                       "finLearningEst_2", "finLearningEst_4", "finLearningEst_6", "finLearningEst_8")

# Convert to long format with reordered variables
df_long <- summDf %>%
  tidyr::pivot_longer(cols = all_of(variables_to_mean), 
                      names_to = "Variable", 
                      values_to = "Value") %>%
  mutate(
    Variable = factor(Variable, levels = variables_to_mean),
    Valence = case_when(
      Variable %in% c("finLearningEst_1", "finLearningEst_2", "finLearningEst_3", "finLearningEst_4") ~ "Reward",
      Variable %in% c("finLearningEst_5", "finLearningEst_6", "finLearningEst_7", "finLearningEst_8") ~ "Loss"
    )
  )

# Custom x-axis labels (modify these as needed)
custom_labels <- c("Alien 1", "Alien 3", "Alien 5", "Alien 7",
                   "Alien 2", "Alien 4", "Alien 6", "Alien 8")

# Bright red and green colors
condition_colors <- c("Loss" = "#FF1744", "Reward" = "#1AC71A")

# Create publication-ready violin plot with scatter points
p <- ggplot(df_long, aes(x = Variable, y = Value, fill = Valence, color = Valence)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.8, size = 1) +
  # geom_hline(yintercept = 0.25, color = "black", linewidth = 0.5) +
  # geom_hline(yintercept = 0.75, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = condition_colors) +
  scale_color_manual(values = condition_colors) +
  scale_x_discrete(labels = custom_labels) +
  labs(x = "Stimulus", y = "Final learning estimate") +
  theme_minimal(base_size = 18) +
  ylim(0,1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 14, hjust = 0.5),
    panel.grid.minor = element_blank(),
    # panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom"
  )
print(p)

# FIGURE 2 - POST-TOURNAMENT ERROR SCATTER ====

postTournErrScat <- ggplot(summDf, aes(x = prolificID, y = postTournErr)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, color = "#fed700", linetype = "solid", linewidth = 1.5) +
  labs(
    x = "Participants",
    y = "Error in post-tournament estimates") +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.ticks.x = element_blank(),
        axis.text.x.bottom = element_blank()) +
  ylim(-0.4,0.4)

print(postTournErrScat)

# SUPPLEMENTARY FIGURE 1 - MVC ====

mvcBar <- ggplot(summDf, aes(x = prolificID, y = mvc, fill = "#ff6201")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 19, color = "black", linetype = "solid", linewidth = 1.25) +
  scale_fill_manual(values = "#ff6201") +
  labs(
    x = "Participants",
    y = "Maximum key presses") +
  theme_minimal(base_size = 18) +
  # theme_minimal(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x = element_blank(),
        axis.text.x.bottom = element_blank()) +
  ylim(0, 100) # Set y-axis limits

# Print the plot
print(mvcBar)

# SUPPLEMENTARY FIGURE 2 - MEAN POST-TASK ESTIMATES ====

summ_long <- reshape2::melt(summDf, id.vars = "prolificID", measure.vars = c("postLearnMean_1","postTournMean_1","postEffortMean_1"))
meanPostEstsRain <- ggplot(summ_long, aes(x = variable, y = value, fill = as.factor(variable))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) +
  geom_boxplot(
    width = .25, 
    outlier.shape = NA,
    aes(fill = as.factor(variable))) +
  geom_point(
    inherit.aes = T,
    aes(color = as.factor(variable)),
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    ) 
  ) + 
  coord_cartesian(xlim = c(1.2, 3.2), clip = "off") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    y = "Mean post-task estimates (as proportions)") +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),) +
  scale_x_discrete(labels = c("postLearnMean_1" = "Post-learning", "postTournMean_1" = "Post-tournament","postEffortMean_1" = "Post-effort")) +
  scale_fill_manual(values = c("postLearnMean_1" = "#6dd3ce","postTournMean_1" = "#c8e9a0","postEffortMean_1" = "#f7a278")) + # Custom colors for fill
  scale_color_manual(values = c("postLearnMean_1" = "#6dd3ce","postTournMean_1" = "#c8e9a0","postEffortMean_1" = "#f7a278")) + # Custom colors for color
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Set the breaks (tick marks)
    labels = c("0", "0.25", "0.5", "0.75", "1")  # Set the labels
  )
# Print the plot
print(meanPostEstsRain)

# SUPPLEMENTARY FIGURE 3 - TOURNAMENT PEFORMANCE ====

summ_long <- reshape2::melt(summDf, id.vars = "prolificID", measure.vars = c("ovrperc","rewOvrPerc","lossOvrPerc","appPerc","avPerc"))

tournAccRain <- ggplot(summ_long, aes(x = variable, y = value, fill = as.factor(variable))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) +
  geom_point(
    inherit.aes = T,
    aes(color = as.factor(variable)),
    size = 1.5,
    alpha = .5,
    position = position_jitter(
      seed = 1, width = .1
    ) 
  ) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA,
    aes(fill = as.factor(variable))) +
  coord_cartesian(xlim = c(1.2, 5.4), clip = "off") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    y = "Tournament performance (as %)",
    x = "Accuracy") +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18)) +
  scale_fill_manual(values = c("ovrperc" = "#fb8fa4","rewOvrPerc" = "#7dbedf","lossOvrPerc" = "#f7cd4d","appPerc" = "#91c579","avPerc" = "#d2aecc")) + # Custom colors for fill
  scale_color_manual(values = c("ovrperc" = "#fb8fa4","rewOvrPerc" = "#7dbedf","lossOvrPerc" = "#f7cd4d","appPerc" = "#91c579","avPerc" = "#d2aecc")) + # Custom colors for color
  scale_x_discrete(labels = c("ovrperc" = "Overall", "rewOvrPerc" = "Reward-only", "lossOvrPerc" = "Loss-only", "appPerc" = "Approach", "avPerc" = "Avoid"))
# Print the plot
print(tournAccRain)

# SUPPLEMENTARY FIGURE 4 - CORRELATION MATRIX FOR LEARNING MEASURES ====

library(corrplot)

# Compute the mean for each variable in stim_vars, grouped by prolificID
rat_means <- tbtDf %>%
  group_by(prolificID) %>%
  summarise(
    plrMean = mean(postLearnRat, na.rm = TRUE),
    ptrMean = mean(postTournRat, na.rm = TRUE),
    perMean = mean(postEffortRat, na.rm = TRUE),
    fleMean = mean(as.numeric(finLearningEst), na.rm = TRUE),
    mebsMean = mean(meanEstByStim, na.rm = TRUE)
  )

rat_means <- as.data.frame(rat_means)

# Select only relevant columns
corr_data <- rat_means[, c("plrMean", "ptrMean", "perMean", "fleMean", "mebsMean")]

# Compute correlation matrix
cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")

custom_labels <- c("Post Learning Mean", "Post Tourn. Mean", "Post Effort Mean", "Final Learning Estimate", "Mean tbt estimate")

rownames(cor_matrix) <- custom_labels
colnames(cor_matrix) <- custom_labels

# Function to compute p-values
cor_pvalues <- function(mat) {
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  rownames(p_mat) <- colnames(mat)
  colnames(p_mat) <- colnames(mat)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      test <- cor.test(mat[, i], mat[, j], use = "pairwise.complete.obs", method = "pearson")
      p_mat[i, j] <- test$p.value
      p_mat[j, i] <- test$p.value
    }
  }
  
  diag(p_mat) <- 0  # Set diagonal to 0 (not needed but avoids confusion)
  return(p_mat)
}

# Compute p-values
p_matrix <- cor_pvalues(corr_data)

# Generate the correlation matrix heatmap with significance levels
svg(filename = "ilmCorrMatrix.svg", width = 9, height = 6)

p <- corrplot(cor_matrix, method = "color", type = "lower", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.cex = 0.8, tl.srt = 45,
         addCoef.col = "black")
dev.off()


# SUPPLEMENTARY FIGURE 5 - SHAPS, FAS AND TX ====

summ_long <- reshape2::melt(summDf, id.vars = "prolificID", measure.vars = c("SHAPS","FAS"))
shapsFasRain <- ggplot(summ_long, aes(x = variable, y = value, fill = as.factor(variable))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) +
  geom_point(
    inherit.aes = T,
    aes(color = as.factor(variable)),
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    ) 
  ) + 
  geom_boxplot(
    width = .22, 
    outlier.shape = NA,
    aes(fill = as.factor(variable))) +
  coord_cartesian(xlim = c(1.2, 2.4), clip = "off") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    x = "",
    y = "Questionnaire score") +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("SHAPS" = "SHAPS", "FAS" = "FAS")) +
  scale_fill_manual(values = c("SHAPS" = "#30c5d2","FAS" = "#E4C2C6")) + # Custom colors for fill
  scale_color_manual(values = c("SHAPS" = "#30c5d2","FAS" = "#E4C2C6")) # Custom colors for color
# Print the plot
print(shapsFasRain)

summ_long <- reshape2::melt(summDf, id.vars = "prolificID", measure.vars = c("AD","SW","Compul"))
transxRain <- ggplot(summ_long, aes(x = variable, y = value, fill = as.factor(variable))) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) +
  geom_point(
    inherit.aes = T,
    aes(color = as.factor(variable)),
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    ) 
  ) + 
  geom_boxplot(
    width = .22, 
    outlier.shape = NA,
    aes(fill = as.factor(variable))) +
  coord_cartesian(xlim = c(1.2, 3.4), clip = "off") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    x = "",
    y = "Factor loading") +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AD" = "AD", "SW" = "SW", "Compul" = "Compul")) +
  scale_fill_manual(values = c("AD" = "#84ffc9","SW" = "#aab2ff", "Compul" = "#eca0ff")) + # Custom colors for fill
  scale_color_manual(values = c("AD" = "#84ffc9","SW" = "#aab2ff", "Compul" = "#eca0ff")) # Custom colors for color
# Print the plot
print(transxRain)

