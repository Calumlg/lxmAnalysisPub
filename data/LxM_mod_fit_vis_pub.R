# LxM MIXED MODEL FIT AND VIS ====

# script for fitting GLMMs to trial-by-trial choice data from 'Effort choices are sensitive to prior
# learning'

# Please excuse the long list of packages for now 

# PACKAGES ====

# fitting
library(nlme)
library(glmmTMB)
library(lme4)
library(blme)
# plotting
library(ggplot2)
library(ggeffects)
library(visreg)
library(sjPlot)
library(qqplotr)
library(ggpubr)
library(RColorBrewer)
library(svglite)
library(ggExtra)
library(emmeans)
# manipulation
library(dplyr)
library(broom.mixed)
library(purrr)
# simulation
library(simr) # pkg for GLMM based power calc
library(RLRsim)
# model eval
library(lmerTest)
library(performance)
library(car)
library(pwr)
library(effsize)
# idk
library(ResourceSelection)
# misc
library(knitr)
library(tidyr)
library(car)      # For Levene's test
library(rstatix)  # For statistical tests
library(afex)

# LOAD DATA ====

setwd('/Users/calum/lxmAnalysisPub/data/')

source('LxM_load_and_preprocess.R')

data <- lxm_load_preprocess(clean = 1, tourn_remove = 1)

tbtDf <- data$tbtDf
summDf <- data$summDf

# MODEL 0 ====
## NOTE use this to identify whether demo variables need to be included in GLMMs

baseGlm <- glm(accepted ~ Age_resc+Gender+SES+ord+outProb+normOutMag+effPrp+valence,
               data = tbtDf,
               family = "binomial")
summary(baseGlm)

nullGlm <- glm(accepted ~ 1,
               data = tbtDf,
               family = "binomial")
summary(nullGlm)

anova(baseGlm, nullGlm, test = "Chi")


# MODEL FORMULAS ====
# here we have different sets of models to fit - point to the deisred formula in func call
# one list for fitting models w/o mental health predictors

## INDIVIDUALISED LEARNING MEASURES ====

## remember no effect of age or gender when we move to GLMMs to removed from formulas below
model_formulas1 <- list(
  # first fit a model that assumes perfect and invariant learning across ps 
  f1 = accepted ~ Age_resc+Gender+outProb+normOutMag+effPrp+valence+(1+outProb+normOutMag+effPrp+valence|prolificID), # our base model
  # then fit some models which add or substitute individualised measures into the model
  f2 = accepted ~ Age_resc+Gender+postLearnRat+normOutMag+effPrp+valence+(1+postLearnRat+normOutMag+effPrp+valence|prolificID),
  f3 = accepted ~ Age_resc+Gender+postTournRat+normOutMag+effPrp+valence+(1+postTournRat+normOutMag+effPrp+valence|prolificID),
  f4 = accepted ~ Age_resc+Gender+meanEstByStim+normOutMag+effPrp+valence+(1+meanEstByStim+normOutMag+effPrp+valence|prolificID),
  f5 = accepted ~ Age_resc+Gender+finLearningEst+normOutMag+effPrp+valence+(1+finLearningEst+normOutMag+effPrp+valence|prolificID)
)

## CONTINUOUS MENTAL HEALTH MEASURES ====

model_formulas2 <- list(
  f1 = accepted ~ Age_resc+Gender+postTournRat+normOutMag+effPrp+valence+(1+postTournRat+normOutMag+effPrp+valence|prolificID),
  f2 = accepted ~ Age_resc+Gender+postTournRat+normOutMag+effPrp+valence+SHAPS_resc+(1+postTournRat+normOutMag+effPrp+valence|prolificID),
  f4 = accepted ~ Age_resc+Gender+postTournRat+normOutMag+effPrp+valence+AD+Compul+SW+(1+postTournRat+normOutMag+effPrp+valence|prolificID),
  f5 = accepted ~ Age_resc+Gender+postTournRat+normOutMag+effPrp+valence+FAS_resc+(1+postTournRat+normOutMag+effPrp+valence|prolificID)
)

# FITTING GLMMS ====

lxm_glmm_fit <- function(tbtDf, model_formulas) {
  # fit the models, summarise and add to list for model comp
  models <- list()
## HERE IS WHERE YOU DEFINE MODEL FORUMULAS AS 1, 2 OR 3 BEFORE CALLING FUNC ====
  model_formulas <- model_formulas
  print(model_formulas)
  # Create an empty list to store model objects
  model_list <- list()
  # save the model comp
  AIC_scores <- numeric(length(model_formulas))
  BIC_scores <- numeric(length(model_formulas))
  
  # Iterate through the list of model formulas and fit the models
  for (i in 1:length(model_formulas)) {
    print(paste("fitting model", i))
    model <- glmer(model_formulas[[i]], data = tbtDf, family=binomial(link="logit"), 
                    control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
    model_list[[i]] <- model
    AIC_scores[i] <- AIC(model)
    BIC_scores[i] <- BIC(model)
  }
  
  # bar plot AIC and BIC
  aicData <- data.frame(model_number = seq_along(AIC_scores), AIC_score = AIC_scores)
  aicPlot <- ggplot(aicData, aes(x = factor(model_number), y = AIC_score)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Model Number", y = "AIC Score", title = "AIC Scores for Models") +
    theme_minimal()
  print(aicPlot)
  
  bicData <- data.frame(model_number = seq_along(BIC_scores), BIC_score = BIC_scores)
  bicPlot <- ggplot(bicData, aes(x = factor(model_number), y = BIC_score)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(x = "Model Number", y = "BIC Score") +
    theme_minimal()
  print(bicPlot)
  
  # SET YOUR WINNING MODEL
  lowestAIC <- which.min(AIC_scores)
  m1 = model_list[[lowestAIC]]
  print(summary(m1))
  print(AIC_scores)
  print(BIC_scores)

  results <- list(modelsFitted = model_list, aic = AIC_scores, bic = BIC_scores)
  return(results)
  
}

# CALL THE FUNCTION ====

ilmFits <- lxm_glmm_fit(tbtDf = tbtDf, model_formulas = model_formulas1)

mhfits <- lxm_glmm_fit(tbtDf, model_formulas = model_formulas2)

# SET WINNING MODEL ====

winMod <- ilmFits$modelsFitted[[which.min(sapply(fits$modelsFitted, AIC))]]
summary(winMod)

mhWinMod <- mhfits$modelsFitted[[which.min(sapply(fits$modelsFitted, AIC))]]
summary(mhWinMod)

# VISUALISE ====

## WINNING MODEL NO MH ====

# update these according to m1 or winMod
fixed_effects <- fixef(winMod)
random_effects <- ranef(winMod)$prolificID
# assign rfx 
outProbEstRfx <- setNames(as.data.frame(random_effects$postTournRat), "outProbRfx")
effLvlRfx <- setNames(as.data.frame(random_effects$effPrp), "effRfx")
outMagRfx <- setNames(as.data.frame(random_effects$normOutMag), "outMagRfx")
valRfx <- setNames(as.data.frame(random_effects$valence), "valRfx")

## FIGURE 4 - BAR FIXED EFFECTS ====

# Get fixed effects
fixed_effects <- tidy(winMod, effects = "fixed", conf.int = TRUE, std.err = TRUE)
print(fixed_effects)
# Convert term to factor and explicitly set the order to match the original data
fixed_effects$term <- factor(fixed_effects$term, levels = fixed_effects$term)
# Create a vibrant color palette with unique colors for each bar
vibrant_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "gold", "#A65628", "#F781BF", "#1B9E77")
# Create the bar plot with ordered factors and color updates
winMod_fixEffBar <- ggplot(fixed_effects, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, linewidth = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = vibrant_colors) +
  scale_x_discrete(labels = c("(Intercept)","Age","Gender Non-binary","Gender Not specified","Gender Woman","Post-tournament rating","Outcome magnitude","Effort level","Valence (effect of reward)")) +
  scale_y_continuous(seq(-15,15,by = 2)) +
  labs(
    x = "Fixed Effects",
    y = "Beta estimate") +
  lims(
    y = c(-15,15)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    legend.position = "none"  # Remove the legend
  )
print(winMod_fixEffBar)
ggsave(filename = "winMod_fixEffBar.svg", plot = winMod_fixEffBar, width = 9, height = 6, dpi = 500, path = plotPath)

### HISTOGRAMS OF RFX ====
# NOTE aes in these plots need to be manually changed to reflect m1 or winMod

## POST TOURNAMENT RATING
bin_count <- 20
x_min <- min(outProbEstRfx)
x_max <- max(outProbEstRfx)
bin_width <- (x_max - x_min) / bin_count

hist_ptr <- ggplot(outProbEstRfx, aes(x = outProbRfx)) +
  geom_histogram(binwidth = bin_width, fill = "gold", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.2, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_outProb)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Post-tournament rating slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_ptr)
ggsave(filename = "winMod_hist_ptr.png", plot = hist_ptr, width = 4, height = 6, dpi = 500, path = plotPath)

## VALENCE
bin_count <- 20
x_min <- min(valRfx)
x_max <- max(valRfx)
bin_width <- (x_max - x_min) / bin_count

hist_val <- ggplot(valRfx, aes(x = random_effects$valence)) +
  geom_histogram(binwidth = bin_width, fill = "#A65628", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.1, x_max*1.1)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_estErr)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Valence slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16) 
print(hist_val)
ggsave(filename = "winMod_hist_val.png", plot = hist_val, width = 4, height = 6, dpi = 500, path = plotPath)

## EFFORT
bin_count <- 20
x_min <- min(effLvlRfx)
x_max <- max(effLvlRfx)
bin_width <- (x_max - x_min) / bin_count

hist_effLvl <- ggplot(effLvlRfx, aes(x = random_effects$effPrp)) +
  geom_histogram(binwidth = bin_width, fill = "#F781BF", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.3, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_effLvl)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Effort level slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_effLvl)
ggsave(filename = "winMod_hist_effLvl.png", plot = hist_effLvl, width = 4, height = 6, dpi = 500, path = plotPath)

## OUTCOME MAGNITUDE 
bin_count <- 20
x_min <- min(outMagRfx)
x_max <- max(outMagRfx)
bin_width <- (x_max - x_min) / bin_count

hist_outMag <- ggplot(outMagRfx, aes(x = random_effects$normOutMag)) +
  geom_histogram(binwidth = bin_width, fill = "#1B9E77", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.3, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(outProbHist)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Outc. magn. slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_outMag)
ggsave(filename = "winMod_hist_outMag.png", plot = hist_outMag, width = 4, height = 6, dpi = 500, path = plotPath)

## MH MOD ====

### EXTRACT FIXED AND RANDOM EFFECTS ====

# update these according to m1 or winMod
fixed_effects <- fixef(mhWinMod)
random_effects <- ranef(mhWinMod)$prolificID
outProbEstRfx <- setNames(as.data.frame(random_effects$postTournRat), "outProbRfx")
effLvlRfx <- setNames(as.data.frame(random_effects$effPrp), "effRfx")
outMagRfx <- setNames(as.data.frame(random_effects$normOutMag), "outMagRfx")
valRfx <- setNames(as.data.frame(random_effects$valence), "valRfx")

### FIGURE 5 - BAR FIXED EFFECTS ====

# Get fixed effects
fixed_effects <- tidy(mhWinMod, effects = "fixed", conf.int = TRUE, std.err = TRUE)
print(fixed_effects)
# Convert term to factor and explicitly set the order to match the original data
fixed_effects$term <- factor(fixed_effects$term, levels = fixed_effects$term)
# Create a vibrant color palette with unique colors for each bar
vibrant_colors <- c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA", "#F46D43", 
                     "#74ADD1", "#FEE08B", "#66C2A5", "#A6D854", "#E78AC3")
# Create the bar plot with ordered factors and color updates
mhWinMod_fixEffBar <- ggplot(fixed_effects, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, linewidth = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = vibrant_colors) +
  scale_x_discrete(labels = c("(Intercept)","Age","Gender Non-binary","Gender Not specified","Gender Woman",
                              "Post-tournament rating","Outcome magnitude","Effort level",
                              "Valence (effect of reward)","SHAPS Sum score")) +
  scale_y_continuous(seq(-15,15,by = 2)) +
  labs(
    x = "Fixed Effects",
    y = "Beta estimate") +
  lims(
    y = c(-15,15)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    legend.position = "none"  # Remove the legend
  )
print(mhWinMod_fixEffBar)
ggsave(filename = "mhWinMod_fixEffBar.svg", plot = mhWinMod_fixEffBar, width = 9, height = 6, dpi = 500, path = plotPath)


### HISTOGRAMS OF RFX ====

## POST TOURNAMENT RATING 
bin_count <- 20
x_min <- min(outProbEstRfx)
x_max <- max(outProbEstRfx)
bin_width <- (x_max - x_min) / bin_count
# plot
hist_ptr <- ggplot(outProbEstRfx, aes(x = outProbRfx)) +
  geom_histogram(binwidth = bin_width, fill = "#74ADD1", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.2, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_outProb)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Post-tournament rating slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_ptr)
ggsave(filename = "mhWinMod_hist_ptr.png", plot = hist_ptr, width = 4, height = 6, dpi = 500, path = plotPath)

## VALENCE
bin_count <- 20
x_min <- min(valRfx)
x_max <- max(valRfx)
bin_width <- (x_max - x_min) / bin_count
# plot
hist_val <- ggplot(valRfx, aes(x = random_effects$valence)) +
  geom_histogram(binwidth = bin_width, fill = "#A6D854", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.1, x_max*1.1)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_estErr)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Valence slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16) 
print(hist_val)
ggsave(filename = "mhWinMod_hist_val.png", plot = hist_val, width = 4, height = 6, dpi = 500, path = plotPath)

## EFFORT
bin_count <- 20
x_min <- min(effLvlRfx)
x_max <- max(effLvlRfx)
bin_width <- (x_max - x_min) / bin_count
# plot
hist_effLvl <- ggplot(effLvlRfx, aes(x = random_effects$effPrp)) +
  geom_histogram(binwidth = bin_width, fill = "#66C2A5", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.3, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(hist_effLvl)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Effort level slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_effLvl)
ggsave(filename = "mhWinMod_hist_effLvl.png", plot = hist_effLvl, width = 4, height = 6, dpi = 500, path = plotPath)

## OUTCOME MAGNITUDE 
bin_count <- 20
x_min <- min(outMagRfx)
x_max <- max(outMagRfx)
bin_width <- (x_max - x_min) / bin_count
# plot
hist_outMag <- ggplot(outMagRfx, aes(x = random_effects$normOutMag)) + 
  geom_histogram(binwidth = bin_width, fill = "#FEE08B", color = "black",linewidth = 0.9) +
  scale_x_continuous(limits = c(x_min*1.3, x_max*1.3)) +
  # scale_y_continuous(limits = c(0,max(ggplot_build(outProbHist)$data[[1]]$count) * 1.1)) +
  labs(
    x = "Outc. magn. slopes",
    y = "Frequency") +
  theme_minimal(base_size = 16)
print(hist_outMag)
ggsave(filename = "mhWinMod_hist_outMag.png", plot = hist_outMag, width = 4, height = 6, dpi = 500, path = plotPath)

# SHAPS INTERACTION MODELS ====

intrMod_ptr_SHAPS <- glmer(accepted ~ Age_resc + Gender + normOutMag + effPrp + postTournRat * SHAPS_resc 
                           + (1 + postTournRat + normOutMag + valence + effPrp | prolificID), 
                           data = tbtDf, 
                           family=binomial(link="logit"), 
                           control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
summary(intrMod_ptr_SHAPS)

intrMod_val_SHAPS <- glmer(accepted ~ Age_resc + Gender + normOutMag + effPrp + postTournRat + valence * SHAPS_resc 
                           + (1 + postTournRat + normOutMag + valence + effPrp | prolificID), 
                           data = tbtDf, 
                           family=binomial(link="logit"), 
                           control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
summary(intrMod_val_SHAPS)

intrMod_magn_SHAPS <- glmer(accepted ~ Age_resc + Gender + normOutMag + postTournRat + effPrp * SHAPS_resc 
                            + (1 + postTournRat + normOutMag + valence + effPrp | prolificID), 
                            data = tbtDf, 
                            family=binomial(link="logit"), 
                            control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
summary(intrMod_eff_SHAPS)

intrMod_eff_SHAPS <- glmer(accepted ~ Age_resc + Gender + effPrp + postTournRat + normOutMag  * SHAPS_resc 
                           + (1 + postTournRat + normOutMag + valence + effPrp | prolificID), 
                           data = tbtDf, 
                           family=binomial(link="logit"), 
                           control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
summary(intrMod_mag_SHAPS)

## FIGURE 6 - SHAPS INTERACTION PLOTS ====

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(effects)
library(gridExtra)

# Method 2: Manual prediction approach
# Create a grid of values for prediction
newdata <- expand.grid(
  postTournRat = seq(0, 1, by = 0.1),
  SHAPS_resc = c(-1.5, 0, 1.5),
  Age_resc = 0,                                  # Set to mean (since rescaled)
  Gender = "Woman (including Trans Female/Trans Woman)",  # Most common category or whatever is appropriate
  effPrp = mean(tbtDf$effPrp, na.rm = TRUE),
  normOutMag = mean(tbtDf$normOutMag, na.rm = TRUE),
  valence = levels(tbtDf$valence)[1]             # Use first level of valence
)

# Predict probabilities
newdata$predicted <- predict(mhMod_wIntr1, newdata = newdata, type = "response", re.form = NA)

# Convert SHAPS_resc to factor for better labeling
newdata$SHAPS_resc_factor <- factor(newdata$SHAPS_resc, 
                                    levels = c(-1.5, 0, 1.5),
                                    labels = c("Low", "Mean", "High"))

# Create the plot
plot2 <- ggplot(newdata, aes(x = postTournRat, y = predicted, color = SHAPS_resc_factor)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#ef476f", "#ffd166", "#26547c"),
                     name = "SHAPS Score") +
  labs(x = "Post-tournament Rating",
       y = "Probability of Acceptance") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.title = element_text(face = "bold"))

print(plot2)
ggsave(filename = "intrMod_SHAPS_ptr_plot_claude.svg", plot = plot2, width = 10, height = 10, dpi = 500, path = plotPath)

# Additional visualization: Showing the difference in slopes more explicitly
# Create slope comparison plot
slopes <- data.frame(
  SHAPS_level = c("Low", "Mean", "High"),
  Slope = c(6.02397 - 0.71045, 6.02397, 6.02397 + 0.71045)
)

plot3 <- ggplot(slopes, aes(x = SHAPS_level, y = Slope, fill = SHAPS_level)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("#26547c", "#ef476f", "#ffd166")) +  
  labs(x = "SHAPS Score",
       y = "Effect of Post-tourn. rating on Accepting Effort") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = round(Slope, 2)), vjust = -0.5, size = 4)
print(plot3)
ggsave(filename = "intrMod_SHAPS_ptr_bar_claude.svg", plot = plot3, width = 10, height = 6, dpi = 500, path = plotPath)

comb_intr_plot <- gridExtra::grid.arrange(plot2, plot3, ncol = 2)
ggsave(filename = "intrMod_SHAPS_ptr_combPlot.svg", plot = comb_intr_plot, width = 12, height = 8, dpi = 500, path = plotPath)

## FIGURE S6 - SHAPS EFFECT ON LOG-ODDS ACCEPT ====

mhMod_SHAPS__ptr_plot <- ggpredict(mhWinMod, terms = c("effPrp [all]","SHAPS_resc [-1.5, 0, 3]"), type = "fe") %>%
  plot() +
  scale_fill_manual(values = c("#F9AED3","#8AC926","#4A90E0")) +
  scale_colour_manual(values = c("#F9AED3","#8AC926","#4A90E0")) +
  labs(
    title = "",
    x = "Effort level",
    y = "p(accept)",
    colour = "SHAPS"
  ) +
  theme( 
    legend.position = "right",
  ) +
  theme_minimal(base_size = 18)
print(mhMod_SHAPS_plot)
ggsave(filename = "mhMod_SHAPS_plot.svg", plot = mhMod_SHAPS_plot, width = 5, height = 5, dpi = 500, path = plotPath)

## FIGURE S6 - SHAPS ON P(ACCEPT) ====

library(ggeffects)
marginal_effects <- ggpredict(mhWinMod, terms = "SHAPS_resc [all]")

mhWinMod_SHAPS_pAccept_plot <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_line(color = "#F9AED3", linewidth = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = "#F9AED3", fill = "#F9AED3" ) +
  labs(
    x = "SHAPS score (z-scored)",
    y = "p(accept)"
  ) +
  theme_minimal(base_size = 18)
print(mhWinMod_SHAPS_pAccept_plot)
ggsave(filename = "mhWinMod_SHAPS_pAccept_plot.svg", plot = mhWinMod_SHAPS_pAccept_plot, width = 5, height = 5, dpi = 500, path = plotPath)


# IS YOUR RANDOM EFX TERM JUSTIFIED???? ====
# take your best model and use an anova to compare its deviance with a logistic
# regression without the random effects

# define a glm equivalent of best model
m0 <- glm(accepted ~ postTournRat + normOutMag + effPrp + as.factor(valence), data = tbtDf, family = binomial)
summary(m0)

# select model to comp to baseGlm or m0
modToTest <- fits$modelsFitted[[2]]
print(modToTest)

# anova to look at deviance of GLMM vs. a log reg
anova(modToTest,baseGlm) # NOTE this can be used to test for the value of including each of the fix effects too

# MIXED ANOVA FOR INTERACTIONS ====

participant_vars <- tbtDf %>%
  group_by(prolificID) %>%
  slice(1) %>%
  select(prolificID, Age_resc, SES, Gender, SHAPS_resc, FAS_resc, 
         AD_resc, Compul_resc, SW_resc)
# Then aggregate the trial-level data

trial_aggregated <- tbtDf %>%
  group_by(prolificID, outProb, normOutMag, effPrp, valence) %>%
  summarise(
    accepted_mean = mean(accepted, na.rm = TRUE),
    .groups = 'drop'
  )
# Join them back together
aggregated_df <- trial_aggregated %>%
  left_join(participant_vars, by = "prolificID")

maov1 <- afex::aov_car(
  accepted_mean ~ outProb + normOutMag + effPrp + valence + AD_resc+SW_resc+Compul_resc +
    Error(prolificID/(outProb+normOutMag+effPrp+valence)),
  data = aggregated_df,
  add = ~ Age_resc + gender, # added as covariates
  type = 3,
  factorize = FALSE)
summary(maov1)

# PARTIAL CORRELATIONS ====

library(ppcor)

## DEFINE CONTROL VARS ====

control_vars <- cbind(as.numeric(summDf$Age), as.factor(summDf$Gender))

## SHAPS ====

pacc_loweff_asin <- asin(sqrt(summDf$percAcceptByEffort_1/100))
pcor.test(summDf$SHAPS_resc, pacc_loweff_asin, control_vars, method = "kendall")

pacc_medeff_asin <- asin(sqrt(summDf$percAcceptByEffort_2/100))
pcor.test(summDf$SHAPS_resc, summDf$percAcceptByEffort_2, control_vars, method = "kendall")

pacc_higheff_asin <- asin(sqrt(summDf$percAcceptByEffort_3/100))
pcor.test(summDf$SHAPS_resc, summDf$percAcceptByEffort_3, control_vars, method = "kendall")

pcor.test(summDf$SHAPS_resc, summDf$percAccept, control_vars, method = "kendall")


