# LxM LOAD AND PREPROCESS ====
## function for loading variants of the LxMfinal data set and doing some requisite pre-processing

## MAKE SURE YOU CHANGE base_dir

# LIBS ====

library(dplyr)

# DEFINE FUNCTION ====

 lxm_load_preprocess <- function(clean, tourn_remove) {
  
   base_dir <- "/Users/calum/lxmAnalysisPub/data/" # change this
   
   # for now just edit the number after lxm to change version
   fileNameTbt <- "choice_trialByTrial.csv"
   fileNameSumm <- "summaryFull.csv"
   
   fPathTbt <- paste(base_dir, fileNameTbt, sep = "/")
   fPathSumm <- paste(base_dir, fileNameSumm, sep = "/")
  
# SET DFS ====
  
  tbtDf <- read.csv(fPathTbt)
  summDf <- read.csv(fPathSumm)
  
## RENAME SUMMDF ID COL ====
  
  summDf <- summDf %>%
    rename(
      prolificID = ParticipantId,
    )
  
  
# RESCALING AND FACTORISING ====
    # to aid mixed mod fitting
  
  tbtDf <- tbtDf %>%
   mutate(valence = ifelse(valence == -1, 0, 1))
  tbtDf$valence <- as.factor(tbtDf$valence)

## LEARNING FIDELITY PREDICTORS ====
  
  tbtDf$postLearnRat_resc <- scale(tbtDf$postLearnRat, center = T, scale = T)
  tbtDf$postTournRat_resc <- scale(tbtDf$postTournRat, center = T, scale = T)
  tbtDf$postEffortRat_resc <- scale(tbtDf$postEffortRat, center = T, scale = T)
  tbtDf$meanEstByStim_resc <- scale(tbtDf$meanEstByStim, center = T, scale = T)
  tbtDf$finLearningEst_resc <- scale (tbtDf$finLearningEst, center = T, scale = T)
  
## DECISION VARS ====
  
  tbtDf$effDiscEV <- scale(tbtDf$effDiscEV, center = T, scale = T)
  tbtDf$alienEV <- scale(tbtDf$alienEV, center = T, scale = T)
  
# RESCALING/FACTORISING DEMOGRAPHIC PREDICTORS ====
  tbtDf$Age_resc <- scale(tbtDf$Age, center = T, scale = T)
  tbtDf$Sex <- as.factor(tbtDf$Sex)
  tbtDf$Gender <- as.factor(tbtDf$Gender)
  gender_map <- c("Man (including Trans Male/Trans Man)" = 1, "Woman (including Trans Female/Trans Woman)" = 2,
                  "Non-binary (would like to give more detail)" = 3, "Rather not say" = 4)
  tbtDf$Gender_numeric <- gender_map[tbtDf$Gender]
  tbtDf$Gender_factor <- as.numeric(factor(tbtDf$Gender))
  
## QUESTIONNAIRES ==== 
  
  tbtDf$SHAPS_resc <- scale(tbtDf$SHAPS, center = T, scale = T)
  summDf$SHAPS_resc <- scale(summDf$SHAPS, center = T, scale = T)
  tbtDf$AD_resc <- scale(tbtDf$AD, center = T, scale = T)
  tbtDf$SW_resc <- scale(tbtDf$SW, center = T, scale = T)
  tbtDf$Compul_resc <- scale(tbtDf$Compul, center = T, scale = T)

 tbtDf$FAS_resc <- scale(tbtDf$FAS, center = T, scale = T)
 tbtDf$AES_resc <- scale(tbtDf$AES, center = T, scale = T)
 tbtDf$STAI_resc <- scale(tbtDf$STAI, center = T, scale = T)
 
 summDf$FAS_resc <- scale(summDf$FAS, center = T, scale = T)
 summDf$AES_resc <- scale(summDf$AES, center = T, scale = T)
 summDf$STAI_resc <- scale(summDf$STAI, center = T, scale = T)

## TOURNAMENT ACC EXCLUSION ====
  
  if (tourn_remove == 1) {

     # Remove rows where ovr tourn acc is better than random     
     threshold_tourn <- 50 # random responding

     tbtDf <- tbtDf[tbtDf$ovrperc > threshold_tourn, ]
     summDf <- summDf[summDf$ovrperc > threshold_tourn, ]
     
  }
  
## CREATE EV DIFFERENCE VARIABLE ====
  
  tbtDf <- tbtDf %>%
     mutate(
        evDiff = ifelse(
           outMag < 0,
           (abs(outMag) * outProb) - (abs(outMag) * 1),
           (abs(outMag) * outProb) - (abs(outMag) * 0)
        )
     )
  tbtDf$evDiff <- abs(tbtDf$evDiff)
  
  return(list(tbtDf = tbtDf, summDf = summDf))
 }
