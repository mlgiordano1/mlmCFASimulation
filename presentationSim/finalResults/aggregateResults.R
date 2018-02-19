library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/presentationsim"
setwd(baseDir)

# allMplusModels <- MplusAutomation::readModels("savedModels")
# saveRDS(allMplusModels, "savedModels/allMplusModels.rds")
m <- readRDS("savedModels/allMplusModels.rds")

# read in the design matrix
dm <- readRDS("SimParams.rds")
dm <- dm$designMatrix

# go one at a time through the design matrix
for (i in seq(nrow(dm))) {
  print(i)
  try({
  if (dm$estimators[i]=="FIML") {
  mName <- paste0(tolower(paste(dm$clusterBal[i], 
                   dm$clusterSize[i], 
                    dm$clusterN[i], 
                    dm$modelSpec[i], 
                    dm$distribution[i], 
                    dm$estimators[i], 
                    dm$Iteration[i], sep = "_")), ".out")
  mod <- m[[mName]]
  param <- mod[["parameters"]][["unstandardized"]]
  param$name <- paste0(param$paramHeader, param$param)
  dm[i, "l1.by.y1"] <- as.numeric(param[param$name=="L1.BYY1", "est"])
  dm[i, "l1.by.y2"] <- as.numeric(param[param$name=="L1.BYY2", "est"])
  dm[i, "l1.by.y3"] <- as.numeric(param[param$name=="L1.BYY3", "est"])
  dm[i, "l2.by.y4"] <- as.numeric(param[param$name=="L2.BYY4", "est"])
  dm[i, "l2.by.y5"] <- as.numeric(param[param$name=="L2.BYY5", "est"])
  dm[i, "l2.by.y6"] <- as.numeric(param[param$name=="L2.BYY6", "est"])
  
  #dm[i, "LB.by.y1"] <- as.numeric(param[param$name=="L4.BYY1", "est"])
  dm[i, "LB.by.y2"] <- as.numeric(param[param$name=="L4.BYY2", "est"])
  dm[i, "LB.by.y3"] <- as.numeric(param[param$name=="L4.BYY3", "est"])
  dm[i, "LB.by.y4"] <- as.numeric(param[param$name=="L4.BYY4", "est"])
  dm[i, "LB.by.y5"] <- as.numeric(param[param$name=="L4.BYY5", "est"])
  dm[i, "LB.by.y6"] <- as.numeric(param[param$name=="L4.BYY6", "est"])
  # add in the misspecified parameters
  } else {
  mod <- readRDS(dm$rdsName[i])
  # dm[i, "l1.by.y1"] <- mod$within$coefficients[["y1~l1"]]
  dm[i, "l1.by.y2"] <- mod$within$coefficients[["y2~l1"]]
  dm[i, "l1.by.y3"] <- mod$within$coefficients[["y3~l1"]]
  # dm[i, "l2.by.y4"] <- mod$within$coefficients[["y4~l2"]]
  dm[i, "l2.by.y5"] <- mod$within$coefficients[["y5~l2"]]
  dm[i, "l2.by.y6"] <- mod$within$coefficients[["y6~l2"]]
  
  dm[i, "LB.by.y2"] <- mod$between$coefficients[["y2~l1"]]
  dm[i, "LB.by.y3"] <- mod$between$coefficients[["y3~l1"]]
  dm[i, "LB.by.y4"] <- mod$between$coefficients[["y4~l1"]]
  dm[i, "LB.by.y5"] <- mod$between$coefficients[["y5~l1"]]
  dm[i, "LB.by.y6"] <- mod$between$coefficients[["y6~l1"]]

  } # end the if
  })# end of try
} # end for loop

saveRDS(dm, "savedModels/results.rds")

dmLong <- gather(dm, paramter, est, l1.by.y2:LB.by.y6, factor_key = TRUE) 

# put in the true values
dmLong[dmLong$paramter=="l1.by.y1", "true"] <- 1
dmLong[dmLong$paramter=="l1.by.y2", "true"] <- .8
dmLong[dmLong$paramter=="l1.by.y3", "true"] <- .7
dmLong[dmLong$paramter=="l2.by.y4", "true"] <- 1
dmLong[dmLong$paramter=="l2.by.y5", "true"] <- .8
dmLong[dmLong$paramter=="l2.by.y6", "true"] <- .7
# tru values for between parameters
dmLong[dmLong$paramter=="LB.by.y2", "true"] <- .7
dmLong[dmLong$paramter=="LB.by.y3", "true"] <- .6
dmLong[dmLong$paramter=="LB.by.y4", "true"] <- .8
dmLong[dmLong$paramter=="LB.by.y5", "true"] <- .7
dmLong[dmLong$paramter=="LB.by.y6", "true"] <- .8

# Remove the scaling indicators
dmLong <- dmLong[which(dmLong$paramter!="l1.by.y1"),]
dmLong <- dmLong[which(dmLong$paramter!="l2.by.y4"),]

#relative bias
dmLong$relBias <- (dmLong$est - dmLong$true) / dmLong$true

saved <- dmLong %>%
  group_by(clusterSize, clusterN, clusterBal, modelSpec, estimators, paramter) %>%
  summarize(count = n(),
            mean  = mean(est),
            true  = mean(true),
            rel.bias = ((est-true)/true))

ggplot(dmLong[which(dmLong$modelSpec=="trueModel"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,1))

ggplot(dmLong[which(dmLong$modelSpec=="misSpec1"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  scale_y_continuous(limits = c(-.5,1))

ggplot(dmLong[which(dmLong$modelSpec=="misSpec2"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,1))

ggplot(dmLong[which(dmLong$modelSpec=="misSpec3"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  scale_y_continuous(limits = c(-.5,1))



