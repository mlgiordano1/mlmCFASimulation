library(tidyr)
library(dplyr)

baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/presentationsim"
setwd(baseDir)

# allMplusModels <- MplusAutomation::readModels("savedModels")
# saveRDS(allMplusModels, "savedModels/allMplusModels.rds")

m <- readRDS("savedModels/allMplusModels.rds")

# m[1]
# m$bal_100_100_misspec_normal_fiml1.out$parameters


# read in the design matrix
dm <- readRDS("SimParams.rds")
dm <- dm$designMatrix

# go one at a time through the design matrix
for (i in seq(nrow(dm))) {
  print(i)
  if (dm$estimators[i]=="FIML") {
  mName <- tolower(paste0(substr(dm$rdsName[i], 13, nchar(dm$rdsName[i])-3), "out"))
  mod <- m[[mName]]
  param <- mod[["parameters"]][["unstandardized"]]
  param$name <- paste0(param$paramHeader, param$param)
  dm[i, "l1.by.y1"] <- as.numeric(param[param$name=="L1.BYY1", "est"])
  dm[i, "l1.by.y2"] <- as.numeric(param[param$name=="L1.BYY2", "est"])
  dm[i, "l1.by.y3"] <- as.numeric(param[param$name=="L1.BYY3", "est"])
  dm[i, "l2.by.y4"] <- as.numeric(param[param$name=="L2.BYY4", "est"])
  dm[i, "l2.by.y5"] <- as.numeric(param[param$name=="L2.BYY5", "est"])
  dm[i, "l2.by.y6"] <- as.numeric(param[param$name=="L2.BYY6", "est"])
  # add in the misspecified parameters
  } else {
  mod <- readRDS(dm$rdsName[i])
  # dm[i, "l1.by.y1"] <- mod$within$coefficients[["y1~l1"]]
  dm[i, "l1.by.y2"] <- mod$within$coefficients[["y2~l1"]]
  dm[i, "l1.by.y3"] <- mod$within$coefficients[["y3~l1"]]
  # dm[i, "l2.by.y4"] <- mod$within$coefficients[["y4~l2"]]
  dm[i, "l2.by.y5"] <- mod$within$coefficients[["y5~l2"]]
  dm[i, "l2.by.y6"] <- mod$within$coefficients[["y6~l2"]]
  } # end the if
} # end for loop


dmLong <- gather(dm, paramter, est, l1.by.y2:l2.by.y4, factor_key = TRUE) 

# put in the true values
dmLong[dmLong$paramter=="l1.by.y1", "true"] <- 1
dmLong[dmLong$paramter=="l1.by.y2", "true"] <- .8
dmLong[dmLong$paramter=="l1.by.y3", "true"] <- .7
dmLong[dmLong$paramter=="l2.by.y4", "true"] <- 1
dmLong[dmLong$paramter=="l2.by.y5", "true"] <- .8
dmLong[dmLong$paramter=="l2.by.y6", "true"] <- .7

saved <- dmLong %>%
  group_by(clusterSize, clusterN, clusterBal, modelSpec, estimators, paramter) %>%
  summarize(count = n(),
            mean  = mean(est),
            true  = mean(true))
