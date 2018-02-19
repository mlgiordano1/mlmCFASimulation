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
dm <- readRDS("finalResults/results.rds")

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

within <- c("l1.by.y2", "l1.by.y3", "l2.by.y5", "l2.by.y6")
within <- dmLong[dmLong$paramter %in% within,]

# want to drop some misSpec Conditions
within <- within[!(within$modelSpec %in% "misSpec"),]
within <- within[!(within$modelSpec %in% "misSpec1"),]
# do some renaming
within[which(within$modelSpec=="misSpec1"), "modelSpec"] <- "Missing L1 to Y5"
within[which(within$modelSpec=="misSpec2"), "modelSpec"] <- "Missing L2 to Y2"
within[which(within$modelSpec=="misSpec3"), "modelSpec"] <- "msg corr err"
# re-ordering factors
within$modelSpec <- factor(within$modelSpec, levels(factor(within$modelSpec))[c(3,1:2)])

ggplot(within, aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("Within: All 3 specifications")
ggsave("finalResults/Within All 3 specifications.jpeg")

ggplot(within[which(within$modelSpec=="trueModel"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("Within: True Model")
ggsave("finalResults/Within True Model.jpeg")

ggplot(within[which(within$modelSpec=="Missing L2 to Y2"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("Within: Missing Cross Loading")
ggsave("finalResults/Within Missing Cross Loading.jpeg")

ggplot(within[which(within$modelSpec=="msg corr err"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("Within: Missing Correlated Error")
ggsave("finalResults/Within Missing Correlated Error.jpeg")


p1 <- ggplot(within[which(within$modelSpec=="trueModel"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2))
ggsave("finalResults/withinTrue.jpeg")

p2 <- ggplot(within[which(within$modelSpec=="Missing L2 to Y2"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(modelSpec~clusterSize) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2))
ggsave("finalResults/withinMisspec1.jpeg")

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



