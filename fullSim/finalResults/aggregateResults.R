rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(MIIVsem)



baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)

source("../SimulationFunctions.R")

# allMplusModels <- readRDS("finalResults/allMplus.Rds")
# 
# # read in the design matrix
# dm <- readRDS("SimParams.rds")
# dm <- dm$designMatrix
# 
# # remove this later
# # dm <- subset(dm, dm$estimators=="FIML")
# 
# dm <- as.matrix(dm)
# 
# newVars <- c( "l1.by.y1"   ,
#  "l1.by.y1.se",
#  "l1.by.y1.sarg",
#  "l1.by.y1.sarg.p",
#  "l1.by.y2"   ,
#  "l1.by.y2.se",
#  "l1.by.y2.sarg",
#  "l1.by.y2.sarg.p",
#  "l1.by.y3"   ,
#  "l1.by.y3.se",
#  "l1.by.y3.sarg",
#  "l1.by.y3.sarg.p",
#  "l1.by.y5"   ,
#  "l1.by.y5.se",
#  "l1.by.y5.sarg",
#  "l1.by.y5.sarg.p",
#  "l2.by.y4"   ,
#  "l2.by.y4.se",
#  "l2.by.y4.sarg",
#  "l2.by.y4.sarg.p",
#  "l2.by.y5"   ,
#  "l2.by.y5.se",
#  "l2.by.y5.sarg",
#  "l2.by.y5.sarg.p",
#  "l2.by.y6"   ,
#  "l2.by.y6.se",
#  "l2.by.y6.sarg",
#  "l2.by.y6.sarg.p",
#  "l2.by.y2"   ,
#  "l2.by.y2.se",
#  "l2.by.y2.sarg",
#  "l2.by.y2.sarg.p",
#  "l1.by.y3"   ,
#  "l1.by.y3.se",
#  "l1.by.y3.sarg",
#  "l1.by.y3.sarg.p",
#  "lb.by.y1"   ,
#  "lb.by.y1.se",
#  "lb.by.y1.sarg",
#  "lb.by.y1.sarg.p",
#  "lb.by.y2"   ,
#  "lb.by.y2.se",
#  "lb.by.y2.sarg",
#  "lb.by.y2.sarg.p",
#  "lb.by.y3"   ,
#  "lb.by.y3.se",
#  "lb.by.y3.sarg",
#  "lb.by.y3.sarg.p",
#  "lb.by.y4"   ,
#  "lb.by.y4.se",
#  "lb.by.y4.sarg",
#  "lb.by.y4.sarg.p",
#  "lb.by.y5"   ,
#  "lb.by.y5.se",
#  "lb.by.y5.sarg",
#  "lb.by.y5.sarg.p",
#  "lb.by.y6"   ,
#  "lb.by.y6.se",
#  "lb.by.y6.sarg",
#  "lb.by.y6.sarg.p")
# 
# results <- matrix(nrow = nrow(dm), ncol = length(newVars) +1)
# colnames(results) <- c("rdsName", newVars)
# # go one at a time through the design matrix
# startTime <- Sys.time()
# for (i in seq(nrow(dm))) {
# # for (i in 1:1000) {
#   print(i)
#   results[i, "rdsName"] <- dm[i, "rdsName"]
#   if (dm[i,"estimators"]=="FIML") {
#     # save the .out name
#     mName <- paste0(tolower(substr(dm[i, "inpName"], start= 13, stop = nchar(dm[i, "inpName"])-3)), "out")
#     # create a DF of parameters from Mplus .out file 
#     m <- parseMplus(allMplusModels[[mName]])
#     mdf <- m$df
#     # make 'eqn' var to pick rows later
#     mdf$eqn <- tolower(paste0(mdf$lv, "=~", mdf$ind))
#     # convert est and se columns to numeric
#     mdf$est <- as.numeric(as.character(mdf$est))
#     mdf$se  <- as.numeric(as.character(mdf$se))
#     # within results
#     try(results[i, "l1.by.y1"]        <- mdf[mdf$eqn=="l1=~y1", "est"])
#     try(results[i, "l1.by.y1.se"]     <- mdf[mdf$eqn=="l1=~y1", "se"])
#     try(results[i, "l1.by.y2"]        <- mdf[mdf$eqn=="l1=~y2", "est"])
#     try(results[i, "l1.by.y2.se"]     <- mdf[mdf$eqn=="l1=~y2", "se"])
#     try(results[i, "l1.by.y3"]        <- mdf[mdf$eqn=="l1=~y3", "est"])
#     try(results[i, "l1.by.y3.se"]     <- mdf[mdf$eqn=="l1=~y3", "se"])
#     try(results[i, "l1.by.y5"]        <- mdf[mdf$eqn=="l1=~y5", "est"])
#     try(results[i, "l1.by.y5.se"]     <- mdf[mdf$eqn=="l1=~y5", "se"])
#     try(results[i, "l2.by.y4"]        <- mdf[mdf$eqn=="l2=~y4", "est"])
#     try(results[i, "l2.by.y4.se"]     <- mdf[mdf$eqn=="l2=~y4", "se"])
#     try(results[i, "l2.by.y5"]        <- mdf[mdf$eqn=="l2=~y5", "est"])
#     try(results[i, "l2.by.y5.se"]     <- mdf[mdf$eqn=="l2=~y5", "se"])
#     try(results[i, "l2.by.y6"]        <- mdf[mdf$eqn=="l2=~y6", "est"])
#     try(results[i, "l2.by.y6.se"]     <- mdf[mdf$eqn=="l2=~y6", "se"])
#     try(results[i, "l2.by.y2"]        <- mdf[mdf$eqn=="l2=~y2", "est"])
#     try(results[i, "l2.by.y2.se"]     <- mdf[mdf$eqn=="l2=~y2", "se"])
#     # between results
#     try(results[i, "lb.by.y1"]        <- mdf[mdf$eqn=="l4=~y1", "est"])
#     try(results[i, "lb.by.y1.se"]     <- mdf[mdf$eqn=="l4=~y1", "se"])
#     try(results[i, "lb.by.y2"]        <- mdf[mdf$eqn=="l4=~y2", "est"])
#     try(results[i, "lb.by.y2.se"]     <- mdf[mdf$eqn=="l4=~y2", "se"])
#     try(results[i, "lb.by.y3"]        <- mdf[mdf$eqn=="l4=~y3", "est"])
#     try(results[i, "lb.by.y3.se"]     <- mdf[mdf$eqn=="l4=~y3", "se"]  )
#     try(results[i, "lb.by.y4"]        <- mdf[mdf$eqn=="l4=~y4", "est"])
#     try(results[i, "lb.by.y4.se"]     <- mdf[mdf$eqn=="l4=~y4", "se"])
#     try(results[i, "lb.by.y5"]        <- mdf[mdf$eqn=="l4=~y5", "est"])
#     try(results[i, "lb.by.y5.se"]     <- mdf[mdf$eqn=="l4=~y5", "se"])
#     try(results[i, "lb.by.y6"]        <- mdf[mdf$eqn=="l4=~y6", "est"])
#     try(results[i, "lb.by.y6.se"]     <- mdf[mdf$eqn=="l4=~y6", "se"])
#   } else {
#     mod <- readRDS(dm[i,"rdsName"])
#     if (mod=="model did not fit properly") {
#       next
#     }
#     wTable <- MIIVsem::estimatesTable(mod$within, sarg = TRUE)
#     bTable <- MIIVsem::estimatesTable(mod$between, sarg = TRUE)
#     wTable$eqn <- paste0(wTable$lhs, wTable$op, wTable$rhs)
#     bTable$eqn <- paste0(bTable$lhs, bTable$op, bTable$rhs)
#     
# 	# within
#     try(results[i, "l1.by.y1"]        <- wTable[wTable$eqn=="l1=~y1", "est"])
#     try(results[i, "l1.by.y1.se"]     <- wTable[wTable$eqn=="l1=~y1", "se"])
#     try(results[i, "l1.by.y1.sarg"]   <- wTable[wTable$eqn=="l1=~y1", "sarg"])
#     try(results[i, "l1.by.y1.sarg.p"] <- wTable[wTable$eqn=="l1=~y1", "sarg.p"])
#     try(results[i, "l1.by.y2"]        <- wTable[wTable$eqn=="l1=~y2", "est"])
#     try(results[i, "l1.by.y2.se"]     <- wTable[wTable$eqn=="l1=~y2", "se"])
#     try(results[i, "l1.by.y2.sarg"]   <- wTable[wTable$eqn=="l1=~y2", "sarg"])
#     try(results[i, "l1.by.y2.sarg.p"] <- wTable[wTable$eqn=="l1=~y2", "sarg.p"])
#     try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
#     try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
#     try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
#     try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
#     try(results[i, "l1.by.y5"]        <- wTable[wTable$eqn=="l1=~y5", "est"])
#     try(results[i, "l1.by.y5.se"]     <- wTable[wTable$eqn=="l1=~y5", "se"])
#     try(results[i, "l1.by.y5.sarg"]   <- wTable[wTable$eqn=="l1=~y5", "sarg"])
#     try(results[i, "l1.by.y5.sarg.p"] <- wTable[wTable$eqn=="l1=~y5", "sarg.p"])
#     try(results[i, "l2.by.y4"]        <- wTable[wTable$eqn=="l2=~y4", "est"])
#     try(results[i, "l2.by.y4.se"]     <- wTable[wTable$eqn=="l2=~y4", "se"])
#     try(results[i, "l2.by.y4.sarg"]   <- wTable[wTable$eqn=="l2=~y4", "sarg"])
#     try(results[i, "l2.by.y4.sarg.p"] <- wTable[wTable$eqn=="l2=~y4", "sarg.p"])
#     try(results[i, "l2.by.y5"]        <- wTable[wTable$eqn=="l2=~y5", "est"])
#     try(results[i, "l2.by.y5.se"]     <- wTable[wTable$eqn=="l2=~y5", "se"])
#     try(results[i, "l2.by.y5.sarg"]   <- wTable[wTable$eqn=="l2=~y5", "sarg"])
#     try(results[i, "l2.by.y5.sarg.p"] <- wTable[wTable$eqn=="l2=~y5", "sarg.p"])
#     try(results[i, "l2.by.y6"]        <- wTable[wTable$eqn=="l2=~y6", "est"])
#     try(results[i, "l2.by.y6.se"]     <- wTable[wTable$eqn=="l2=~y6", "se"])
#     try(results[i, "l2.by.y6.sarg"]   <- wTable[wTable$eqn=="l2=~y6", "sarg"])
#     try(results[i, "l2.by.y6.sarg.p"] <- wTable[wTable$eqn=="l2=~y6", "sarg.p"])
#     try(results[i, "l2.by.y2"]        <- wTable[wTable$eqn=="l2=~y2", "est"])
#     try(results[i, "l2.by.y2.se"]     <- wTable[wTable$eqn=="l2=~y2", "se"])
#     try(results[i, "l2.by.y2.sarg"]   <- wTable[wTable$eqn=="l2=~y2", "sarg"])
#     try(results[i, "l2.by.y2.sarg.p"] <- wTable[wTable$eqn=="l2=~y2", "sarg.p"])
#     try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
#     try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
#     try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
#     try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
#     try(results[i, "lb.by.y1"]        <- bTable[wTable$eqn=="l1=~y1", "est"])
#     try(results[i, "lb.by.y1.se"]     <- bTable[wTable$eqn=="l1=~y1", "se"])
#     try(results[i, "lb.by.y1.sarg"]   <- bTable[wTable$eqn=="l1=~y1", "sarg"])
#     try(results[i, "lb.by.y1.sarg.p"] <- bTable[wTable$eqn=="l1=~y1", "sarg.p"])
#     # between
#     try(results[i, "lb.by.y2"]        <- bTable[bTable$eqn=="l1=~y2", "est"])
#     try(results[i, "lb.by.y2.se"]     <- bTable[bTable$eqn=="l1=~y2", "se"])
#     try(results[i, "lb.by.y2.sarg"]   <- bTable[bTable$eqn=="l1=~y2", "sarg"])
#     try(results[i, "lb.by.y2.sarg.p"] <- bTable[bTable$eqn=="l1=~y2", "sarg.p"])
#     try(results[i, "lb.by.y3"]        <- bTable[bTable$eqn=="l1=~y3", "est"])
#     try(results[i, "lb.by.y3.se"]     <- bTable[bTable$eqn=="l1=~y3", "se"])
#     try(results[i, "lb.by.y3.sarg"]   <- bTable[bTable$eqn=="l1=~y3", "sarg"])
#     try(results[i, "lb.by.y3.sarg.p"] <- bTable[bTable$eqn=="l1=~y3", "sarg.p"])
#     try(results[i, "lb.by.y4"]        <- bTable[bTable$eqn=="l1=~y4", "est"])
#     try(results[i, "lb.by.y4.se"]     <- bTable[bTable$eqn=="l1=~y4", "se"])
#     try(results[i, "lb.by.y4.sarg"]   <- bTable[bTable$eqn=="l1=~y4", "sarg"])
#     try(results[i, "lb.by.y4.sarg.p"] <- bTable[bTable$eqn=="l1=~y4", "sarg.p"])
#     try(results[i, "lb.by.y5"]        <- bTable[bTable$eqn=="l1=~y5", "est"])
#     try(results[i, "lb.by.y5.se"]     <- bTable[bTable$eqn=="l1=~y5", "se"])
#     try(results[i, "lb.by.y5.sarg"]   <- bTable[bTable$eqn=="l1=~y5", "sarg"])
#     try(results[i, "lb.by.y5.sarg.p"] <- bTable[bTable$eqn=="l1=~y5", "sarg.p"])
#     try(results[i, "lb.by.y6"]        <- bTable[bTable$eqn=="l1=~y6", "est"])
#     try(results[i, "lb.by.y6.se"]     <- bTable[bTable$eqn=="l1=~y6", "se"])
#     try(results[i, "lb.by.y6.sarg"]   <- bTable[bTable$eqn=="l1=~y6", "sarg"])
#     try(results[i, "lb.by.y6.sarg.p"] <- bTable[bTable$eqn=="l1=~y6", "sarg.p"])
# 
#   } # end the if
# } # end for loop
# endTime <- Sys.time()
# print(endTime-startTime)
# save the results
# saveRDS(results, "finalResults/resultsMat.rds")
# # merge together
# allResults <- plyr::join(as.data.frame(dm), 
#                          as.data.frame(results), 
#                          by = "rdsName")
# # save all!
# saveRDS(allResults, "finalResults/allResultsDf.rds")
dm <- readRDS("finalResults/allResultsDf.rds")

# # make a single skew condition var
# dm$skewKurt <- paste0(dm$wSkew, dm$wKurt, dm$bSkew, dm$bKurt)
n <-  c("Iteration", "clusterSize", "clusterN", "clusterBal", 
  "modelSpec", "estimators", "skewKurt", "dfName", "datName", 
  "sampleSize", "rdsName", "inpName")
loadings <- c("l1.by.y1",
"l1.by.y2",
"l1.by.y3",
"l1.by.y5",
"l2.by.y4",
"l2.by.y5",
"l2.by.y6",
"l2.by.y2",
"l1.by.y3",
"lb.by.y1",
"lb.by.y2",
"lb.by.y3",
"lb.by.y4",
"lb.by.y5",
"lb.by.y6")

# loadings long
loadLong <- gather(dm[c(n, loadings)], 
  paramter, est, l1.by.y1:lb.by.y6, factor_key = TRUE) 
# convert to numeric
loadLong$est <- as.numeric(loadLong$est)

# put in the true values
loadLong[loadLong$paramter=="l1.by.y1", "true"] <- 1
loadLong[loadLong$paramter=="l1.by.y2", "true"] <- .8
loadLong[loadLong$paramter=="l1.by.y3", "true"] <- .7
loadLong[loadLong$paramter=="l2.by.y4", "true"] <- 1
loadLong[loadLong$paramter=="l2.by.y5", "true"] <- .8
loadLong[loadLong$paramter=="l2.by.y6", "true"] <- .7
# tru values for between parameters
loadLong[loadLong$paramter=="LB.by.y2", "true"] <- .7
loadLong[loadLong$paramter=="LB.by.y3", "true"] <- .6
loadLong[loadLong$paramter=="LB.by.y4", "true"] <- .8
loadLong[loadLong$paramter=="LB.by.y5", "true"] <- .7
loadLong[loadLong$paramter=="LB.by.y6", "true"] <- .8
# Remove the scaling indicators
loadLong <- loadLong[which(loadLong$paramter!="l1.by.y1"),]
loadLong <- loadLong[which(loadLong$paramter!="l2.by.y4"),]

#relative bias
loadLong$relBias <- (loadLong$est - loadLong$true) / loadLong$true
names(loadLong)
btwFactors <- c("clusterSize", "clusterN", "clusterBal", "skewKurt")
wFactors <- c("modelSpec", "Estimators")

dmLong <- dmLong[complete.cases(dmLong),]

saved <- loadLong %>%
  group_by(clusterSize, clusterN, clusterBal, skewKurt, modelSpec, estimators, paramter) %>%
  summarize(count = n(),
            mean  = mean(est, na.rm = TRUE),
            meanBias = mean(relBias, na.rm = TRUE),
            meanBias2 = sum((est-true)/true, na.rm = TRUE)/n(),
            rmse = sqrt(sum((est-true)^2, na.rm = TRUE)/n()),
            true  = mean(true))
# need to do some reshaping
temp <- reshape2::dcast(saved, clusterSize + clusterN + clusterBal + skewKurt ~ modelSpec+paramter+estimators, value.var = "mean" )
# save as a csv so we can edit
write.csv(x = temp, file = "finalResults/parametersTable.csv")


saved$btwKey <- paste0(saved$clusterSize, saved$clusterN, saved$clusterBal, saved$skewKurt)


temp <- reshape(saved, v.names = "mean", timevar = c("modelSpec", "estimators", "paramter"), idvar = "btwKey", direction = "wide")

?reshape()


reshape(dat1, idvar = "name", timevar = "numbers", direction = "wide")
# pulling just the within parameters
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

# pulling just the within parameters
between <- c("LB.by.y2", "LB.by.y3","LB.by.y4", "LB.by.y5", "LB.by.y6")
between <- dmLong[dmLong$paramter %in% between,]
# want to drop some misSpec Conditions
between <- between[!(between$modelSpec %in% "misSpec"),]
between <- between[!(between$modelSpec %in% "misSpec1"),]
# do some renaming
between[which(between$modelSpec=="misSpec1"), "modelSpec"] <- "Missing L1 to Y5"
between[which(between$modelSpec=="misSpec2"), "modelSpec"] <- "Missing L2 to Y2"
between[which(between$modelSpec=="misSpec3"), "modelSpec"] <- "msg corr err"
# re-ordering factors
between$modelSpec <- factor(between$modelSpec, levels(factor(between$modelSpec))[c(3,1:2)])

ggplot(within[within$modelSpec=="trueModel",], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(~skewKurt) +
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


ggplot(between, aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  facet_grid(~skewKurt) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("between: All 3 specifications")
ggsave("finalResults/between All 3 specifications.jpeg")

ggplot(between[which(between$modelSpec=="trueModel"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("between: True Model")
ggsave("finalResults/between True Model.jpeg")

ggplot(between[which(between$modelSpec=="Missing L2 to Y2"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("between: Missing Cross Loading")
ggsave("finalResults/between Missing Cross Loading.jpeg")

ggplot(between[which(between$modelSpec=="msg corr err"),], aes(x=paramter, y = relBias, fill = estimators)) +
  geom_boxplot() +
  #facet_grid(~modelSpec) +
  geom_hline(yintercept = .2) + 
  geom_hline(yintercept = -.2) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(-.5,.5), breaks = seq(from = -.8, to = .8, by = .2)) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  ggtitle("between: Missing Correlated Error")
ggsave("finalResults/between Missing Correlated Error.jpeg")




