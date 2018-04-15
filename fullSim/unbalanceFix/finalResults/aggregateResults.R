rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(MIIVsem)



baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim/unbalanceFix"
setwd(baseDir)

source("../../SimulationFunctions.R")

allMplusModels <- readRDS("finalResults/allMplus.Rds")
# 
# read in the design matrix
dm <- readRDS("SimParams.rds")
dm <- dm$designMatrix

# remove later
# dm <- subset(dm, dm$estimators=="Muthen")
# as a matrix
dm <- as.matrix(dm)

newVars <- c( "l1.by.y1"   ,
 "l1.by.y1.se",
 "l1.by.y1.sarg",
 "l1.by.y1.sarg.p",
 "l1.by.y2"   ,
 "l1.by.y2.se",
 "l1.by.y2.sarg",
 "l1.by.y2.sarg.p",
 "l1.by.y3"   ,
 "l1.by.y3.se",
 "l1.by.y3.sarg",
 "l1.by.y3.sarg.p",
 "l1.by.y5"   ,
 "l1.by.y5.se",
 "l1.by.y5.sarg",
 "l1.by.y5.sarg.p",
 "l2.by.y4"   ,
 "l2.by.y4.se",
 "l2.by.y4.sarg",
 "l2.by.y4.sarg.p",
 "l2.by.y5"   ,
 "l2.by.y5.se",
 "l2.by.y5.sarg",
 "l2.by.y5.sarg.p",
 "l2.by.y6"   ,
 "l2.by.y6.se",
 "l2.by.y6.sarg",
 "l2.by.y6.sarg.p",
 "l2.by.y2"   ,
 "l2.by.y2.se",
 "l2.by.y2.sarg",
 "l2.by.y2.sarg.p",
 "l1.by.y3"   ,
 "l1.by.y3.se",
 "l1.by.y3.sarg",
 "l1.by.y3.sarg.p",
 "lb.by.y1"   ,
 "lb.by.y1.se",
 "lb.by.y1.sarg",
 "lb.by.y1.sarg.p",
 "lb.by.y2"   ,
 "lb.by.y2.se",
 "lb.by.y2.sarg",
 "lb.by.y2.sarg.p",
 "lb.by.y3"   ,
 "lb.by.y3.se",
 "lb.by.y3.sarg",
 "lb.by.y3.sarg.p",
 "lb.by.y4"   ,
 "lb.by.y4.se",
 "lb.by.y4.sarg",
 "lb.by.y4.sarg.p",
 "lb.by.y5"   ,
 "lb.by.y5.se",
 "lb.by.y5.sarg",
 "lb.by.y5.sarg.p",
 "lb.by.y6"   ,
 "lb.by.y6.se",
 "lb.by.y6.sarg",
 "lb.by.y6.sarg.p")

results <- matrix(nrow = nrow(dm), ncol = length(newVars) +1)
colnames(results) <- c("rdsName", newVars)


# go one at a time through the design matrix
startTime <- Sys.time()
for (i in seq(nrow(dm))) {
# for (i in 1:1000) {
  print(i)
  results[i, "rdsName"] <- dm[i, "rdsName"]
  if (dm[i,"estimators"]=="FIML") {
    # save the .out name
    mName <- paste0(tolower(substr(dm[i, "inpName"], start= 13, stop = nchar(dm[i, "inpName"])-3)), "out")
    # create a DF of parameters from Mplus .out file
    m <- parseMplus(allMplusModels[[mName]])
    mdf <- m$df
    # make 'eqn' var to pick rows later
    mdf$eqn <- tolower(paste0(mdf$lv, "=~", mdf$ind))
    # convert est and se columns to numeric
    mdf$est <- as.numeric(as.character(mdf$est))
    mdf$se  <- as.numeric(as.character(mdf$se))
    # within results
    try(results[i, "l1.by.y1"]        <- mdf[mdf$eqn=="l1=~y1", "est"])
    try(results[i, "l1.by.y1.se"]     <- mdf[mdf$eqn=="l1=~y1", "se"])
    try(results[i, "l1.by.y2"]        <- mdf[mdf$eqn=="l1=~y2", "est"])
    try(results[i, "l1.by.y2.se"]     <- mdf[mdf$eqn=="l1=~y2", "se"])
    try(results[i, "l1.by.y3"]        <- mdf[mdf$eqn=="l1=~y3", "est"])
    try(results[i, "l1.by.y3.se"]     <- mdf[mdf$eqn=="l1=~y3", "se"])
    try(results[i, "l1.by.y5"]        <- mdf[mdf$eqn=="l1=~y5", "est"])
    try(results[i, "l1.by.y5.se"]     <- mdf[mdf$eqn=="l1=~y5", "se"])
    try(results[i, "l2.by.y4"]        <- mdf[mdf$eqn=="l2=~y4", "est"])
    try(results[i, "l2.by.y4.se"]     <- mdf[mdf$eqn=="l2=~y4", "se"])
    try(results[i, "l2.by.y5"]        <- mdf[mdf$eqn=="l2=~y5", "est"])
    try(results[i, "l2.by.y5.se"]     <- mdf[mdf$eqn=="l2=~y5", "se"])
    try(results[i, "l2.by.y6"]        <- mdf[mdf$eqn=="l2=~y6", "est"])
    try(results[i, "l2.by.y6.se"]     <- mdf[mdf$eqn=="l2=~y6", "se"])
    try(results[i, "l2.by.y2"]        <- mdf[mdf$eqn=="l2=~y2", "est"])
    try(results[i, "l2.by.y2.se"]     <- mdf[mdf$eqn=="l2=~y2", "se"])
    # between results
    try(results[i, "lb.by.y1"]        <- mdf[mdf$eqn=="l4=~y1", "est"])
    try(results[i, "lb.by.y1.se"]     <- mdf[mdf$eqn=="l4=~y1", "se"])
    try(results[i, "lb.by.y2"]        <- mdf[mdf$eqn=="l4=~y2", "est"])
    try(results[i, "lb.by.y2.se"]     <- mdf[mdf$eqn=="l4=~y2", "se"])
    try(results[i, "lb.by.y3"]        <- mdf[mdf$eqn=="l4=~y3", "est"])
    try(results[i, "lb.by.y3.se"]     <- mdf[mdf$eqn=="l4=~y3", "se"]  )
    try(results[i, "lb.by.y4"]        <- mdf[mdf$eqn=="l4=~y4", "est"])
    try(results[i, "lb.by.y4.se"]     <- mdf[mdf$eqn=="l4=~y4", "se"])
    try(results[i, "lb.by.y5"]        <- mdf[mdf$eqn=="l4=~y5", "est"])
    try(results[i, "lb.by.y5.se"]     <- mdf[mdf$eqn=="l4=~y5", "se"])
    try(results[i, "lb.by.y6"]        <- mdf[mdf$eqn=="l4=~y6", "est"])
    try(results[i, "lb.by.y6.se"]     <- mdf[mdf$eqn=="l4=~y6", "se"])
  } else {
    mod <- readRDS(dm[i,"rdsName"])
    if (mod=="model did not fit properly") {
      next
    }
    wTable <- MIIVsem::estimatesTable(mod$within, sarg = TRUE)
    bTable <- MIIVsem::estimatesTable(mod$between, sarg = TRUE)
    wTable$eqn <- paste0(wTable$lhs, wTable$op, wTable$rhs)
    bTable$eqn <- paste0(bTable$lhs, bTable$op, bTable$rhs)

	# within
    try(results[i, "l1.by.y1"]        <- wTable[wTable$eqn=="l1=~y1", "est"])
    try(results[i, "l1.by.y1.se"]     <- wTable[wTable$eqn=="l1=~y1", "se"])
    try(results[i, "l1.by.y1.sarg"]   <- wTable[wTable$eqn=="l1=~y1", "sarg"])
    try(results[i, "l1.by.y1.sarg.p"] <- wTable[wTable$eqn=="l1=~y1", "sarg.p"])
    try(results[i, "l1.by.y2"]        <- wTable[wTable$eqn=="l1=~y2", "est"])
    try(results[i, "l1.by.y2.se"]     <- wTable[wTable$eqn=="l1=~y2", "se"])
    try(results[i, "l1.by.y2.sarg"]   <- wTable[wTable$eqn=="l1=~y2", "sarg"])
    try(results[i, "l1.by.y2.sarg.p"] <- wTable[wTable$eqn=="l1=~y2", "sarg.p"])
    try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
    try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
    try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
    try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
    try(results[i, "l1.by.y5"]        <- wTable[wTable$eqn=="l1=~y5", "est"])
    try(results[i, "l1.by.y5.se"]     <- wTable[wTable$eqn=="l1=~y5", "se"])
    try(results[i, "l1.by.y5.sarg"]   <- wTable[wTable$eqn=="l1=~y5", "sarg"])
    try(results[i, "l1.by.y5.sarg.p"] <- wTable[wTable$eqn=="l1=~y5", "sarg.p"])
    try(results[i, "l2.by.y4"]        <- wTable[wTable$eqn=="l2=~y4", "est"])
    try(results[i, "l2.by.y4.se"]     <- wTable[wTable$eqn=="l2=~y4", "se"])
    try(results[i, "l2.by.y4.sarg"]   <- wTable[wTable$eqn=="l2=~y4", "sarg"])
    try(results[i, "l2.by.y4.sarg.p"] <- wTable[wTable$eqn=="l2=~y4", "sarg.p"])
    try(results[i, "l2.by.y5"]        <- wTable[wTable$eqn=="l2=~y5", "est"])
    try(results[i, "l2.by.y5.se"]     <- wTable[wTable$eqn=="l2=~y5", "se"])
    try(results[i, "l2.by.y5.sarg"]   <- wTable[wTable$eqn=="l2=~y5", "sarg"])
    try(results[i, "l2.by.y5.sarg.p"] <- wTable[wTable$eqn=="l2=~y5", "sarg.p"])
    try(results[i, "l2.by.y6"]        <- wTable[wTable$eqn=="l2=~y6", "est"])
    try(results[i, "l2.by.y6.se"]     <- wTable[wTable$eqn=="l2=~y6", "se"])
    try(results[i, "l2.by.y6.sarg"]   <- wTable[wTable$eqn=="l2=~y6", "sarg"])
    try(results[i, "l2.by.y6.sarg.p"] <- wTable[wTable$eqn=="l2=~y6", "sarg.p"])
    try(results[i, "l2.by.y2"]        <- wTable[wTable$eqn=="l2=~y2", "est"])
    try(results[i, "l2.by.y2.se"]     <- wTable[wTable$eqn=="l2=~y2", "se"])
    try(results[i, "l2.by.y2.sarg"]   <- wTable[wTable$eqn=="l2=~y2", "sarg"])
    try(results[i, "l2.by.y2.sarg.p"] <- wTable[wTable$eqn=="l2=~y2", "sarg.p"])
    try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
    try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
    try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
    try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
    try(results[i, "lb.by.y1"]        <- bTable[wTable$eqn=="l1=~y1", "est"])
    try(results[i, "lb.by.y1.se"]     <- bTable[wTable$eqn=="l1=~y1", "se"])
    try(results[i, "lb.by.y1.sarg"]   <- bTable[wTable$eqn=="l1=~y1", "sarg"])
    try(results[i, "lb.by.y1.sarg.p"] <- bTable[wTable$eqn=="l1=~y1", "sarg.p"])
    # between
    try(results[i, "lb.by.y2"]        <- bTable[bTable$eqn=="l1=~y2", "est"])
    try(results[i, "lb.by.y2.se"]     <- bTable[bTable$eqn=="l1=~y2", "se"])
    try(results[i, "lb.by.y2.sarg"]   <- bTable[bTable$eqn=="l1=~y2", "sarg"])
    try(results[i, "lb.by.y2.sarg.p"] <- bTable[bTable$eqn=="l1=~y2", "sarg.p"])
    try(results[i, "lb.by.y3"]        <- bTable[bTable$eqn=="l1=~y3", "est"])
    try(results[i, "lb.by.y3.se"]     <- bTable[bTable$eqn=="l1=~y3", "se"])
    try(results[i, "lb.by.y3.sarg"]   <- bTable[bTable$eqn=="l1=~y3", "sarg"])
    try(results[i, "lb.by.y3.sarg.p"] <- bTable[bTable$eqn=="l1=~y3", "sarg.p"])
    try(results[i, "lb.by.y4"]        <- bTable[bTable$eqn=="l1=~y4", "est"])
    try(results[i, "lb.by.y4.se"]     <- bTable[bTable$eqn=="l1=~y4", "se"])
    try(results[i, "lb.by.y4.sarg"]   <- bTable[bTable$eqn=="l1=~y4", "sarg"])
    try(results[i, "lb.by.y4.sarg.p"] <- bTable[bTable$eqn=="l1=~y4", "sarg.p"])
    try(results[i, "lb.by.y5"]        <- bTable[bTable$eqn=="l1=~y5", "est"])
    try(results[i, "lb.by.y5.se"]     <- bTable[bTable$eqn=="l1=~y5", "se"])
    try(results[i, "lb.by.y5.sarg"]   <- bTable[bTable$eqn=="l1=~y5", "sarg"])
    try(results[i, "lb.by.y5.sarg.p"] <- bTable[bTable$eqn=="l1=~y5", "sarg.p"])
    try(results[i, "lb.by.y6"]        <- bTable[bTable$eqn=="l1=~y6", "est"])
    try(results[i, "lb.by.y6.se"]     <- bTable[bTable$eqn=="l1=~y6", "se"])
    try(results[i, "lb.by.y6.sarg"]   <- bTable[bTable$eqn=="l1=~y6", "sarg"])
    try(results[i, "lb.by.y6.sarg.p"] <- bTable[bTable$eqn=="l1=~y6", "sarg.p"])

  } # end the if
} # end for loop
endTime <- Sys.time()
print(endTime-startTime)
# save the results
# saveRDS(results, "finalResults/resultsMat.rds")
# # merge together
allResults <- plyr::join(as.data.frame(dm),
                         as.data.frame(results),
                         by = "rdsName")
# # save all!
saveRDS(allResults, "finalResults/unbalancedResultsDf.rds")
# make all long


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Now we are going to process subsets for the results section
# originally I was doing the processing in the .RMD file but that caused problems
# Now we make the subsets, and basically use the .RMD file to make the tables/figures
# --------------------------------------------------------------------------
baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)

# read in allresults
dm <- readRDS("finalResults/allResultsDf.rds")
# convert all to character
dm <- as.data.frame(results)
dm[, ] <- lapply(dm[, ], as.character)
# give this function the subset you want and have it process the df
processDF <- function(df) {
  # make it long
  df <- gather(df, parameter, est, l1.by.y1:lb.by.y6.sarg.p, factor_key = FALSE)
  df$est <- as.numeric(df$est)
  # put in true values
  df[df$parameter=="l1.by.y1", "true"] <- 1
  df[df$parameter=="l1.by.y2", "true"] <- .8
  df[df$parameter=="l1.by.y3", "true"] <- .7
  df[df$parameter=="l2.by.y4", "true"] <- 1
  df[df$parameter=="l2.by.y5", "true"] <- .8
  df[df$parameter=="l2.by.y6", "true"] <- .7
  df[df$parameter=="l1.by.y5", "true"] <- .3
  df[df$parameter=="l2.by.y2", "true"] <- .3
  # tru values for between parameters
  df[df$parameter=="lb.by.y2", "true"] <- .7
  df[df$parameter=="lb.by.y3", "true"] <- .6
  df[df$parameter=="lb.by.y4", "true"] <- .8
  df[df$parameter=="lb.by.y5", "true"] <- .7
  df[df$parameter=="lb.by.y6", "true"] <- .8
  # Remove the scaling indicators
  df <- df[which(df$parameter!="l1.by.y1"),]
  df <- df[which(df$parameter!="l2.by.y4"),]
  df <- df[which(df$parameter!="lb.by.y1"),]
  # compute relative bias 
  df$p_relBias <- ((df$est - df$true) / df$true)*100
  # make a cluster var
  df$cluster <- as.factor(paste0(df$clusterN,"groups_of_N", df$clusterSize))
  # getting the order right
  try(df$cluster <- relevel(df$cluster, " 30groups_of_N 30"))
  try(df$cluster <- relevel(df$cluster, " 30groups_of_N100"))
  try(df$cluster <- relevel(df$cluster, "100groups_of_N 30"))
  try(df$cluster <- relevel(df$cluster, "100groups_of_N100"))
  return(df)
}

l_sub <- processDF(dm)
wl <-c("l1.by.y1",
       "l1.by.y2",
       "l1.by.y3",
       "l1.by.y5",
       "l2.by.y4",
       "l2.by.y5",
       "l2.by.y6",
       "l2.by.y2")
ggplot(l_sub[l_sub$parameter %in% wl, ], aes(x=parameter, y = p_relBias)) +
          geom_boxplot() +
          # facet_grid(.~clusterSize + clusterN) +
          # facet_grid(.~cluster, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE) +  
          stat_summary(fun.y = mean, geom="point",colour="darkred", size=1, 
            position = position_dodge(width = .75)) +
          geom_hline(yintercept = 20) + 
          geom_hline(yintercept = -20) + 
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          coord_cartesian(ylim=c(-100, 100)) +
          scale_y_continuous(breaks = seq(from = -100, to = 100, by = 20)) +
          theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5),
                legend.position = 'none') +
          ylab("Percent Relative Bias") +
          xlab("Within Loadings")
# balance  ------------------------------------------------------------------
# subset what we want
l_sub <- subset(dm, dm$skewKurt=="0000")
l_sub <- subset(l_sub, l_sub$clusterBal=="unbal")
l_sub <- subset(l_sub, l_sub$modelSpec=="trueModel")
# l_sub <- subset(l_sub, l_sub$clusterN=="100")
# l_sub <- subset(l_sub, l_sub$clusterSize==" 30")
# process the DF
l_sub <- processDF(l_sub)
# save it so .RMD can read it in
saveRDS(l_sub, "./finalResults/unbalance.RDS")
rm(list="l_sub")
# -----------------------------------------------------------------------------



