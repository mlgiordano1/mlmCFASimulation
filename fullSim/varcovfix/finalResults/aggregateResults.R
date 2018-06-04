rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(MIIVsem)

# baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim/varcovfix"
# setwd(baseDir)
# 
# source("../../SimulationFunctions.R")
# 
# # --------------------------------------------------------------------------
# # I have a more global parseMplus
# # however it doesn't parse the variances. I made a quick and dirty one here
# # the only reason I'm not including it in simulation functions is because
# # I haven't tested this on all.
# # --------------------------------------------------------------------------
# parseMplus <- function(outFile) {
#   # things this will return 
#   # list of: term, df 
#   # did model estimate normally
#   if (any(grepl('THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY', outFile))) {
#     term <- "model estimation DID NOT terminate normally"
#     return(list(term = term, residCov = NULL, saddle = NULL, df = NULL))
#     
#   }
#   if (any(grepl('MODEL ESTIMATION TERMINATED NORMALLY', outFile, fixed = FALSE))) {
#     term <- "normal"
#   }
#   # Residual Cov
#   if (any(grepl("THE RESIDUAL COVARIANCE MATRIX (THETA) IS NOT POSITIVE DEFINITE", outFile, fixed = TRUE))) {
#     residCov <- "Not-positive definite"
#   } else {
#     residCov <- "No error msg"
#   }
#   # Saddle
#   if (any(grepl("SADDLE", outFile, fixed = TRUE))) {
#     saddle <- "yes - saddle"
#   } else {
#     saddle <- "No error msg"
#   }
#   
#   # create a DF to save results
#   df <- data.frame(V1 = NA)
#   dfi <- 1
#   # find within
#   withinLevel <- grep(outFile, pattern = "Within Level")
#   index <- withinLevel+2
#   head <- outFile[[index]]
#   index <- index+1
#   while (TRUE) {
#     if (nchar(outFile[[index]])<61) {
#       break
#     }
#     df[dfi, "V1"] <- paste0(head, outFile[[index]])
#     dfi <- dfi+1
#     index <- index+1
#   }
#   index <- index +1
#   #header should now be l2
#   head <- outFile[[index]]
#   index <- index+1
#   while (TRUE) {
#     if (nchar(outFile[[index]])<61) {
#       break
#     }
#     df[dfi, "V1"] <- paste0(head, outFile[[index]])
#     dfi <- dfi+1
#     index <- index+1
#   }
#   index <- index +1
#   # add the covariance parameters
#   head <- outFile[[index]]
#   index <- index+1
#   df[dfi, "V1"] <- paste0(head, outFile[[index]])
#   dfi <- dfi+1
#   
#   index <- index+2
#   head <- outFile[[index]]
#   index <- index+1
#   df[dfi, "V1"] <- paste0(head, outFile[[index]])
#   dfi <- dfi+1
#   
#   # add variances of l1 and l2
#   index <- index+2
#   head  <- outFile[[index]]
#   index <- index+1
#   df[dfi, "V1"] <- paste0(head, outFile[[index]])
#   dfi <- dfi+1
#   index <- index+1
#   df[dfi, "V1"] <- paste0(head, outFile[[index]])
#   dfi <- dfi+1
#   
#   # find between
#   betweenLevel <- grep(outFile, pattern = "Between Level")
#   index <- betweenLevel+2
#   head <- outFile[[index]]
#   index <- index+1
#   while (TRUE) {
#     if (nchar(outFile[[index]])<61) {
#       break
#     }
#     df[dfi, "V1"] <- paste0(head, outFile[[index]])
#     dfi <- dfi+1
#     index <- index+1
#   }
#   
#   # find between variance
#   t <- grep(outFile, pattern = "Variances")[3]
#   index <- t
#   head <- outFile[[index]]
#   index <- index+1
#   df[dfi, "V1"] <- paste0(head, outFile[[index]])
#   dfi <- dfi+1
#   
#   # clean up the df
#   df <- strsplit(df$V1, " +")
#   df <- do.call(rbind.data.frame, df)
#   names(df) <- c("d", "lv", "by", "ind", "est", "se", "est/se", "p")
#   df <- df[c("lv", "by", "ind", "est", "se", "est/se", "p")]
#   
#   # take out the variances
#   var <- subset(df, lv=="Variances")
#   var$lv <- var$by
#   var$by <- "WITH"
#   var[,4:7] <- var[, 3:6]
#   var$ind <- var$lv
#   
#   # remove the unclean variances from prev
#   df <- subset(df, lv!="Variances")
#   # merge new and clean var
#   df <- rbind(df, var)
#   
#   df$lv <- as.character(df$lv)
#   df$by <- as.character(df$by)
#   df$ind <- as.character(df$ind)
#   df$est <- as.numeric(as.character(df$est))
#   df$se <- as.numeric(as.character(df$se))
#   
#   return(list(term = term, residCov = residCov, saddle = saddle, df = df))
#   
# }
# 
# allMplusModels <- readRDS("../finalResults/allMplus.Rds")
# # 
# # read in the design matrix
# dm <- readRDS("SimParams.rds")
# dm <- dm$designMatrix
# 
# # remove later
# # dm <- subset(dm, dm$estimators=="Muthen")
# # as a matrix
# dm <- as.matrix(dm)
# 
# newVars <- c("l1~~l1",
#              "l2~~l2",
#              "l1~~l2",
#              "y2~~y3",
#              "lb~~lb")
# 
# results <- matrix(nrow = nrow(dm), ncol = length(newVars) +1)
# colnames(results) <- c("rdsName", newVars)
# 
# 
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
#     mdf$eqn  <- tolower(paste(mdf$lv, mdf$by, mdf$ind, sep = "."))
#     # convert est and se columns to numeric
#     mdf$est <- as.numeric(as.character(mdf$est))
#     mdf$se  <- as.numeric(as.character(mdf$se))
#     
#     # within
#     try(results[i, "l1~~l1"]        <- mdf[mdf$eqn=="l1.with.l1", "est"])
#     try(results[i, "l2~~l2"]        <- mdf[mdf$eqn=="l2.with.l2", "est"])
#     try(results[i, "l1~~l2"]        <- mdf[mdf$eqn=="l1.with.l2", "est"])
#     try(results[i, "y2~~y3"]        <- mdf[mdf$eqn=="y2.with.y3", "est"])
#     # between
#     try(results[i, "lb~~lb"]        <- mdf[mdf$eqn=="l4.with.l4", "est"])
#     
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
# 	  # within
#     try(results[i, "l1~~l1"]        <- wTable[wTable$eqn=="l1~~l1", "est"])
#     try(results[i, "l2~~l2"]        <- wTable[wTable$eqn=="l2~~l2", "est"])
#     try(results[i, "l1~~l2"]        <- wTable[wTable$eqn=="l1~~l2", "est"])
#     try(results[i, "y2~~y3"]        <- wTable[wTable$eqn=="y2~~y3", "est"])
#     
#     # between 
#     try(results[i, "lb~~lb"]        <- bTable[bTable$eqn=="l1~~l1", "est"])
#     
#     
#     
# 
#   } # end the if
# } # end for loop
# endTime <- Sys.time()
# print(endTime-startTime)
# # save the results
# # saveRDS(results, "finalResults/resultsMat.rds")
# # # merge together
# allResults <- plyr::join(as.data.frame(dm),
#                          as.data.frame(results),
#                          by = "rdsName")
# # # save all!
# saveRDS(allResults, "finalResults/varCovDf.rds")
# # make all long


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Now we are going to process subsets for the results section
# originally I was doing the processing in the .RMD file but that caused problems
# Now we make the subsets, and basically use the .RMD file to make the tables/figures
# --------------------------------------------------------------------------
baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim/varcovfix"
setwd(baseDir)

# read in allresults
dm <- readRDS("finalResults/varcovDf.rds")
# convert all to character
dm <- as.data.frame(dm)
dm[, ] <- lapply(dm[, ], as.character)

  # make it long
  dm <- gather(dm, parameter, est, 18:22, factor_key = FALSE)
  dm$est <- as.numeric(dm$est)
  # put in true values
  dm[dm$parameter=="l1~~l1", "true"] <- 2
  dm[dm$parameter=="l2~~l2", "true"] <- 2
  dm[dm$parameter=="l1~~l2", "true"] <- .3
  dm[dm$parameter=="y2~~y3", "true"] <- .3
  dm[dm$parameter=="lb~~lb", "true"] <- .5

  # doing some parameter renaming
  dm[dm$parameter=="l1~~l1",'parameter'] <- "L1 Variance"
  dm[dm$parameter=="l2~~l2",'parameter'] <- "L2 Variance"
  dm[dm$parameter=="l1~~l2",'parameter'] <- "L1/L2 Corr"
  dm[dm$parameter=="y2~~y3",'parameter'] <- "Y2/Y3 Corr"
  dm[dm$parameter=="lb~~lb",'parameter'] <- "LB Variance"
  dm$parameter <- as.factor(dm$parameter)
  try(dm$parameter <- relevel(dm$parameter, "LB Variance"))
  try(dm$parameter <- relevel(dm$parameter, "Y2/Y3 Corr"))
  try(dm$parameter <- relevel(dm$parameter, "L1/L2 Corr"))
  try(dm$parameter <- relevel(dm$parameter, "L2 Variance"))
  try(dm$parameter <- relevel(dm$parameter, "L1 Variance"))
  
   # compute relative bias 
  dm$p_relBias   <- ((dm$est - dm$true) / dm$true)*100
  # make a cluster var
  dm$clusterN    <- as.numeric(dm$clusterN)
  dm$clusterSize <- as.numeric(dm$clusterSize)
  # dm$cluster   <- as.factor(paste0(dm$clusterN,"groups_of_N", dm$clusterSize))
  dm$cluster     <- as.factor(paste0("CN = ",dm$clusterN,"; CS = ", dm$clusterSize))
  # getting the order right
  try(dm$cluster <- relevel(dm$cluster, "CN = 30; CS = 30"))
  try(dm$cluster <- relevel(dm$cluster, "CN = 30; CS = 100"))
  try(dm$cluster <- relevel(dm$cluster, "CN = 100; CS = 30"))
  try(dm$cluster <- relevel(dm$cluster, "CN = 100; CS = 100"))



# save 
saveRDS(dm, "./finalResults/cleaned_varcov.RDS")





