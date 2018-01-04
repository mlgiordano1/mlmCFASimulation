# TODO:
# Look below to the section after my goldstein function


# Decisions to keep in mind
# 1. Subset the rows corresponding to only relevant items
# 2. do comparisons pairwise (as opposed to 3 at a time,etc)
#    not sure if there would be much time gain by doing 3,
#    and it appears as if the results are nearly identical
# 3. 



# Decisions to be made
# Nlme vs LME?
# play around with different optimizers
# REML vs ML

#install.packages('nlme')


#library('nlme')
#library('lme4')
#library("reshape2")
#library('dummies')
#library('dplyr')
library('magrittr')

setwd("C:/Users/mgiordan/git/mlmcfasimulation/temp")
source("goldsteinFunction.R")


# ------------------------------------------------------------------------------
# Process Data
myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)
# # make data long
# long <- reshape2::melt(myData, id.vars = c("id", "cluster"), variable.name = "item")
# # dummy code the indicators
# for (level in unique(long$item)) {
#   long[paste("dummy", level, sep = "_")] <- ifelse(long$item == level, 1, 0)
#   long[level] <- ifelse(long$item == level, 1, 0)
# }
# ------------------------------------------------------------------------------

# save some values
indicators = c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")
indicators = c("y1", "y2", "y3")


bModel <- '
l1 =~ y1+y2+y3+y4+y5+y6+y7+y8+y9
'

wModel <- '
l1=~y1+y2+y3+y5
l2=~y4+y5+y6+y8
l3=~y7+y8+y9+y6
l1~~l2
l1~~l3
l2~~l3'

temp <- mlcfaMIIV(withinModel = wModel,
                  betweenModel = bModel, 
                  estimator = "muthen",
                  allIndicators = indicators,
                  l1Var = "id", 
                  l2Var = "cluster", 
                  df = myData)


# TESting the time
startTime <- proc.time()
temp <- goldstein(withinModel = wModel,
                  betweenModel = bModel, 
                  allIndicators = indicators,
                  l1Var = "id", 
                  l2Var = "cluster", 
                  df = myData) 
endTime <- proc.time()
endTime-startTime
View(temp$within)

temp$within
temp$between

withinModel = wModel
betweenModel = bModel 
allIndicators = indicators
l1Var = "id"
l2Var = "cluster"
df = myData

temp$within



MIIVsem::miive(wModel, sample.cov = temp$within, sample.nobs = 1000)

MIIVsem::miive(bModel, sample.cov = temp$between, sample.nobs = 100)

?MIIVsem::miive



