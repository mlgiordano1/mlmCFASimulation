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
source("mlcfaFunction.R")
source("simulating mlcfa.R")

# --------------------------------------------------------------------------
# The model we will use
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

bSimModel <- '
l1 =~ y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6 + 1*y7 + 1*y8 + 1*y9
'

wSimModel <- '
l1 =~ y1 + 1*y2 + 1*y3 + 0.3*y5
l2 =~ y4 + 1*y5 + 1*y6 + 0.3*y8
l3 =~ y7 + 1*y8 + 1*y9 + 0.3*y6
l1 ~~ 0.3*l2
l1 ~~ 0.3*l3
l2 ~~ 0.3*l3'

# save some values
indicators = c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")


# ------------------------------------------------------------------------------
# Process Data
myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)
# ------------------------------------------------------------------------------

myData2 <- gen(sampleSize = 3000, 
               clustSize = 30, 
               varWithinLV = 1, 
               varWithinIndE = .5, 
               varBtwLV = 1, 
               varBtwIndE = 3)




temp <- mlcfaMIIV(withinModel = wModel,
                  betweenModel = bModel, 
                  estimator = "muthen",
                  allIndicators = indicators,
                  l1Var = "id", 
                  l2Var = "cluster", 
                  df = myData2)

temp$within
temp$between

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





