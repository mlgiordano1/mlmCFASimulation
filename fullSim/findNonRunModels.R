rm(list=ls())

# set the working director
try({
  baseDir <- "/nas/longleaf/home/mgiordan/fullSim"
  setwd(baseDir)
})
try({
  baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
  setwd(baseDir)
})

# reading in the parameters of the model
simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer

# just searching through goldstein and muthen
sub <- subset(designMatrix, !designMatrix$estimators=="FIML")
# does the .rds file esist
sub$modelRun <- file.exists(sub$rdsName)
# check results
table(sub$estimators, sub$modelRun)

#subset only those models that have NOT been run
cleanUp <- subset(sub, sub$modelRun==FALSE)
simParams$designMatrix <- cleanUp

saveRDS(simParams, file = "nonRunModels.rds")

