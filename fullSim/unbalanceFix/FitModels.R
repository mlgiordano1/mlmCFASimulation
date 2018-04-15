rm(list=ls())
# set the type to fit
estimator <- "Muthen"
# set the working director
try({
  baseDir <- "/nas/longleaf/home/mgiordan/unbalanceFix"
  setwd(baseDir)
})
try({
  baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim/unbalanceFix"
  setwd(baseDir)
})
# reading in the parameters of the model
simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer


#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------

args=commandArgs(TRUE)
print(args[1])
condition <- as.numeric(args[1])


# load relevant packages
try({
  library("lavaan",  lib.loc="/nas/longleaf/home/mgiordan/Rlibs")
  library("MIIVsem", lib.loc="/nas/longleaf/home/mgiordan/Rlibs")
  library("nlme",    lib.loc="/nas/longleaf/home/mgiordan/Rlibs")
})
try({
  library("lavaan")
  library("MIIVsem")
  library("nlme")
})
# source relevant functions
try({
  source("SimulationFunctions.R")      # for longleaf
})
try({
  source("../../SimulationFunctions.R")   # for my computer
})
# subset just the estimator we want
designMatrix <- designMatrix[which(designMatrix$estimators==estimator),]
# for (i in condition:(condition+24)) { # startingPoint!
for (i in seq(nrow(designMatrix))) {
  print(i)
  # if the current row is the FIML estimator move to next bc fiml is all Mplus
  if (designMatrix$estimators[[i]]=="FIML") {
    next
  }
  # set the model spec
  if (designMatrix$modelSpec[[i]]=="trueModel") {
    wModel <- simParams$wModelTrue
    bModel <- simParams$bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpecW1") {
    wModel <- simParams$wModelMis1
    bModel <- simParams$bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpecW2") {
    wModel <- simParams$wModelMis2
    bModel <- simParams$bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpecW3") {
    wModel <- simParams$wModelMis3
    bModel <- simParams$bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpecB") {
    wModel <- simParams$wModelTrue
    bModel <- simParams$bModelMis
  }
  
  # read in data
  df <- readRDS(designMatrix$dfName[[i]])
  
  fit <- tryCatch({
    mlcfaMIIV(withinModel   = wModel, 
              betweenModel  = bModel, 
              estimator     = designMatrix$estimators[[i]], 
              allIndicators = paste0("y", 1:6), 
              l1Var         = "id", 
              l2Var         = "cluster", 
              df            = df)
  }, warning = function(e) {
    message(e)
    return("model did not fit properly")
  }, error = function(e) {
    message(e)
    return("model did not fit properly")
  })
  #save as RDS
  saveRDS(fit, file = designMatrix$rdsName[[i]])
}



