# set the type to fit
estimator <- "Goldstein"
# set the working director
try({
  baseDir <- "/nas/longleaf/home/mgiordan/forumPres"
  setwd(baseDir)
})
try({
  baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
  setwd(baseDir)
})
# reading in the parameters of the model
simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer
wModelTrue    <- simParams$wModelTrue
wModelMis     <- simParams$wModelMis
wModelMis1    <- simParams$wModelMis1
wModelMis2    <- simParams$wModelMis2
wModelMis3    <- simParams$wModelMis3
bModelTrue    <- simParams$bModelTrue


#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------
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
  source("../SimulationFunctions.R")   # for my computer
})
# subset just the estimator we want
designMatrix <- designMatrix[which(designMatrix$estimators==estimator),]
for (i in 5201:5300) {
  print(i)
  # if the current row is the FIML estimator move to next bc fiml is all Mplus
  if (designMatrix$estimators[[i]]=="FIML") {
    next
  }
  # set the model spec
  if (designMatrix$modelSpec[[i]]=="trueModel") {
    wModel <- wModelTrue
    bModel <- bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpec") {
    wModel <- wModelMis
    bModel <- bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpec1") {
    wModel <- wModelMis1
    bModel <- bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpec2") {
    wModel <- wModelMis2
    bModel <- bModelTrue
  }
  if (designMatrix$modelSpec[[i]]=="misSpec3") {
    wModel <- wModelMis3
    bModel <- bModelTrue
  }
  
  # read in data
  df <- read.table(designMatrix$dfName[[i]])
  names(df) <- c(paste0("y", 1:6), "cluster")
  df$id <- 1:nrow(df)
  
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



