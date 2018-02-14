# set the type to fit
estimator <- "Muthen"
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
setwd(baseDir)



simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer
wModelTrue    <- simParams$wModelTrue
wModelMis     <- simParams$wModelMis
bModelTrue    <- simParams$bModelTrue


#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------
# source relevant functions
source("../simulationfunctions.R")
dataDir     <- paste0(getwd(), "/rawData")
fitModelDir <- paste0(getwd(), "/savedModels")

# subset just the estimator we want
designMatrix <- designMatrix[which(designMatrix$estimators==estimator),]


for (i in 3841:3920) {
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
  
  # read in data
  df <- read.table(designMatrix$dfName[[i]])
  names(df) <- c(paste0("y", 1:6), "cluster")
  df$id <- 1:nrow(df)
  
  fit <- mlcfaMIIV(withinModel = wModel, 
                   betweenModel = bModel, 
                   estimator = designMatrix$estimators[[i]], 
                   allIndicators = paste0("y", 1:6), 
                   l1Var = "id", 
                   l2Var = "cluster", 
                   df = df)
  #save as RDS
  saveRDS(fit, file = designMatrix$rdsName[[i]])
}
