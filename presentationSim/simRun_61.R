library("lavaan", lib.loc="/nas/longleaf/home/mgiordan/Rlibs")
library("MIIVsem", lib.loc="/nas/longleaf/home/mgiordan/Rlibs")
library("nlme", lib.loc="/nas/longleaf/home/mgiordan/Rlibs")

# set the type to fit
estimator <- "Goldstein"
baseDir <- "/nas/longleaf/home/mgiordan/forumPres"
# baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/forumpres"
setwd(baseDir)



simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer
wModelTrue    <- simParams$wModelTrue
# wModelMis     <- simParams$wModelMis
bModelTrue    <- simParams$bModelTrue

wModelMis1 <- '
l1=~y1+y2+y3
l2=~y4+y5+y6+y2
y2~~y3
l1~~l2
'
  
wModelMis2 <- '
l1=~y1+y2+y3+y5
l2=~y4+y5+y6
y2~~y3
l1~~l2
'

wModelMis3 <- '
l1=~y1+y2+y3+y5
l2=~y4+y5+y6+y2
l1~~l2
'

#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------
# source relevant functions
source("SimulationFunctions.R")

# subset just the estimator we want
designMatrix <- designMatrix[which(designMatrix$estimators==estimator),]
designMatrix <- designMatrix[which(designMatrix$modelSpec=="misSpec"),]


for (i in 2401:2440) {
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
    #wModel <- wModelMis
    bModel <- bModelTrue
    
    # read in data
    df <- read.table(designMatrix$dfName[[i]])
    names(df) <- c(paste0("y", 1:6), "cluster")
    df$id <- 1:nrow(df)
  
    fit1 <- mlcfaMIIV(withinModel = wModelMis1, 
                   betweenModel = bModel, 
                   estimator = designMatrix$estimators[[i]], 
                   allIndicators = paste0("y", 1:6), 
                   l1Var = "id", 
                   l2Var = "cluster", 
                   df = df)
    fit2 <- mlcfaMIIV(withinModel = wModelMis2, 
                   betweenModel = bModel, 
                   estimator = designMatrix$estimators[[i]], 
                   allIndicators = paste0("y", 1:6), 
                   l1Var = "id", 
                   l2Var = "cluster", 
                   df = df)
    fit3 <- mlcfaMIIV(withinModel = wModelMis3, 
                   betweenModel = bModel, 
                   estimator = designMatrix$estimators[[i]], 
                   allIndicators = paste0("y", 1:6), 
                   l1Var = "id", 
                   l2Var = "cluster", 
                   df = df)
  }
  

  #save as RDS
  saveRDS(fit1, file = gsub("misSpec", "misSpec1", designMatrix$rdsName[i]))
  saveRDS(fit2, file = gsub("misSpec", "misSpec2", designMatrix$rdsName[i]))
  saveRDS(fit3, file = gsub("misSpec", "misSpec3", designMatrix$rdsName[i]))
}

designMatrix$rdsName[[i]]


