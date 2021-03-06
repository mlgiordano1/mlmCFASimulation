# TODO
# add in the outside decomp to the muthen estimator
# remove printing function
# add a progress bar?


# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation"
makeNewData <- FALSE
# The model we will use
bModelTrue <- '
l1 =~ y1+y2+y3+y4+y5+y6+y7+y8+y9
'

wModelTrue <- '
l1=~y1+y2+y3+y5
l2=~y4+y5+y6+y8
l3=~y7+y8+y9+y6
l1~~l2
l1~~l3
l2~~l3
'

wModelMis <- '
l1=~y1+y2+y3
l2=~y4+y5+y6
l3=~y7+y8+y9
l1~~l2
l1~~l3
l2~~l3
'
# number of iterations
iterationsPer <- 5


#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------
setwd(baseDir)
# source relevant functions
source("simulationfunctions.R")
dataDir <- paste0(getwd(), "/rawData")
fitModelDir <- paste0(getwd(), "/savedModels")
# Set up the directory structure
createDirStr(baseDir=baseDir)
# make the design matrix
designMatrix <- createDesignMatrix(nIter = iterationsPer,
                                   sampleSize = c(3000, 6000),
                                   clusterSizes = c("bal", "unbal"),
                                   modelSpec = c("trueModel", "misSpec"),
                                   distribution = c("normal", "non-Normal"),
                                   estimators = c("FIML", "Goldstein", "Muthen"))

designMatrix <- designMatrix[designMatrix$estimators=="Muthen",]

 # make DF names
designMatrix$dfName <- paste0(dataDir, "/",
                              designMatrix$sampleSize, "_",
                              designMatrix$clusterSizes, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$Iteration,
                              ".dat")
#make rds name
designMatrix$rdsName <- paste0(fitModelDir, "/",
                              designMatrix$sampleSize, "_",
                              designMatrix$clusterSizes, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$estimators,
                              designMatrix$Iteration,
                              ".rds")

# create data based on design matrix
if (makeNewData==TRUE) {
  makeDataMplus(wd = "C:/Users/mgiordan/git/mlmcfasimulation",
                iterations = 5,
                designMatrix = designMatrix)
}

# do the thing
 
pb <- txtProgressBar(min = 0, max = nrow(designMatrix), style = 3) 

for (i in seq(nrow(designMatrix))) {
  setTxtProgressBar(pb, i)
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
  names(df) <- c(paste0("y", 1:9), "cluster")
  df$id <- 1:nrow(df)
  
  fit <- mlcfaMIIV(withinModel = wModel, 
                   betweenModel = bModel, 
                   estimator = designMatrix$estimators[[i]], 
                   allIndicators = paste0("y", 1:9), 
                   l1Var = "id", 
                   l2Var = "cluster", 
                   df = df)
  #save as RDS
  saveRDS(fit, file = designMatrix$rdsName[[i]])
}
