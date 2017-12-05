# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation"
setwd(baseDir)

# Set up the directory structure
createDirStr(baseDir=baseDir)
# make the design matrix
designMatrix <- createDesignMatrix(
                  nIter = 2,
                  sampleSize = 1200,
                  clusterSizes = "Balanced",
                  modelSpec = "CorrectModel",
                  distribution = "CorrectModel",
                  estimators = c("FIML", "GoldsteinMIIV", "MuthenMIIV"))







?setNames

# simulate DataFrames

createDirStr(baseDir = baseDir)
