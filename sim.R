# Create a base directory on your own
baseDir <- "C:/users/mgiordan/desktop/mcfasim"
setwd(baseDir)


# Create Directory Structure
# only run this once because it will overwrite everything else
createDirStr <- function(baseDir) {
  dir.create(path=paste0(baseDir, "/rawData"))
  dir.create(path=paste0(baseDir, "/modelFits"))
  dir.create(path=paste0(baseDir, "/finalResults"))
}

createDesignMatrix <- function(nIter,sampleSize,clusterSizes,
                               strMisSpec,distMisSpec,estimators) {
  # setup the Matrix
  localMat <- expand.grid(seq(nIter),
                          sampleSize,
                          clusterSize,
                          strMisSpec,
                          distMisSpec,
                          estimator)
  # name the columns will make it easier to pull
  names(localMat) <- c("nIter",
                      "sampleSize",
                      "clusterSize",
                      "strMisSpec",
                      "distMisSpec",
                      "estimator")
  return(localMat)
}

designMatrix <- createDesignMatrix(
  nIter = 2,
  sampleSize = 1200,
  clusterSizes = "Balanced",
  strMisSpec = "CorrectModel",
  distMisSpec = "CorrectModel",
  estimators = c("FIML", "GoldsteinMIIV", "MuthenMIIV"))







?setNames

# simulate DataFrames

createDirStr(baseDir = baseDir)
