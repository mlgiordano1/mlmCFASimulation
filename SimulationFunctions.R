# Create Directory Structure
# only run this once because it will overwrite everything else
createDirStr <- function(baseDir) {
  if (!dir.exists(path=paste0(baseDir, "/rawData"))) {
    dir.create(path=paste0(baseDir, "/rawData"))
    print("created raw data directory")
  } else {
    print("Raw Data directory already exists")
  }
  if (!dir.exists(path=paste0(baseDir, "/savedModels"))) {
    dir.create(path=paste0(baseDir, "/savedModels"))
    print("Created savedModels directory")
  } else {
    print("savedModels directory already exists")
  }
  if (!dir.exists(path=paste0(baseDir, "/finalResults"))) {
    dir.create(path=paste0(baseDir, "/finalResults"))
    print("Created finalResults directory")
  } else {
    print("finalResults directory already exists")
  }
}

createDesignMatrix <- function(nIter,sampleSize,clusterSizes,
                               modelSpec,distribution,estimators) {
  # setup the Matrix
  localMat <- expand.grid(seq(nIter),
                          sampleSize,
                          clusterSizes,
                          modelSpec,
                          distribution,
                          estimators)
  # name the columns will make it easier to pull
  names(localMat) <- c("Iteration",
                      "sampleSize",
                      "clusterSizes",
                      "modelSpec",
                      "distribution",
                      "estimators")
  return(localMat)
}
