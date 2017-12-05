# Create Directory Structure
# only run this once because it will overwrite everything else
createDirStr <- function(baseDir) {
  dir.create(path=paste0(baseDir, "/rawData"))
  dir.create(path=paste0(baseDir, "/modelFits"))
  dir.create(path=paste0(baseDir, "/finalResults"))
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
