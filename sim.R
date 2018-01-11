# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation"
setwd(baseDir)

# Set up the directory structure
createDirStr(baseDir=baseDir)
# make the design matrix
designMatrix <- createDesignMatrix(
                  nIter = 8,
                  sampleSize = c(3000, 6000),
                  clusterSizes = "Balanced",
                  modelSpec = c("trueModel", "misSpec"),
                  distribution = "normal",
                  estimators = c("FIML", "GoldsteinMIIV", "MuthenMIIV"))

# general setup
# for each row in matrix
# 1. read in data
# 2. fit model
# 3. save output

for (i in seq(nrow(designMatrix))) {
  # read in data
  dfName <- paste(designMatrix$Iteration, "_",
                  designMatrix$sampleSize, "_",
                  designMatrix$clusterSizes, "_",
                  designMatrix$distribution,
                  sep = "")
}