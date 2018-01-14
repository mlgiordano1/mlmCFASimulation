# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation"
setwd(baseDir)

# source relevant functions
source("simulationfunctions.R")

# Set up the directory structure
createDirStr(baseDir=baseDir)
# make the design matrix
designMatrix <- createDesignMatrix(
                  nIter = 1,
                  sampleSize = c(3000, 6000),
                  clusterSizes = c("bal", "unbal"),
                  modelSpec = c("trueModel", "misSpec"),
                  distribution = c("normal", "non-Normal"),
                  estimators = c("FIML", "GoldsteinMIIV", "MuthenMIIV"))

# create data based on design matrix
makeDataMplus(wd = "C:/Users/mgiordan/git/mlmcfasimulation",
              iterations = 5,
              designMatrix = designMatrix)

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
