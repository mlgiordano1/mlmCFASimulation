rm(list=ls())
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim/varcovfix"
setwd(baseDir)
allParams <- readRDS("../simParams.rds")

makeNewData <- TRUE

designMatrix <- allParams$designMatrix
designMatrix <- subset(designMatrix, skewKurt == "0000")
designMatrix <- subset(designMatrix, clusterBal == "bal")
designMatrix <- subset(designMatrix, modelSpec =="trueModel")


# The data generating matrices for using SemTools
(wLambda        = matrix(c(1.0, 0.0,
                           0.8, 0.3,
                           0.7, 0.0,
                           0.0, 1.0,
                           0.3, 0.8, 
                           0.0, 0.7),
                         nrow  = 6, 
                         byrow = TRUE))
(wPsi           = matrix(c(2.0, 0.3,
                           0.3, 2.0), 
                         nrow = 2, 
                         byrow = TRUE))
wTheta         = matrix(0, nrow = 6, ncol = 6)
diag(wTheta)   = .8
wTheta[2, 3]   = .3
wTheta[3, 2]   = .3
# between matrices
bLambda        = matrix(data = c(1.0, 
                                 0.7, 
                                 0.6, 
                                 0.8, 
                                 0.7, 
                                 0.8), 
                        nrow  = 6, 
                        byrow = TRUE)
bPsi           = matrix(c(.5), nrow = 1, byrow = FALSE)
bTheta         = matrix(0, nrow = 6, ncol = 6)
diag(bTheta)   = .2
# bTheta[6, 5]   = .2
# bTheta[5, 6]   = .2
bTheta


#----------------------------------------------------------------------------
# Should not need to edit below this line
#----------------------------------------------------------------------------
setwd(baseDir)


  
# source relevant functions
source("../../simulationfunctions.R")
dataDir <- "rawData"
fitModelDir <- "savedModels"
# Set up the directory structure
createDirStr(baseDir=baseDir)

# merging the seeds into the between cell factors
btwCell <- designMatrix[,c("Iteration", 
                        "clusterSize", 
                        "clusterN", 
                        "clusterBal", 
                        "wSkew",
                        "wKurt",
                        "bSkew",
                        "bKurt",
                        "dfName", "seed")]
btwCell <- unique(btwCell)



# create data based on design matrix
if (makeNewData==TRUE) {
  for (i in seq(nrow(btwCell))) {
    print(paste0("Generating and saving dataset ", i, "/", nrow(btwCell)))
    if (btwCell$clusterBal[i]=="bal") {
      bal <- TRUE
    } else {
      bal <- FALSE
    }
    set.seed(btwCell$seed[i])
    df <- simData2(indicatorNames = paste0("y", 1:6),
                   wLambda        = wLambda,
                   wPsi           = wPsi,
                   wTheta         = wTheta,
                   bLambda        = bLambda,
                   bPsi           = bPsi,
                   bTheta         = bTheta,
                   clusterNo      = btwCell$clusterN[i],
                   clusterSize    = btwCell$clusterSize[i],
                   wSkew          = btwCell$wSkew[i],
                   wKurt          = btwCell$wKurt[i], 
                   bSkew          = btwCell$bSkew[i],
                   bKurt          = btwCell$bKurt[i],
                   clusterBal     = bal,
                   seed           = btwCell$seed[i])
    # write df as .RDS file for miiv
    saveRDS(df, file = btwCell$dfName[i])
    # write df as .dat file for MPlus
    # data.table::fwrite(x         = df, 
    #                    file      = btwCell$datName[i], 
    #                    append    = FALSE,
    #                    quote     = FALSE, 
    #                    sep       = "\t", 
    #                    row.names = FALSE, 
    #                    col.names = FALSE)
  }
}



allParams$designMatrix <- designMatrix
#save all relevant infomation about simulation
saveRDS(allParams,
             "SimParams.rds")


