rm(list=ls())
# factors for the simulation:
# use even numbers for clusterSize and clusterN
# simulation will automatically make unbalanced have half clusters -15 and half +15
iterationsPer= 600        # number of iterations per between cell condition
# between cell factors
clusterSize  = c(30,100)        # use even numbers
clusterN     = c(30,100)        # use even numbers
clusterBal   = c("bal", "unbal")   # bal and unbal
wSkew        = c(0, 2)
wKurt        = c(0, 8)
bSkew        = c(0, 2)
bKurt        = c(0, 8)
print(paste0("There are ", length(clusterSize)*length(clusterN)*length(clusterBal)*4, 
  " between group conditions"))
# Within cell factors
# modelSpec    = c("trueModel") # trueModel and misSpec
modelSpec    = c("trueModel", "misSpecW1", "misSpecW2", "misSpecW3", "misSpecB") # trueModel and misSpec
estimators   = c("FIML", "Muthen", "Goldstein") # FIML, Goldstein, Muthen
# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
makeNewData <- TRUE

# The data generating models
bGenModel <- '
l1 =~ 1*y1+ .7*y2 +.6*y3 + .8*y4 + .7*y5 + .8*y6
l1~~.5*l1
y1~~.2*y1
y2~~.2*y2
y3~~.2*y3
y4~~.2*y4
y5~~.2*y5
y6~~.2*y6
'

wGenModel <- '
l1 =~ 1*y1 + .8*y2 + .7*y3 + .3*y5
l2 =~ 1*y4 + .8*y5 + .7*y6 + .3*y2
y2~~.3*y3
y1~~.8*y1
y2~~.8*y2
y3~~.8*y3
y4~~.8*y4
y5~~.8*y5
y6~~.8*y6
l1~~2*l1
l2~~2*l2
l1~~.3*l2
'
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
bTheta[6, 5]   = .2
bTheta[5, 6]   = .2
bTheta

# the data fitting models
bModelTrue <- '
l1 =~ y1+y2+y3+y4+y5+y6
y5~~y6
'

bModelMis <- '
l1 =~ y1+y2+y3+y4+y5+y6
'

wModelTrue <- '
l1=~y1+y2+y3+y5
l2=~y4+y5+y6+y2
y2~~y3
l1~~l2
'
wModelMis <- '
l1=~y1+y2+y3
l2=~y4+y5+y6
l1~~l2
'

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
setwd(baseDir)

if (file.exists("seeds.rds")){
  if(winDialog(type = "yesno", 
               paste("'Seeds.rds' already exists, and contains a ",
                     "fixed set of seeds for the simulation. ",
                     "Making new seeds could change results.
                     
                     Do you want to make new seeds?")) == "YES" ) {
    # make seeds No's
    seeds <- runif(n = 100000, min = 0, max = 1000000000)
    saveRDS(seeds, "seeds.rds")
    print("new seeds made")
  } else {
    seeds <- readRDS("seeds.rds")
  }
} else {
  # make seeds No's
  seeds <- runif(n = 100000, min = 0, max = 1000000000)
  saveRDS(seeds, "seeds.rds")
  print("new seeds made")
}
  
# source relevant functions
source("../simulationfunctions.R")
dataDir <- "rawData"
fitModelDir <- "savedModels"
# Set up the directory structure
createDirStr(baseDir=baseDir)
# make the design matrix
designMatrix <- createDesignMatrix(nIter        = iterationsPer,
                                   clusterSize  = clusterSize,
                                   clusterN     = clusterN,
                                   clusterBal   = clusterBal,
                                   wSkew        = wSkew,
                                   wKurt        = wKurt, 
                                   bSkew        = bSkew, 
                                   bKurt        = bKurt,
                                   #within factors
                                   modelSpec    = modelSpec,
                                   estimators   = estimators)

# that created a fully balanced case, which we don't really want
# correcting and just pulling cases we want
designMatrix[which(designMatrix$wSkew == 0 & designMatrix$wKurt  > 0), ] <- NA
designMatrix[which(designMatrix$wSkew >  0 & designMatrix$wKurt == 0), ] <- NA
designMatrix[which(designMatrix$bSkew == 0 & designMatrix$bKurt  > 0), ] <- NA
designMatrix[which(designMatrix$bSkew >  0 & designMatrix$bKurt == 0), ] <- NA
designMatrix <- designMatrix[complete.cases(designMatrix),]
designMatrix$skewKurt <- paste0(designMatrix$wSkew, designMatrix$wKurt, 
                                designMatrix$bSkew, designMatrix$bKurt )
unique(designMatrix$skewKurt)

# merging the seeds into the between cell factors
btwCell <- designMatrix[,c("Iteration", 
                        "clusterSize", 
                        "clusterN", 
                        "clusterBal", 
                        "wSkew",
                        "wKurt",
                        "bSkew",
                        "bKurt")]
btwCell <- unique(btwCell)
btwCell$seed <- seeds[1:nrow(btwCell)]
btwCell$dfName <- paste0(dataDir, "/",
                         btwCell$clusterSize, "_",
                         btwCell$clusterN, "_",
                         btwCell$clusterBal, "_",
                         btwCell$wSkew, btwCell$wKurt,
                         btwCell$bSkew, btwCell$bKurt, "_",
                         btwCell$Iteration,
                         ".rds")
btwCell$datName <- paste0(dataDir, "/",
                         btwCell$clusterSize, "_",
                         btwCell$clusterN, "_",
                         btwCell$clusterBal, "_",
                         btwCell$wSkew, btwCell$wKurt,
                         btwCell$bSkew, btwCell$bKurt, "_",
                         btwCell$Iteration,
                         ".dat")


designMatrix <- merge(designMatrix, btwCell)

# compute sample size
designMatrix$sampleSize <- designMatrix$clusterSize*designMatrix$clusterN
# make rds name
designMatrix$rdsName <- paste0(fitModelDir, "/",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$clusterBal, "_",
                              designMatrix$wSkew, designMatrix$wKurt,
                              designMatrix$bSkew, designMatrix$bKurt, "_",
                              designMatrix$Iteration,
                              ".rds")
# make inp name
designMatrix$inpName <- paste0(fitModelDir, "/",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$clusterBal, "_",
                              designMatrix$wSkew, designMatrix$wKurt,
                              designMatrix$bSkew, designMatrix$bKurt, "_",
                              # within factors
                              designMatrix$modelSpec, "_",
                              designMatrix$estimators, "_",
                              designMatrix$Iteration,
                              ".inp")
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
    data.table::fwrite(x         = df, 
                       file      = btwCell$datName[i], 
                       append    = FALSE,
                       quote     = FALSE, 
                       sep       = "\t", 
                       row.names = FALSE, 
                       col.names = FALSE)
  }
}

#save all relevant infomation about simulation
saveRDS(list(designMatrix  = designMatrix,
             iterationsPer = iterationsPer,
             bModelTrue    = bModelTrue, 
             wModelTrue    = wModelTrue,
             wModelMis     = wModelMis,
             wModelMis1    = wModelMis1,
             wModelMis2    = wModelMis2,
             wModelMis3    = wModelMis3,
             bModelMis     = bModelMis), 
             "SimParams.rds")

 