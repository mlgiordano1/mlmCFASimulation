# factors for the simulation:
# use even numbers for clusterSize and clusterN
# simulation will automatically make unbalanced have 
# half clusters -15 and half +15
iterationsPer= 150        # number of iterations
clusterSize  = c(100) # use even numbers
clusterN     = c(100) # use even numbers
clusterBal   = c("bal")   # bal and unbal
modelSpec    = c("trueModel") # trueModel and misSpec
distribution = c("normal")               # normal and nonNormal
estimators   = c("FIML", "Goldstein", "Muthen") # FIML, Goldstein, Muthen

# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
makeNewData <- TRUE

# The models we will use
# check to make sure these match the data generating models
bModelTrue <- '
l1b =~ y1+y2+y3+y4
'
wModelTrue <- '
l1w=~y1+y2+y3+y4
'
wModelMis <- '
l1=~y1+y2+y3
l2=~y4+y5+y6
l1~~l2
'

# Mplus Model Generation:
MplusModel <- "
MODEL POPULATION:
  %Within%
  ! Loadings
  L1 by y1@1;
  L1 by y2@.9;
  L1 by y3@.7;
  L1 by y4@.6;
  ! Variance of the factor?
  !L1@1;
  L1@2.5; 
  [L1@0];
  ! Residual variances of indicators
  y1@1;
  y2@1;
  y3@1;
  y4@1;

  %Between%
  L4 by y1@1;
  L4 by y2@.7;
  L4 by y3@.9;
  L4 by y4@.6;
  ! Variance of between factor
  L4@.5;
  [L4@0];
  ! Residual variances of indicators
  y1@.1;
  y2@.2;
  y3@.2;
  y4@.2;


Model:
  %Within%
  ! Loadings
  L1 by y1*1;
  L1 by y2*.9;
  L1 by y3*.7;
  L1 by y4*.6;
  ! Variance of the factor?
  !L1@1;
  L1*2.5; 
  [L1*0];
  ! Residual variances of indicators
  y1*1;
  y2*.6;
  y3*1;
  y4*.9;

  %Between%
  L4 by y1*1;
  L4 by y2*.7;
  L4 by y3*.9;
  L4 by y4*.6;
  ! Variance of between factor
  l4*.7;
  [L4*0];
  ! Residual variances of indicators
  y1*.05;
  y2*.05;
  y3*.05;
  y4*.05;

"


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
                                   # use even numbers for clusterSize and clusterN
                                   # simulation will automatically make unbalanced have 
                                   # half clusters -15 and half +15
                                   clusterSize  = clusterSize ,
                                   clusterN     = clusterN ,
                                   clusterBal   = clusterBal ,
                                   modelSpec    = modelSpec   ,
                                   distribution = distribution,
                                   estimators   = estimators)

# merging the seeds into the between cell factors
temp <- designMatrix[,c("Iteration", "clusterSize", "clusterN", "clusterBal", "distribution")]
temp <- unique(temp)
temp$seed <- seeds[1:nrow(temp)]
designMatrix <- merge(designMatrix, temp)

# compute sample size
designMatrix$sampleSize <- designMatrix$clusterSize*designMatrix$clusterN
 # make DF names
designMatrix$dfName <- paste0(dataDir, "/",
                              designMatrix$clusterBal, "_",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$Iteration,
                              ".dat")
#make rds name
designMatrix$rdsName <- paste0(fitModelDir, "/",
                              designMatrix$clusterBal, "_",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$estimators,
                              designMatrix$Iteration,
                              ".rds")
# create data based on design matrix
if (makeNewData==TRUE) {
  makeDataMplus(wd = baseDir,
                iterations = iterationsPer,
                designMatrix = designMatrix,
                mplusMod = MplusModel)
}

#save Models
saveRDS(list(designMatrix = designMatrix,
             iterationsPer = iterationsPer,
             bModelTrue = bModelTrue, 
             wModelTrue = wModelTrue,
             wModelMis  = wModelMis
             ), "SimParams.rds")

 


