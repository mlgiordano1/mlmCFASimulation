# factors for the simulation:
# use even numbers for clusterSize and clusterN
# simulation will automatically make unbalanced have 
# half clusters -15 and half +15
iterationsPer= 500        # number of iterations
# between cell factors
clusterSize  = c(30, 100) # use even numbers
clusterN     = c(30, 100) # use even numbers
distribution = c("normal")               # normal and nonNormal
clusterBal   = c("bal")   # bal and unbal
# Within cell factors
modelSpec    = c("trueModel", "misSpec", "misSpec1", "misSpec2", "misSpec3") # trueModel and misSpec
estimators   = c("FIML", "Goldstein", "Muthen") # FIML, Goldstein, Muthen

# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
makeNewData <- FALSE

# The models we will use
# check to make sure these match the data generating models
bModelTrue <- '
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

mPlusModelStatement <- "
MODEL POPULATION:
	%Within%
  ! Loadings
  L1 by y1@1;
  L1 by y2@.8;
  L1 by y3@.7;
  L1 by y5@.3;
  L2 by y4@1;
  L2 by y5@.8;
  L2 by y6@.7;
  L2 by y2@.3;
  y2 with y3 @ .3;

	y1-y6*.8;
	L1*2;
  L2*2;

	%Between%
  L4 by y1@1;
  L4 by y2@.7;
  L4 by y3@.6;
  L4 by y4@.8;
  L4 by y5@.7;
  L4 by y6@.8;
	L4*.5;
	y1-y6@.2;

MODEL:
	
	%Within%
  ! Loadings
  L1 by y1*1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4*1;
  L2 by y5*.8;
  L2 by y6*.7;
  L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;

	%Between%
  L4 by y1*1;
  L4 by y2*.7;
  L4 by y3*.6;
  L4 by y4*.8;
  L4 by y5*.7;
  L4 by y6*.8;
	L4*.5;
	y1-y6*.2;

output:
	tech8 tech9;
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
                              # designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$Iteration,
                              ".dat")
#make rds name
designMatrix$rdsName <- paste0(fitModelDir, "/",
                              designMatrix$clusterBal, "_",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$distribution, "_",
                              designMatrix$estimators, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$Iteration,
                              ".rds")

#make inp name
designMatrix$inpName <- paste0(fitModelDir, "/",
                              designMatrix$clusterBal, "_",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$modelSpec, "_",
                              designMatrix$distribution, "_",
                              designMatrix$estimators, "_",
                              designMatrix$Iteration,
                              ".inp")
# create data based on design matrix
if (makeNewData==TRUE) {
  makeDataMplus(mplusModel   = mPlusModelStatement,
                wd           = baseDir,
                iterations   = iterationsPer,
                designMatrix = designMatrix)
}

#save Models
saveRDS(list(designMatrix  = designMatrix,
             iterationsPer = iterationsPer,
             bModelTrue    = bModelTrue, 
             wModelTrue    = wModelTrue,
             wModelMis     = wModelMis,
             wModelMis1    = wModelMis1,
             wModelMis2    = wModelMis2,
             wModelMis3    = wModelMis3,
             mplusModel    = mPlusModelStatement), "SimParams.rds")

 


