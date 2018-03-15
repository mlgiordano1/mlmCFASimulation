# factors for the simulation:
# use even numbers for clusterSize and clusterN
# simulation will automatically make unbalanced have half clusters -15 and half +15
iterationsPer= 500        # number of iterations per between cell condition
# between cell factors
clusterSize  = c(100)     # use even numbers
clusterN     = c(100)     # use even numbers
clusterBal   = c("bal")   # bal and unbal
distribution = c("normal")               # normal and nonNormal

wSkew        = c(0, 2)
wKurt        = c(0, 3)
bSkew        = c(0, 2)
bKurt        = c(0, 3)
# Within cell factors
modelSpec    = c("trueModel") # trueModel and misSpec
# modelSpec    = c("trueModel", "misSpec", "misSpec1", "misSpec2", "misSpec3") # trueModel and misSpec
estimators   = c("FIML", "Muthen") # FIML, Goldstein, Muthen

# Create a base directory on your own
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
makeNewData <- FALSE

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


# the data fitting models
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
                                   distribution = distribution,
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

# merging the seeds into the between cell factors
btwCell <- designMatrix[,c("Iteration", 
                        "clusterSize", 
                        "clusterN", 
                        "clusterBal", 
                        "distribution", 
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
                         btwCell$distribution, "_",
                         btwCell$wSkew, btwCell$wKurt,
                         btwCell$bSkew, btwCell$bKurt, "_",
                         btwCell$Iteration,
                         ".rds")
designMatrix <- merge(designMatrix, btwCell)

# compute sample size
designMatrix$sampleSize <- designMatrix$clusterSize*designMatrix$clusterN
# make DF names

#make rds name
designMatrix$rdsName <- paste0(fitModelDir, "/",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$clusterBal, "_",
                              designMatrix$distribution, "_",
                              designMatrix$wSkew, designMatrix$wKurt,
                              designMatrix$bSkew, designMatrix$bKurt, "_",
                              designMatrix$Iteration,
                              ".rds")

#make inp name
designMatrix$inpName <- paste0(fitModelDir, "/",
                              designMatrix$clusterSize, "_",
                              designMatrix$clusterN, "_",
                              designMatrix$clusterBal, "_",
                              designMatrix$distribution, "_",
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
    print(i)
    if (btwCell$clusterBal[i]=="bal") {
      bal <- TRUE
    } else {
      bal <- FALSE
    }
    df <- simData(indicatorNames = paste0("y", 1:6),
                  withinModel    = wGenModel, 
                  betweenModel   = bGenModel, 
                  clusterNo      = btwCell$clusterN[i],
                  clusterSize    = btwCell$clusterSize[i],
                  wSkew          = btwCell$wSkew[i],
                  wKurt          = btwCell$wKurt[i], 
                  bSkew          = btwCell$bSkew[i],
                  bKurt          = btwCell$bKurt[i],
                  clusterBal     = bal,
                  seed           = btwCell$seed[i])
    saveRDS(df, file = btwCell$dfName[i])
    
  }
}


#save Models
saveRDS(list(designMatrix  = designMatrix,
             iterationsPer = iterationsPer,
             bModelTrue    = bModelTrue, 
             wModelTrue    = wModelTrue,
             wModelMis     = wModelMis,
             wModelMis1    = wModelMis1,
             wModelMis2    = wModelMis2,
             wModelMis3    = wModelMis3), 
             "SimParams.rds")

 


