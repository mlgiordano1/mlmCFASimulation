rm(list = ls())

baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)


mplusBModel <-'
  L4 by y1@1;
  L4 by y2*.7;
  L4 by y3*.6;
  L4 by y4*.8;
  L4 by y5*.7;
  L4 by y6*.8;
	L4*.5;
	y1-y6*.2;
'

mpluswModelTrue <- '
  L1 by y1@1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4@1;
  L2 by y5*.8;
  L2 by y6*.7;
  L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
  l1 with l2*.3;
'
mpluswModelMis <- '
  L1 by y1@1;
  L1 by y2*.8;
  L1 by y3*.7;
  ! L1 by y5*.3;
  L2 by y4@1;
  L2 by y5*.8;
  L2 by y6*.7;
  ! L2 by y2*.3;
  ! y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis1 <- '
  L1 by y1@1;
  L1 by y2*.8;
  L1 by y3*.7;
  ! L1 by y5*.3;
  L2 by y4@1;
  L2 by y5*.8;
  L2 by y6*.7;
  L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis2 <- '
  L1 by y1@1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4@1;
  L2 by y5*.8;
  L2 by y6*.7;
  ! L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis3 <- '
  L1 by y1@1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4@1;
  L2 by y5*.8;
  L2 by y6*.7;
  L2 by y2*.3;
  ! y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'

# reading in the parameters of the model
simParams     <- readRDS("SimParams.rds")
designMatrix  <- simParams$designMatrix
iterationsPer <- simParams$iterationsPer
wModelTrue    <- simParams$wModelTrue
wModelMis     <- simParams$wModelMis
wModelMis1    <- simParams$wModelMis1
wModelMis2    <- simParams$wModelMis2
wModelMis3    <- simParams$wModelMis3
bModelTrue    <- simParams$bModelTrue

# subset just the estimator we want
designMatrix <- designMatrix[which(designMatrix$estimators=="FIML"),]

for (i in seq(nrow(designMatrix))) { # startingPoint!
  print(paste0("writing .inp file ", i, "/", nrow(designMatrix)))
  # Save the model we will be using
  if (designMatrix$modelSpec[i] == "trueModel") {
    wModel <- mpluswModelTrue
    bModel <- mplusBModel
  }  else if (designMatrix$modelSpec[i] == "misSpec") {
    wModel <- mpluswModelMis
    bModel <- mplusBModel
  } else if (designMatrix$modelSpec[i] == "misSpec1") {
    wModel <- mpluswModelMis1
    bModel <- mplusBModel
  } else if (designMatrix$modelSpec[i] == "misSpec2") {
    wModel <- mpluswModelMis2
    bModel <- mplusBModel
  } else if (designMatrix$modelSpec[i] == "misSpec3") {
    wModel <- mpluswModelMis3
    bModel <- mplusBModel
  }
  
  # write inp file
  writeLines(c('TITLE:	"This is my Title"',
               paste0('DATA: FILE = \'../', designMatrix$datName[i], "';"),
               'Variable: NAMES ARE id cluster y1 y2 y3 y4 y5 y6;',
                'USEVARIABLE = cluster y1 y2 y3 y4 y5 y6;',
               'MISSING=.;',
                 "Cluster = Cluster",
                 "ANALYSIS:	TYPE = TWOLEVEL;",
                 "Model:",
                 "%within%",
                 wModel,
                 "%BETWEEN%",
                 bModel),
                 con = designMatrix$inpName[i])
}

setwd("./savedModels" )
MplusAutomation::runModels()
