baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
setwd(baseDir)


mplusBModel <-'
  L4 by y1*1;
  L4 by y2*.7;
  L4 by y3*.6;
  L4 by y4*.8;
  L4 by y5*.7;
  L4 by y6*.8;
	L4*.5;
	y1-y6*.2;
'

mpluswModelTrue <- '
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
'
mpluswModelMis <- '
  L1 by y1*1;
  L1 by y2*.8;
  L1 by y3*.7;
  ! L1 by y5*.3;
  L2 by y4*1;
  L2 by y5*.8;
  L2 by y6*.7;
  ! L2 by y2*.3;
  ! y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis1 <- '
  L1 by y1*1;
  L1 by y2*.8;
  L1 by y3*.7;
  ! L1 by y5*.3;
  L2 by y4*1;
  L2 by y5*.8;
  L2 by y6*.7;
  L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis2 <- '
  L1 by y1*1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4*1;
  L2 by y5*.8;
  L2 by y6*.7;
  ! L2 by y2*.3;
  y2 with y3 * .3;

	y1-y6*.8;
	L1*2;
  L2*2;
'
mpluswModelMis3 <- '
  L1 by y1*1;
  L1 by y2*.8;
  L1 by y3*.7;
  L1 by y5*.3;
  L2 by y4*1;
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
  
  writeLines(c('TITLE:	"This is my Title"',
                 paste0("Data: file is ../", designMatrix$dfName[i], ";"),
                 "VARIABLE:	NAMES ARE y1-y6 clust;",
                 "Cluster = Clust",
                 "ANALYSIS:	TYPE = TWOLEVEL;",
                 "Model:",
                 "%within%",
                 wModel,
                 "%BETWEEN%",
                 bModel),
              con = designMatrix$inpName[i])
}

# MplusAutomation::runModels("./savedModels" )
