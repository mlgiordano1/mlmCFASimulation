baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/presentationSim"
setwd(baseDir)


mplusBModel <-'
l3 by y1-y6;
'

mpluswModelTrue <- '
l1 by y1-y3 y5;
l2 by y4-y6 y2;
y2 with y3;
l1 with l2;
'
mpluswModelMis <- '
l1 by y1-y3;
l2 by y4-y6;
'
mpluswModelMis1 <- '
l1 by y1-y3
l2 by y4-y6 y2;
y2 with y3;
l1 with l2;
'
mpluswModelMis2 <- '
l1 by y1-y3 y5;
l2 by y4-y6
y2 with y3;
l1 with l2;
'
mpluswModelMis3 <- '
l1 by y1-y3 y5;
l2 by y4-y6 y2;
! missing correlated error
l1 with l2;
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
