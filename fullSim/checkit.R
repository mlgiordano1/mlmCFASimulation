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


i=300
print(designMatrix$seed[i])
df <- readRDS(designMatrix$dfName[i])
mean(df$y1)
mean(df$y2)
mean(df$y3)
mean(df$y4)
