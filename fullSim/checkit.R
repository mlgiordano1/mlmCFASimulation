rm(list=ls())
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


sub <- designMatrix[which(designMatrix$skewKurt=="0000"), ]

sub[1,]
i=7
print(sub$seed[i])
sub$wSkew[i]

df <- readRDS(sub$dfName[i])
mean(df$y1)
mean(df$y2)
mean(df$y3)
mean(df$y4)

hist(df$y1)
hist(df$y2)
hist(df$y3)
hist(df$y4)
hist(df$y5)
hist(df$y6)
install.packages("moments")
library(moments)
skewness(df$y1)
kurtosis(df$y1)

n <- paste0("y", 1:6)
for (i in seq(n)) {
  print(paste0("Skewness: ",    skewness(df[,n[i]]), 
               ":: Kurtosis: ", kurtosis(df[,n[i]])))
}
