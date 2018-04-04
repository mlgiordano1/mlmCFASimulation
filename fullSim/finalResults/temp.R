rm(list=ls())

baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)
# read in all of the raw mplus
allModels <- readRDS(file = "finalResults/allMplus.Rds")

# souuce the functions
source("../simulationfunctions.R")

#subset just
names(allModels)

sub <- allModels[grep("100_100_bal_2828_true", names(allModels))]
names(sub)




parseMplus(allModels[[1333]])
m <- matrix(nrow = 600)
for (i in seq(sub)) {
  print(i)
  d <- parseMplus(sub[[i]])
  # print(d[["residCov"]])
  try(m[i,1] <- d[["residCov"]])
}
table(m[,1])


d$residCov


