rm(list=ls())
setwd("c:/users/mgiordan/git/mlmcfasimulation/fullsim/varcovfix")
divideBy <- 100       # how many files would you like to split it into

# read in the designMatrix to determine number of iterations per estimator
dm <- readRDS("simparams.rds")
totalIter <- nrow(dm$designMatrix)/3 # how many iterations does your simulation have
nPer <- ceiling(totalIter/divideBy)  # the number of iterations per
start1 <- 1
stop1 <- nPer
i <- 1
while (start1 <= totalIter) {
print(start1)
  #print(paste0(start1, " through ", stop1))


  start1 <- stop1 + 1
  stop1 <- start1+nPer-1
  i = i+1
}
