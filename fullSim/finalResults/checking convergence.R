rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(MIIVsem)



baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)

source("../SimulationFunctions.R")

allMplusModels <- readRDS("finalResults/allMplus.Rds")
s <- allMplusModels[grep("unbal_0000_tru" , names(allMplusModels))]


i=1
mat <- matrix(ncol = 6, nrow = length(s))
colnames(mat) <- c("name", "normal", "nonid", "nonpos", "saddle", "singular" )


for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # save the model name
  mat[i, "name"] <- names(s)[i]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}

table(mat[,"nonid"], mat[,"nonpos"])
table(mat[,"saddle"], mat[,"singular"])
table(mat[,"singular"])

"30_30_unbal_0000_truemodel_fiml_373"

names(s)
str(mod)
names(allMplusModels)
library(mgcode)

MplusErrors()



unbal <- readRDS("./finalResults/unbalance.RDS")
# pick a specific model
mod <- s[[grep("30_30_unbal_0000_truemodel_fiml_373", names(s))]]
mod
