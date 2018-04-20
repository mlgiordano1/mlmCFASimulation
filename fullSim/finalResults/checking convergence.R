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

# read in design matrix
simParams     <- readRDS("SimParams.rds")
dm  <- simParams$designMatrix
dm$outName <- tolower(paste0(substr(dm$inpName, start = 13, stop = nchar(dm$inpName)-4), ".out"))
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# true model etc
s <- allMplusModels[grep("_bal_0000_tru" , names(allMplusModels))]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# misspec1
s <- allMplusModels[grep("_bal_0000_misspecw1" , names(allMplusModels), ignore.case = TRUE)]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# misspecw2
s <- allMplusModels[grep("_bal_0000_misspecw2" , names(allMplusModels), ignore.case = TRUE)]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# misspecw3
s <- allMplusModels[grep("_bal_0000_misspecw3" , names(allMplusModels), ignore.case = TRUE)]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# unbal etc
s <- allMplusModels[grep("_unbal_0000_tru" , names(allMplusModels))]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# 2800
s <- allMplusModels[grep("_bal_" , names(allMplusModels))]
s <- s[grep("_tru" , names(s))]
s <- s[grep("2800" , names(s))]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# 0028
s <- allMplusModels[grep("_bal_" , names(allMplusModels))]
s <- s[grep("_tru" , names(s))]
s <- s[grep("0028" , names(s))]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# 2828
s <- allMplusModels[grep("_bal_" , names(allMplusModels))]
s <- s[grep("_tru" , names(s))]
s <- s[grep("2828" , names(s))]
mat <- matrix(ncol = 9 , nrow = length(s))
colnames(mat) <- c("name","cn", "cs", "cn_cs", "normal", "nonid", "nonpos", "saddle", "singular" )
mat[,"name"] <- names(s)
mat <- mat[order(mat[,"name"]), ]
dm1 <- dm[dm$outName %in% names(s),]
dm1 <- dm1[order(dm1$outName),]
mat[,"cs"] <- dm1$clusterSize
mat[,"cn"] <- dm1$clusterN
mat[,"cn_cs"] <- paste0(mat[,"cn"], "_", mat[,"cs"])
for (i in seq(nrow(mat))) {
  print(i)
  # subest one model
  mod <- s[[i]]
  # terminated normall
  mat[i, "normal"] <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", mod, ignore.case = TRUE))
  mat[i, "nonid"] <- any(grepl("NONIDENTIFICATION IS MOST LIKELY DUE TO HAVING MORE PARAMETERS", mod, ignore.case = TRUE))
  mat[i, "nonpos"] <- any(grepl("NON-POSITIVE DEFINITE", mod, ignore.case = TRUE))
  mat[i, "saddle"] <- any(grepl("saddle", mod, ignore.case = TRUE))
  mat[i, "singular"] <- any(grepl("saddle", mod, ignore.case = TRUE)) 
}
print(paste0("There was ", 
             table(mat[,"normal"])[1], 
             " non-converged solution"))
(m <- subset(mat, mat[,"normal"]==FALSE))
table(mat[, "normal"], mat[,"cn_cs"])




names(s)
str(mod)
names(allMplusModels)
library(mgcode)

MplusErrors()



unbal <- readRDS("./finalResults/unbalance.RDS")
# pick a specific model
mod <- s[[grep("30_30_unbal_0000_truemodel_fiml_373", names(s))]]
mod
