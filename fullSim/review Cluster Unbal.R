# install.packages("rsem")
# install.packages('glmnet')
# install.packages('np')
# 
# install.packages("C:/Users/Michael/downloads/MIIVsem_0.5.3.tar", repos = NULL, type = 'source')
library(MIIVsem)
library(tidyverse)

baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)
source("../simulationfunctions.R")


# The data generating matrices for using SemTools
(wLambda        = matrix(c(1.0, 0.0,
                           0.8, 0.3,
                           0.7, 0.0,
                           0.0, 1.0,
                           0.3, 0.8, 
                           0.0, 0.7),
                         nrow  = 6, 
                         byrow = TRUE))
(wPsi           = matrix(c(2.0, 0.3,
                           0.3, 2.0), 
                         nrow = 2, 
                         byrow = TRUE))
wTheta         = matrix(0, nrow = 6, ncol = 6)
diag(wTheta)   = .8
wTheta[2, 3]   = .3
wTheta[3, 2]   = .3
# between matrices
bLambda        = matrix(data = c(1.0, 
                                 0.7, 
                                 0.6, 
                                 0.8, 
                                 0.7, 
                                 0.8), 
                        nrow  = 6, 
                        byrow = TRUE)
bPsi           = matrix(c(.5), nrow = 1, byrow = FALSE)
bTheta         = matrix(0, nrow = 6, ncol = 6)
diag(bTheta)   = .2
# bTheta[6, 5]   = .2
# bTheta[5, 6]   = .2
bTheta

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

nIter <- 100

newVars <- c( "l1.by.y1"   ,
 "l1.by.y1.se",
 "l1.by.y1.sarg",
 "l1.by.y1.sarg.p",
 "l1.by.y2"   ,
 "l1.by.y2.se",
 "l1.by.y2.sarg",
 "l1.by.y2.sarg.p",
 "l1.by.y3"   ,
 "l1.by.y3.se",
 "l1.by.y3.sarg",
 "l1.by.y3.sarg.p",
 "l1.by.y5"   ,
 "l1.by.y5.se",
 "l1.by.y5.sarg",
 "l1.by.y5.sarg.p",
 "l2.by.y4"   ,
 "l2.by.y4.se",
 "l2.by.y4.sarg",
 "l2.by.y4.sarg.p",
 "l2.by.y5"   ,
 "l2.by.y5.se",
 "l2.by.y5.sarg",
 "l2.by.y5.sarg.p",
 "l2.by.y6"   ,
 "l2.by.y6.se",
 "l2.by.y6.sarg",
 "l2.by.y6.sarg.p",
 "l2.by.y2"   ,
 "l2.by.y2.se",
 "l2.by.y2.sarg",
 "l2.by.y2.sarg.p",
 "l1.by.y3"   ,
 "l1.by.y3.se",
 "l1.by.y3.sarg",
 "l1.by.y3.sarg.p",
 "lb.by.y1"   ,
 "lb.by.y1.se",
 "lb.by.y1.sarg",
 "lb.by.y1.sarg.p",
 "lb.by.y2"   ,
 "lb.by.y2.se",
 "lb.by.y2.sarg",
 "lb.by.y2.sarg.p",
 "lb.by.y3"   ,
 "lb.by.y3.se",
 "lb.by.y3.sarg",
 "lb.by.y3.sarg.p",
 "lb.by.y4"   ,
 "lb.by.y4.se",
 "lb.by.y4.sarg",
 "lb.by.y4.sarg.p",
 "lb.by.y5"   ,
 "lb.by.y5.se",
 "lb.by.y5.sarg",
 "lb.by.y5.sarg.p",
 "lb.by.y6"   ,
 "lb.by.y6.se",
 "lb.by.y6.sarg",
 "lb.by.y6.sarg.p")

  indicatorNames = paste0("y", 1:6)
  clusterNo = 100
  clusterSize = 100
  wLambda = wLambda
  wPsi = wPsi
  wTheta = wTheta
  bLambda = bLambda
  bPsi = bPsi 
  bTheta = bTheta
  clusterBal = FALSE
  # save indicator names
  inb <- paste0(indicatorNames, "b")
  inw <- paste0(indicatorNames, "w")
  # create the implied covariance matrices
  wCov <- wLambda%*%wPsi%*%t(wLambda) + wTheta
  bCov <- bLambda%*%bPsi%*%t(bLambda) + bTheta


results <- matrix(nrow = nIter, ncol = (length(newVars) +1))
colnames(results) <- c("rdsName", newVars)

i = 1
for (i in seq(nIter)) {
  print(i)
  # --------------------------------------------------------------------------
  # Generate Data
  # --------------------------------------------------------------------------


  # # Generate the within data
  # # if no skew or kurtosis specified for within use mass::mvrnorm
  #   dfW <- MASS::mvrnorm(n        = clusterNo*clusterSize,
  #                        mu       = rep(0, length(indicatorNames)),
  #                        Sigma    = wCov)
  #   dfB <- MASS::mvrnorm(n        = clusterNo,
  #                        mu       = rep(0, length(indicatorNames)),
  #                        Sigma    = bCov)
  # # adding column names
  # colnames(dfW) <- inw
  # colnames(dfB) <- inb
  # # make vector of ids
  # id <- 1:(clusterNo*clusterSize)
  # # make vector of clusters
  # if (clusterBal==TRUE) {
  #   cluster <- rep(1:clusterNo, clusterSize)
  # } else {
  #   cluster <- c(rep(1:(clusterNo/2), clusterSize-15),
  #                rep(((clusterNo/2)+1):clusterNo, clusterSize+15))
  # }
  # dfw <- as.data.frame(dfW)
  # dfB <- as.data.frame(dfB)
  # dfB$cluster <- (1:clusterNo)
  # df <- cbind(dfW,
  #             cluster,
  #             id)
  # df <- as.data.frame(df)
  # df <-  merge(df, dfB, by = "cluster")
  # # make df a dataframe
  # df <- as.data.frame(df)
  # # create new vars
  # for (i2 in seq(indicatorNames)) {
  #   df[, indicatorNames[i2]] <- df[, inw[i2]] + df[, inb[i2]]
  # }
  # # subset just the vars we want
  # df <- df[, c("id", "cluster", indicatorNames)]

df <- simData2(paste0("y", 1:6),
         clusterNo = 100,
         clusterSize = 100,
         wLambda = wLambda,
         wPsi = wPsi,
         wTheta = wTheta,
         bLambda = bLambda,
         bPsi = bPsi,
         bTheta = bTheta,
         clusterBal = FALSE)
table(df$cluster)


fit <- mlcfaMIIV(withinModel = wModelTrue, 
          betweenModel = bModelTrue, 
          estimator = "muthen", 
          allIndicators = paste0("y", 1:6), 
          l1Var = "id", 
          l2Var = "cluster", 
          df = df)
fit$within

wTable <- MIIVsem::estimatesTable(fit$within, sarg = TRUE)
#     bTable <- MIIVsem::estimatesTable(mod$between, sarg = TRUE)
wTable$eqn <- paste0(wTable$lhs, wTable$op, wTable$rhs)
#     bTable$eqn <- paste0(bTable$lhs, bTable$op, bTable$rhs)
#     
	# within
    try(results[i, "l1.by.y1"]        <- wTable[wTable$eqn=="l1=~y1", "est"])
    try(results[i, "l1.by.y1.se"]     <- wTable[wTable$eqn=="l1=~y1", "se"])
    try(results[i, "l1.by.y1.sarg"]   <- wTable[wTable$eqn=="l1=~y1", "sarg"])
    try(results[i, "l1.by.y1.sarg.p"] <- wTable[wTable$eqn=="l1=~y1", "sarg.p"])
    try(results[i, "l1.by.y2"]        <- wTable[wTable$eqn=="l1=~y2", "est"])
    try(results[i, "l1.by.y2.se"]     <- wTable[wTable$eqn=="l1=~y2", "se"])
    try(results[i, "l1.by.y2.sarg"]   <- wTable[wTable$eqn=="l1=~y2", "sarg"])
    try(results[i, "l1.by.y2.sarg.p"] <- wTable[wTable$eqn=="l1=~y2", "sarg.p"])
    try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
    try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
    try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
    try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
    try(results[i, "l1.by.y5"]        <- wTable[wTable$eqn=="l1=~y5", "est"])
    try(results[i, "l1.by.y5.se"]     <- wTable[wTable$eqn=="l1=~y5", "se"])
    try(results[i, "l1.by.y5.sarg"]   <- wTable[wTable$eqn=="l1=~y5", "sarg"])
    try(results[i, "l1.by.y5.sarg.p"] <- wTable[wTable$eqn=="l1=~y5", "sarg.p"])
    try(results[i, "l2.by.y4"]        <- wTable[wTable$eqn=="l2=~y4", "est"])
    try(results[i, "l2.by.y4.se"]     <- wTable[wTable$eqn=="l2=~y4", "se"])
    try(results[i, "l2.by.y4.sarg"]   <- wTable[wTable$eqn=="l2=~y4", "sarg"])
    try(results[i, "l2.by.y4.sarg.p"] <- wTable[wTable$eqn=="l2=~y4", "sarg.p"])
    try(results[i, "l2.by.y5"]        <- wTable[wTable$eqn=="l2=~y5", "est"])
    try(results[i, "l2.by.y5.se"]     <- wTable[wTable$eqn=="l2=~y5", "se"])
    try(results[i, "l2.by.y5.sarg"]   <- wTable[wTable$eqn=="l2=~y5", "sarg"])
    try(results[i, "l2.by.y5.sarg.p"] <- wTable[wTable$eqn=="l2=~y5", "sarg.p"])
    try(results[i, "l2.by.y6"]        <- wTable[wTable$eqn=="l2=~y6", "est"])
    try(results[i, "l2.by.y6.se"]     <- wTable[wTable$eqn=="l2=~y6", "se"])
    try(results[i, "l2.by.y6.sarg"]   <- wTable[wTable$eqn=="l2=~y6", "sarg"])
    try(results[i, "l2.by.y6.sarg.p"] <- wTable[wTable$eqn=="l2=~y6", "sarg.p"])
    try(results[i, "l2.by.y2"]        <- wTable[wTable$eqn=="l2=~y2", "est"])
    try(results[i, "l2.by.y2.se"]     <- wTable[wTable$eqn=="l2=~y2", "se"])
    try(results[i, "l2.by.y2.sarg"]   <- wTable[wTable$eqn=="l2=~y2", "sarg"])
    try(results[i, "l2.by.y2.sarg.p"] <- wTable[wTable$eqn=="l2=~y2", "sarg.p"])
    try(results[i, "l1.by.y3"]        <- wTable[wTable$eqn=="l1=~y3", "est"])
    try(results[i, "l1.by.y3.se"]     <- wTable[wTable$eqn=="l1=~y3", "se"])
    try(results[i, "l1.by.y3.sarg"]   <- wTable[wTable$eqn=="l1=~y3", "sarg"])
    try(results[i, "l1.by.y3.sarg.p"] <- wTable[wTable$eqn=="l1=~y3", "sarg.p"])
    # try(results[i, "lb.by.y1"]        <- bTable[wTable$eqn=="l1=~y1", "est"])
    # try(results[i, "lb.by.y1.se"]     <- bTable[wTable$eqn=="l1=~y1", "se"])
    # try(results[i, "lb.by.y1.sarg"]   <- bTable[wTable$eqn=="l1=~y1", "sarg"])
    # try(results[i, "lb.by.y1.sarg.p"] <- bTable[wTable$eqn=="l1=~y1", "sarg.p"])
#     # between
#     try(results[i, "lb.by.y2"]        <- bTable[bTable$eqn=="l1=~y2", "est"])
#     try(results[i, "lb.by.y2.se"]     <- bTable[bTable$eqn=="l1=~y2", "se"])
#     try(results[i, "lb.by.y2.sarg"]   <- bTable[bTable$eqn=="l1=~y2", "sarg"])
#     try(results[i, "lb.by.y2.sarg.p"] <- bTable[bTable$eqn=="l1=~y2", "sarg.p"])
#     try(results[i, "lb.by.y3"]        <- bTable[bTable$eqn=="l1=~y3", "est"])
#     try(results[i, "lb.by.y3.se"]     <- bTable[bTable$eqn=="l1=~y3", "se"])
#     try(results[i, "lb.by.y3.sarg"]   <- bTable[bTable$eqn=="l1=~y3", "sarg"])
#     try(results[i, "lb.by.y3.sarg.p"] <- bTable[bTable$eqn=="l1=~y3", "sarg.p"])
#     try(results[i, "lb.by.y4"]        <- bTable[bTable$eqn=="l1=~y4", "est"])
#     try(results[i, "lb.by.y4.se"]     <- bTable[bTable$eqn=="l1=~y4", "se"])
#     try(results[i, "lb.by.y4.sarg"]   <- bTable[bTable$eqn=="l1=~y4", "sarg"])
#     try(results[i, "lb.by.y4.sarg.p"] <- bTable[bTable$eqn=="l1=~y4", "sarg.p"])
#     try(results[i, "lb.by.y5"]        <- bTable[bTable$eqn=="l1=~y5", "est"])
#     try(results[i, "lb.by.y5.se"]     <- bTable[bTable$eqn=="l1=~y5", "se"])
#     try(results[i, "lb.by.y5.sarg"]   <- bTable[bTable$eqn=="l1=~y5", "sarg"])
#     try(results[i, "lb.by.y5.sarg.p"] <- bTable[bTable$eqn=="l1=~y5", "sarg.p"])
#     try(results[i, "lb.by.y6"]        <- bTable[bTable$eqn=="l1=~y6", "est"])
#     try(results[i, "lb.by.y6.se"]     <- bTable[bTable$eqn=="l1=~y6", "se"])
#     try(results[i, "lb.by.y6.sarg"]   <- bTable[bTable$eqn=="l1=~y6", "sarg"])
#     try(results[i, "lb.by.y6.sarg.p"] <- bTable[bTable$eqn=="l1=~y6", "sarg.p"])
#     
}

results <- as.data.frame(results)
# convert all to character
results[, ] <- lapply(results[, ], as.character)
# give this function the subset you want and have it process the df
processDF <- function(df) {
  # make it long
  df <- gather(df, parameter, est, l1.by.y1:lb.by.y6.sarg.p, factor_key = FALSE)
  df$est <- as.numeric(df$est)
  # put in true values
  df[df$parameter=="l1.by.y1", "true"] <- 1
  df[df$parameter=="l1.by.y2", "true"] <- .8
  df[df$parameter=="l1.by.y3", "true"] <- .7
  df[df$parameter=="l2.by.y4", "true"] <- 1
  df[df$parameter=="l2.by.y5", "true"] <- .8
  df[df$parameter=="l2.by.y6", "true"] <- .7
  df[df$parameter=="l1.by.y5", "true"] <- .3
  df[df$parameter=="l2.by.y2", "true"] <- .3
  # tru values for between parameters
  df[df$parameter=="lb.by.y2", "true"] <- .7
  df[df$parameter=="lb.by.y3", "true"] <- .6
  df[df$parameter=="lb.by.y4", "true"] <- .8
  df[df$parameter=="lb.by.y5", "true"] <- .7
  df[df$parameter=="lb.by.y6", "true"] <- .8
  # Remove the scaling indicators
  df <- df[which(df$parameter!="l1.by.y1"),]
  df <- df[which(df$parameter!="l2.by.y4"),]
  df <- df[which(df$parameter!="lb.by.y1"),]
  # compute relative bias 
  df$p_relBias <- ((df$est - df$true) / df$true)*100
  # make a cluster var
  df$cluster <- as.factor(paste0(df$clusterN,"groups_of_N", df$clusterSize))
  # getting the order right
  try(df$cluster <- relevel(df$cluster, " 30groups_of_N 30"))
  try(df$cluster <- relevel(df$cluster, " 30groups_of_N100"))
  try(df$cluster <- relevel(df$cluster, "100groups_of_N 30"))
  try(df$cluster <- relevel(df$cluster, "100groups_of_N100"))
  return(df)
}


dfLong <- processDF(results)

wl <-c("l1.by.y1",
       "l1.by.y2",
       "l1.by.y3",
       "l1.by.y5",
       "l2.by.y4",
       "l2.by.y5",
       "l2.by.y6",
       "l2.by.y2")
ggplot(dfLong[dfLong$parameter %in% wl, ], aes(x=parameter, y = p_relBias)) +
          geom_boxplot() +
          # facet_grid(.~clusterSize + clusterN) +
          # facet_grid(.~cluster, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE) +  
          stat_summary(fun.y = mean, geom="point",colour="darkred", size=1, 
            position = position_dodge(width = .75)) +
          geom_hline(yintercept = 20) + 
          geom_hline(yintercept = -20) + 
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          coord_cartesian(ylim=c(-100, 100)) +
          scale_y_continuous(breaks = seq(from = -100, to = 100, by = 20)) +
          theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5),
                legend.position = 'none') +
          ylab("Percent Relative Bias") +
          xlab("Within Loadings")
