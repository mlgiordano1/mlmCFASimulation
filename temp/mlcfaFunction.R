# withinModel = wModel
# betweenModel = bModel
# estimator = "muthen"
# allIndicators = indicators
# l1Var = "id"
# l2Var = "cluster"
# df = myData
# 
# install.packages("MIIVsem")

# return within and between models
mlcfaMIIV <- function(withinModel,
                      betweenModel,
                      estimator = "muthen",
                      allIndicators,
                      l1Var,
                      l2Var,
                      df) {
  # Program some checks, like is long a DF, fitWith =nlme or lmer, 
  # all indicators is charater, etc.
  
  # -----------------------------------------------------------------
  # Do some universal steps
  # number of subjects
  n <- nrow(df) 
  # number of groups
  g <- length(unique(df[[l2Var]]))
  # -----------------------------------------------------------------

    
  # -----------------------------------------------------------------
  # Process the data
  if (estimator == "muthen") {
    # decompose muthen style
    covMats <- decompMuthen(allIndicators, l1Var, l2Var, df, n=n, g=g)
  } else if (estimator == "goldstein") {
    # decompose muthen style
    covMats <- decompGoldstein(allIndicators, l1Var, l2Var, df)
  }


  
  # -----------------------------------------------------------------
  # fit with MIIVsem
  # Fit covariance matrices with MIIVsem
  w <- MIIVsem::miive(withinModel,  
                      sample.cov = covMats[["within"]], 
                      sample.nobs = n)
  b <- MIIVsem::miive(betweenModel,
                      sample.cov = covMats[["between"]],
                      sample.nobs = g)
  # return the list of within and between models
  return(list(within=w, between=b))
} # end function
  
  
decompGoldstein <- function(allIndicators,
                            l1Var,
                            l2Var,
                            df) {
  # make long (necessary for fitting in nlme)
  long <- reshape2::melt(df, id.vars = c(l1Var, l2Var), variable.name = "item")
  # dummy code the indicators
  for (level in unique(long$item)) {
    long[level] <- ifelse(long$item == level, 1, 0)
  }
  # save combinations of indicators
  combinations <- t(combn(allIndicators, m=2))
  # create the matrices 
  wCovMat <- matrix(nrow=length(allIndicators), ncol=length(allIndicators), 
                    dimnames = list(allIndicators, allIndicators)) 
  bCovMat <- matrix(nrow=length(allIndicators), ncol=length(allIndicators), 
                    dimnames = list(allIndicators, allIndicators))
  # loop through all bivariate combinations. for each fit and pull out covariances with getVCE()
  for (i in seq(nrow(combinations))) {
    # indicators in question
    i1 <- combinations[i,1]
    i2 <- combinations[i,2]
    # Create the model and random statements
    form <- paste(i1, i2 , sep = "+" )
    model <- as.formula(paste0("value~ -1 +", form))
    ranef <- as.formula(paste("~-1 + ", form, "|cluster/id"))
    # subset the rows we want
    subset <- dplyr::filter(.data = long, item %in% c(i1, i2))
    # Try catch incase models do not converge
    tryCatch({
      print(i)
      fit <- nlme::lme(fixed   = model, 
                            random  = ranef, 
                            data    = subset, 
                            method  = "REML",
                            control = nlme::lmeControl(opt='optim'))
      covMats <- getVCE(fit, names = c(i1, i2))
      # fill in the larger 
      wCovMat[i1, i2] <- covMats$within[i1, i2]
      wCovMat[i1, i1] <- covMats$within[i1, i1]
      wCovMat[i2, i2] <- covMats$within[i2, i2]
      wCovMat[i2, i1] <- covMats$within[i2, i1]
      # fill in the larger between
      bCovMat[i1, i2] <- covMats$between[i1, i2]
      bCovMat[i1, i1] <- covMats$between[i1, i1]
      bCovMat[i2, i2] <- covMats$between[i2, i2]
      bCovMat[i2, i1] <- covMats$between[i2, i1]
    }, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  } # end of for loop making covariance matrices
  return(list(within=wCovMat, between=bCovMat))  
} # end of the function


getVCE <- function(twoLvlnlmeObj, names) {
  # --------------------------------
  # do some checks?
  # --------------------------------
  # Save varcorr output
  v               <- nlme::VarCorr(twoLvlnlmeObj)
  # pull the between variances and save them on the diag
  b.cov           <- diag(v[2:3,1])
  # compute the covariance
  b.cov[2,1]      <- sqrt(b.cov[1,1])*sqrt(b.cov[2,2])*as.numeric(v[3,3])
  b.cov[1,2]      <- sqrt(b.cov[1,1])*sqrt(b.cov[2,2])*as.numeric(v[3,3])
  colnames(b.cov) <- names
  rownames(b.cov) <- names
  # now for the within
  w.cov           <- diag(v[5:6,1])
  w.cov[2,1]      <- sqrt(w.cov[1,1])*sqrt(w.cov[2,2])*as.numeric(v[6,3])
  w.cov[1,2]      <- sqrt(w.cov[1,1])*sqrt(w.cov[2,2])*as.numeric(v[6,3])
  colnames(w.cov) <- names
  rownames(w.cov) <- names
  # compile two matices in a list
  return(list(within=w.cov, between=b.cov))
}




decompMuthen <- function(allIndicators,
                            l1Var,
                            l2Var,
                            df,
                            n,
                            g) {
  #-------------------------------------------------------------------
  # some general setup
  # ------------------------------------------------------------------
  grpMnNames <- paste(allIndicators, "_gmn", sep = "")
  mnNames    <- paste(allIndicators, "_mn" , sep = "")
  grpSizeNames <- paste(allIndicators, "_n" , sep = "")
  # creating an aggregated data set (with group means and group sizes)
  agg <- cbind(aggregate(df[allIndicators], 
                        by = list(cluster = df[[l2Var]]), 
                        FUN = mean), 
                aggregate(df[l2Var], 
                          by = list(cluster = df[[l2Var]]), 
                          FUN = length)
  )
  # dropping the repeat "cluster" var
  agg <- agg[,-(ncol(agg)-1)]
  # adding names
  names(agg) <- c("cluster", grpMnNames, "grpSize")
  
  #-------------------------------------------------------------------
  # within covariance matrices
  # ------------------------------------------------------------------
  # merge agg with full df
  df <- merge(df, agg, by = l2Var)
  # Create difference matrix
  d <- as.matrix(df[allIndicators]-df[grpMnNames])
  # Create covariance matrix
  wCovMat <- ((1/(n-g-1)) * t(d) %*% d)
  
  #-------------------------------------------------------------------
  # between covariance matrices
  # ------------------------------------------------------------------
  # average cluster size
  nMn <- mean(agg[['grpSize']])
  # save overall average for each indicator
  for (i in seq(allIndicators)) {
    agg <- cbind(agg, mean(df[[allIndicators[i]]]))
  }
  names(agg) <- c("cluster", grpMnNames, "grpSize", mnNames)
  # difference matrix
  d <- as.matrix(agg[mnNames]-agg[grpMnNames])
  # covariance matrix
  bCovMat <- ((nMn/(g-1)) * t(d) %*% d)
  # names
  colnames(bCovMat)  <- allIndicators
  rownames(bCovMat)  <- allIndicators
  # return
  return(list(within=wCovMat, between=bCovMat)) 

}
