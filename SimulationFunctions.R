
# Create Directory Structure
# only run this once because it will overwrite everything else
createDirStr <- function(baseDir) {
  if (!dir.exists(path=paste0(baseDir, "/rawData"))) {
    dir.create(path=paste0(baseDir, "/rawData"))
    print("created raw data directory")
  } else {
    print("Raw Data directory already exists")
  }
  if (!dir.exists(path=paste0(baseDir, "/savedModels"))) {
    dir.create(path=paste0(baseDir, "/savedModels"))
    print("Created savedModels directory")
  } else {
    print("savedModels directory already exists")
  }
  if (!dir.exists(path=paste0(baseDir, "/finalResults"))) {
    dir.create(path=paste0(baseDir, "/finalResults"))
    print("Created finalResults directory")
  } else {
    print("finalResults directory already exists")
  }
}

createDesignMatrix <- function(nIter,
                               clusterSize,
                               clusterN,
                               clusterBal,
                               distribution,
                               wSkew = 0,
                               wKurt = 0, 
                               bSkew = 0, 
                               bKurt = 0,
                               #within factors
                               modelSpec,
                               estimators) {
  # setup the Matrix
  localMat <- expand.grid(seq(nIter),
                          clusterSize,
                          clusterN,
                          clusterBal,
                          distribution,
                          wSkew,
                          wKurt,
                          bSkew,
                          bKurt,
                          # within factors
                          modelSpec,
                          estimators, 
                          stringsAsFactors = FALSE)
  # name the columns will make it easier to pull
  names(localMat) <- c("Iteration",
                      "clusterSize",
                      "clusterN",
                      "clusterBal",
                      "distribution",
                      "wSkew",
                      "wKurt",
                      "bSkew",
                      "bKurt",
                      # within 
                      "modelSpec",
                      "estimators")
  return(localMat)
}

makeDataMplus <- function(mplusModel, wd, iterations, designMatrix) {
  # get unique conditions
  dm <- designMatrix[c("sampleSize",
                       "clusterSize",
                       "clusterN",
                       "clusterBal",
                       # "modelSpec",
                       "distribution")]
  dm <- unique(dm)
  # data directory
  dataDir <- paste0(wd, "/rawData")
  # save mplusGenFile
  fileName1 <- paste0(dataDir, "/temp.inp")
  fileName2 <- paste0(dataDir, "/temp.out")
  
  # loop through the various combinations to make the data
for (i in seq(nrow(dm))) {
  title <- "
! DO NOT EDIT IN TEXT. EDIT IN PRIMARY SIMULATION CODE r FILE. 
TITLE: Sim

montecarlo:
names are y1-y6 ;"

sampleSize <- paste("nobservations = ", dm[i,"sampleSize"], ";")
if (dm[[i,"clusterBal"]]=="bal") {
  ncsizes <- paste("ncsizes = ", 1, ";")
  csize <- paste0(dm[i, "clusterN"], " (", dm[i, "clusterSize"], ") !number (size)")
}
if (dm[[i,"clusterBal"]]=="unbal") {
  ncsizes <- paste("ncsizes = ", 2, ";")
  csize <- paste0((dm[i, "clusterN"]/2), " (", (dm[i, "clusterSize"]-15), ") ",
                  (dm[i, "clusterN"]/2), " (", (dm[i, "clusterSize"]+15), ") !number (size)")
}
 
csizes <- paste("csizes = ", csize, ";")
seed <- paste("seed = ", designMatrix$seed[i], ";")
nrep <- paste("nrep = ", iterations, ";")
rsave <- "repsave = ALL;"
save <- paste("save = ", 
              paste(dm[i,"clusterBal"],
                    dm[i,"clusterSize"], 
                    dm[i,"clusterN"],
                    # dm[i,"modelSpec"],
                    dm[i,"distribution"],
                    sep = "_"),
              "_*.dat;",
              sep = "")

# paste0(designMatrix[i, c("sampleSize",
#                        "clusterBal",
#                        "modelSpec",
#                        "distribution")])


theRest <- "
ANALYSIS:	
	TYPE IS twolevel;
    !ESTIMATOR=BAYES;
    !estimator = WLS;
	!distribution = skewnormal;
"



  # Write data generation input file
  writeLines(c(title,
             sampleSize,
             ncsizes,
             csizes,
             seed,
             nrep,
             rsave,
             save,
             theRest,
             mplusModel),
             fileName1)
  # run it
    MplusAutomation::runModels(directory = dataDir)
  # remove
  file.remove(fileName1)
  file.remove(fileName2)
  # remove the list files
  file.remove(list.files(dataDir, pattern = "_list", full.names = TRUE))
} # end for loop
} # end makeData function




# credit script to Francis L. Huang
# Edited for my purposes
# working paper on mcfa in R with Lavaan
mcfa.input<-function(l1Var, l2Var, dat){
    dat1    <- dat[complete.cases(dat),]
    g       <- dat1[,l2Var]            #grouping
    freq    <- data.frame(table(g))
    dat2    <- dat1[,!names(dat1) %in% c(l1Var, l2Var)] #select all but l1/l2 vars
    G       <- length(table(g))
    n       <- nrow(dat2)
    k       <- ncol(dat2)
    scaling <- (n^2-sum(freq$Freq^2)) / (n*(G-1))
    varn    <- names(dat2)
    ms      <- matrix(0,n,k)
   for (i in 1:k){
      ms[,i]<-ave(dat2[,i],g)
   }   
   cs<-dat2-ms #deviation matrix, centered scores
   colnames(ms)<-colnames(cs)<-varn
   b.cov<-(cov(ms) * (n - 1))/(G-1) #group level cov matrix
   w.cov<-(cov(cs) * (n - 1))/(n-G) #individual level cov matrix
   pb.cov<-(b.cov-w.cov)/scaling #estimate of pure/adjusted between cov matrix
   w.cor<-cov2cor(w.cov) #individual level cor matrix
   b.cor<-cov2cor(b.cov) #group level cor matrix
   pb.cor<-cov2cor(pb.cov) #estimate of pure between cor matrix
   icc<-round(diag(pb.cov)/(diag(w.cov)+diag(pb.cov)),3) #iccs
   return(list(b.cov=b.cov,pw.cov=w.cov,ab.cov=pb.cov,pw.cor=w.cor,
         b.cor=b.cor,ab.cor=pb.cor,
         n=n,G=G,c.=scaling,sqc=sqrt(scaling),
         icc=icc,dfw=n-G,dfb=G) )
}

alpha<-function(dat){
   covar<-dat[lower.tri(dat)] #get unique covariances
   n<-ncol(dat) #number of items in the scale
   al<-((sum(covar)/length(covar))*n^2/sum(dat))
   cat("alpha:",return(al),"\n")   
}

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
  if (tolower(estimator) == "muthen") {
    # decompose muthen style
    muth <- mcfa.input(l1Var, l2Var, dat = df)
    covMats <- list(within = muth$pw.cov, between=muth$ab.cov)
  } else if (tolower(estimator) == "goldstein") {
    # decompose muthen style
    covMats <- decompGoldstein(allIndicators, l1Var, l2Var, df)
  } else {
    print("incorrect estimator entered")
  }

  # -----------------------------------------------------------------
  # fit with MIIVsem
  # Fit covariance matrices with MIIVsem
  w <- MIIVsem::miive(withinModel,  
                      sample.cov = covMats[["within"]], 
                      sample.nobs = n, 
                      var.cov = TRUE)
  b <- MIIVsem::miive(betweenModel,
                      sample.cov = covMats[["between"]],
                      sample.nobs = g,
                      var.cov = TRUE, 
                      overid.degree = 1, 
                      overid.method = "random")
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
      #print(i)
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
  # bCovMat <- ((nMn/(g-1)) * t(d) %*% d)
  bCovMat <- ((g-1) * t(nMn*d) %*% d)
  # names
  colnames(bCovMat)  <- allIndicators
  rownames(bCovMat)  <- allIndicators
  # return
  return(list(within=wCovMat, between=bCovMat)) 

}


simData <- function(indicatorNames,
                    withinModel, 
                    betweenModel, 
                    clusterNo,
                    clusterSize,
                    wSkew = 0,
                    wKurt = 0,
                    bSkew = 0,
                    bKurt = 0,
                    clusterBal = TRUE,
                    seed = sample(1:1000000, 1)) {

inb <- paste0(indicatorNames, "b")
inw <- paste0(indicatorNames, "w")

# Simulate between
dfB <- lavaan::simulateData(model       = betweenModel, 
                    model.type  = 'cfa', 
                    sample.nobs = clusterNo,
                    Skewness    = bSkew,
                    Kurt        = bKurt, 
                    seed        = seed)
# make sure data are in correct order
dfB        <- dfB[, indicatorNames]
# rename with a b
names(dfB) <- inb

# Simulate within
dfW <- lavaan::simulateData(model       = withinModel, 
                    model.type  = 'cfa', 
                    sample.nobs = clusterNo*clusterSize, 
                    Skewness    = wSkew,
                    Kurt        = wKurt, 
                    seed        = (seed+11))
# make sure data are in correct order
dfW        <- dfW[, indicatorNames]
# rename with W
names(dfW) <- inw

# make vector of ids
id <- 1:(clusterNo*clusterSize)
# make vector of clusters
if (clusterBal==TRUE) {
  cluster <- rep(1:clusterNo, clusterSize)
} else {
  cluster <- c(rep(1:(clusterNo/2), clusterSize-15),
               rep(((clusterNo/2)+1):clusterNo, clusterSize+15))
}

df <- cbind(dfW, dfB,
            cluster,
            id)
# make df a dataframe
df <- as.data.frame(df)
# create new vars
for (i in seq(indicatorNames)) {
  df[, indicatorNames[i]] <- df[, inw[i]] + df[, inb[i]]
}
# subset just the vars we want
df <- df[, c("id", "cluster", indicatorNames)]

return(df)
}

simData2 <- function(indicatorNames,
                     wLambda,
                     wPsi,
                     wTheta,
                     bLambda,
                     bPsi,
                     bTheta,
                     clusterNo,
                     clusterSize,
                     wSkew = 0,
                     wKurt = 0,
                     bSkew = 0,
                     bKurt = 0,
                     clusterBal = TRUE,
                     seed = sample(1:1000000, 1)) {
  # save indicator names
  inb <- paste0(indicatorNames, "b")
  inw <- paste0(indicatorNames, "w")
  # create the implied covariance matrices
  wCov <- wLambda%*%wPsi%*%t(wLambda) + wTheta
  bCov <- bLambda%*%bPsi%*%t(bLambda) + bTheta
  
  # Generate the within data
  # if no skew or kurtosis specified for within use mass::mvrnorm
  if (wSkew==0&wKurt==0) {
    dfW <- MASS::mvrnorm(n        = clusterNo*clusterSize, 
                         mu       = rep(0, length(indicatorNames)), 
                         Sigma    = wCov)
  } else { 
    # if skew or kurtosis is in the model, use semTools::mvrnonnorm
    dfW <- semTools::mvrnonnorm(n        = clusterNo*clusterSize, 
                                mu       = rep(0, length(indicatorNames)), 
                                Sigma    = wCov, 
                                skewness = wSkew, 
                                kurtosis = wKurt)
  }
  # generate the between
  if (bSkew==0&bKurt==0) {
    dfB <- MASS::mvrnorm(n        = clusterNo, 
                         mu       = rep(0, length(indicatorNames)), 
                         Sigma    = bCov, 
                         skewness = bSkew, 
                         kurtosis = bKurt) 
  } else {
    dfB <- semTools::mvrnonnorm(n        = clusterNo, 
                                mu       = rep(0, length(indicatorNames)), 
                                Sigma    = bCov, 
                                skewness = bSkew, 
                                kurtosis = bKurt)
  }
  # adding column names
  colnames(dfW) <- inw
  colnames(dfB) <- inb
  # make vector of ids
  id <- 1:(clusterNo*clusterSize)
  # make vector of clusters
  if (clusterBal==TRUE) {
    cluster <- rep(1:clusterNo, clusterSize)
  } else {
    cluster <- c(rep(1:(clusterNo/2), clusterSize-15),
                 rep(((clusterNo/2)+1):clusterNo, clusterSize+15))
  }
  dfw <- as.data.frame(dfW)
  dfB <- as.data.frame(dfB)
  df <- cbind(dfW, dfB,
              cluster,
              id)
  # make df a dataframe
  df <- as.data.frame(df)
  # create new vars
  for (i in seq(indicatorNames)) {
    df[, indicatorNames[i]] <- df[, inw[i]] + df[, inb[i]]
  }
  # subset just the vars we want
  df <- df[, c("id", "cluster", indicatorNames)]
  
  return(df)
}


