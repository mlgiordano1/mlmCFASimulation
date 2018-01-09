
gen <- function(sampleSize,
                clustSize,
                evenClust = TRUE,
                varWithinLV,
                varWithinIndE,
                varBtwLV,
                varBtwIndE,
                normal=TRUE) {
  # Generate Within latent vars
  # means
  mu <- rep(0,3)
  # make covariance matrix
  Sigma <- matrix((.3*sqrt(varWithinLV)*sqrt(varWithinLV)), # corr times sd of each
                  nrow = 3, 
                  ncol = 3)
  diag(Sigma) <- varWithinLV
  # generate 3 multivariate normal lvs
  wRawVars <- MASS::mvrnorm(n=sampleSize, mu=mu, Sigma=Sigma)
  # ----------------------------------------------------
  # if normal=false, then transform (see link)
  # https://www.r-bloggers.com/easily-generate-correlated-variables-from-any-distribution-without-copulas/
  # ----------------------------------------------------
  # Generate indicators
  y1w <- wRawVars[,1] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y2w <- wRawVars[,1] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y3w <- wRawVars[,1] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y4w <- wRawVars[,2] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y5w <- wRawVars[,2] + .3*wRawVars[,1] + rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y6w <- wRawVars[,2] + .3*wRawVars[,3] + rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y7w <- wRawVars[,3] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y8w <- wRawVars[,3] + .3*wRawVars[,2] + rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  y9w <- wRawVars[,3] +                   rnorm(n=sampleSize,mean = 0, sd=varWithinIndE)
  d_w <- cbind(y1w, y2w, y3w,y4w, y5w, y6w, y7w, y8w, y9w,
               cluster = rep(1:(sampleSize/clustSize), clustSize),
               id = 1:sampleSize) # going to need to edit this
  # generate between LV
  bRawVars <- cbind(l1b = rnorm(n = (sampleSize/clustSize), 
                                mean = 0, 
                                sd = varBtwLV),
                  cluster = 1:(sampleSize/clustSize))
  # -----------------------------------------------------------------------------
  # Generate between indicators
  y1b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y2b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y3b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y4b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y5b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y6b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y7b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y8b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  y9b <- bRawVars[,1] + rnorm(n=(sampleSize/clustSize),mean = 0, sd=varBtwIndE)
  d_b <- cbind(y1b, y2b, y3b, y4b, y5b, y6b, y7b, y8b, y9b, 
               cluster = bRawVars[,2])
  # merge and make final dataf
  d <- merge(d_w, d_b, by = "cluster")
  # make single indicators
  d$y1 <- d$y1w + d$y1b
  d$y2 <- d$y2w + d$y2b
  d$y3 <- d$y3w + d$y3b
  d$y4 <- d$y4w + d$y4b
  d$y5 <- d$y5w + d$y5b
  d$y6 <- d$y6w + d$y6b
  d$y7 <- d$y7w + d$y7b
  d$y8 <- d$y8w + d$y8b
  d$y9 <- d$y9w + d$y9b
  return(d[c("id", "cluster", paste("y", 1:9, sep = ""))])
}
