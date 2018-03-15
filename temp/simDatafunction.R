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
