# Decisions to be made
# Nlme vs LME?
# fill in matrices, two at a time, or three at a time?
# remove irrelevant rows when not in use?
# play around with different optimizers



library('nlme')
library('lme4')
library("reshape2")
library('dummies')
library('dplyr')

# ------------------------------------------------------------------------------
# Process Data
myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)
# make data long
long <- melt(myData, id.vars = c("id", "cluster"), variable.name = "item")
# dummy code the indicators
for (level in unique(long$item)) {
  long[paste("dummy", level, sep = "_")] <- ifelse(long$item == level, 1, 0)
}
# ------------------------------------------------------------------------------

namesOfIndicators <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")
combinations <- t(combn(namesOfIndicators, m=2))


# fitting with different numbers of indicators 
fit <- lme (value~-1+dummy_y1+dummy_y2+dummy_y3, random =~ -1+dummy_y1+dummy_y2+dummy_y3|cluster/id, data=long )
summary(fit)
fit <- lmer(value~-1+dummy_y1+dummy_y2+dummy_y3+ (-1+dummy_y1+dummy_y2+dummy_y3|cluster/id), data=long )
summary(fit)
VarCorr(fit)

allResults <- list()



startTime <- proc.time()
# go through all the combinations
for (i in seq(nrow(allComb))) {
  # which are the items
  items <- as.vector(allComb[i,])
  # dummy code names
  dummies <- paste("dummy_", items, sep = "")
  # subset just corresponding rows
  temp <- filter(long, item %in% items)
  # subset just the items
  temp <- temp[,c("value", "id", "cluster", dummies)]
  
  #rename the cols
  temp$var1 <- temp[[dummies[1]]]
  temp$var2 <- temp[[dummies[2]]]
  # fitting with just the 3
  
  tryCatch({
    print(i)
    fit <- lme(value~-1+var1+var2, random =~ -1+var1+var2|cluster/id, data=temp )
    mylist[[paste0(allComb[i,], collapse="")]] = VarCorr(fit)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
endTime <- proc.time()

paste("The loop took (in seconds):")
endTime-startTime

#Try this with lme4

# would keeping in the full sample stabilize and not change results?
startTime <- proc.time()
# go through all the combinations
for (i in seq(nrow(allComb))) {
  # which are the items
  items <- as.vector(allComb[i,])
  # dummy code names
  dummies <- paste("dummy_", items, sep = "")
  # subset just corresponding rows
  # temp <- filter(long, item %in% items)
  temp <- long
  # subset just the items
  temp <- temp[,c("value", "id", "cluster", dummies)]
  
  #rename the cols
  temp$var1 <- temp[[dummies[1]]]
  temp$var2 <- temp[[dummies[2]]]
  # fitting with just the 3
  
  tryCatch({
    print(i)
    fit <- lmer(value~-1+var1+var2+ (-1+var1+var2|cluster/id), data=temp )
    allResults[[paste0(allComb[i,], collapse="")]] <- VarCorr(fit)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
endTime <- proc.time()

paste("The loop took (in seconds):")
endTime-startTime



# Step 2 - fill within covariance matrix
allResults["y1y2"]

#make a matrix
within <- matrix(nrow=9, ncol=9, 
                 dimnames = list(c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9"),
                                 c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")))
diag(within) <- 1
# go through results and fill in 
for (i in seq(nrow(allComb))) {
  #save the vars being used as a vector
  vars <- allComb[i,]
  # save the as a combined name as well
  name <- paste(allComb[i,], collapse = "")
  # pulling out the relevant number
  # dim(allResults[[name]])
  # str(allResults[[name]])
  # allResults[[name]]$`id:cluster`[1,2]
  # fill in the lower diag
  within[vars[2], vars[1]] <- allResults[[name]]$`id:cluster`[1,2]
  # fill in the upper diag
  within[vars[1], vars[2]] <- allResults[[name]]$`id:cluster`[1,2]
}

length(allResults)



mylist["y1y2"]

fit <- lmer(value~-1+var1+var2+ (-1+var1+var2|cluster/id), data=temp )
summary(fit)
VarCorr(fit)

?system.time()
allResults[[2]]

str(allResults)


#name items to subset
items <- c("y1", "y2")
# subset just those items
temp <- filter(long, item %in% items)
# dummy code names
items <- paste("dummy_", items, sep = "")

# fitting with just the 3
fit <- lme(value~paste(c("-1", items), collapse = "+"), random =~ -1+dummy_y1+dummy_y2+dummy_y3|cluster/id, data=temp)

# fitting with just the 3
fit <- lme(value~paste(c("-1", items), collapse = "+"), random =~ -1+dummy_y1+dummy_y2|cluster/id, data=temp)
  
# fitting with just the 3
fit <- lme(value~-1+dummy_y1+dummy_y2, random =~ -1+dummy_y1+dummy_y2|cluster/id, data=temp)

# subset within 
mat <- matrix(as.numeric(VarCorr(fit)[3:4, 3:4]), nrow=2)
mat <- rbind(NA, mat)
mat <- cbind(mat, NA)
diag(mat) <- 1
rownames(mat) <- c("y1", "y2", "y3")
colnames(mat) <- c("y1", "y2", "y3")
names(mat) <- c("y1", "y2", "y3")



