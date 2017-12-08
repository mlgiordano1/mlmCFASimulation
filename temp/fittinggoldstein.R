
# REAd in the data
# Make the data wide and dummy code the indicators
# fit the models

library('nlme')
library('lme4')
library("reshape2")
library('dummies')
library('dplyr')


myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)

# make data long
long <- melt(myData, id.vars = c("id", "cluster"), variable.name = "item")
# dummy code the indicators
for (level in unique(long$item)) {
  long[paste("dummy", level, sep = "_")] <- ifelse(long$item == level, 1, 0)
}

allComb <- t(combn(c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9"), m=2))

allResults <- list()
mylist <- list()

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
    mylist[[paste0(allComb[i,], collapse="")]] = VarCorr(fit)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
endTime <- proc.time()

paste("The loop took (in seconds):")
endTime-startTime

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



