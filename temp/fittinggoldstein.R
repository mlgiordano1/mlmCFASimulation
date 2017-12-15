# TODO:
# Look below to the section after my goldstein function


# Decisions to keep in mind
# 1. Subset the rows corresponding to only relevant items
# 2. do comparisons pairwise (as opposed to 3 at a time,etc)
#    not sure if there would be much time gain by doing 3,
#    and it appears as if the results are nearly identical
# 3. 



# Decisions to be made
# Nlme vs LME?
# play around with different optimizers
# REML vs ML

install.packages('nlme')


library('nlme')
library('lme4')
#library("reshape2")
#library('dummies')
#library('dplyr')
library('magrittr')

# ------------------------------------------------------------------------------
# Process Data
myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)
# make data long
long <- reshape2::melt(myData, id.vars = c("id", "cluster"), variable.name = "item")
# dummy code the indicators
for (level in unique(long$item)) {
  long[paste("dummy", level, sep = "_")] <- ifelse(long$item == level, 1, 0)
  long[level] <- ifelse(long$item == level, 1, 0)
}
# ------------------------------------------------------------------------------





longDF <- long
i=1
outcome = "value"
allIndicators = c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")

# use longDF, allIndicators, fitWith (nlme, lmer), 
# return a list of two matrices (within and between)
goldstein <- function(longDF, 
                      outcome,
                      fitWith,
                      allIndicators) {
  # Program some checks, like is longDF a DF, fitWith =nlme or lmer, 
  # all indicators is charater, etc.
  
  # we need to fit these piecewise
  # So first save all the combinations
  combinations <- t(combn(allIndicators, m=2))
  # create a list to save all results
  allModels <- list()
  # choose the estimator to use
  if (tolower(fitWith) == 'nlme') {
    print("fitting with NLME")
    # for each combination subset, fit, save model
    for (i in seq(nrow(combinations))) {
      # save the formula objects to be used
      form <- paste(combinations[i,1], combinations[i,2], sep = "+")
      model <- as.formula(paste0(outcome,"~ -1 +", form))
      ranef <- as.formula(paste("~-1 + ", form, "|cluster/id"))
      # fit the model
      # try to speed up with dplyr?
      # need to use a try catch for models that do not converge.
      subset <- dplyr::filter(.data = longDF, item %in% combinations[i,])
      # Try catch incase models do not converge
       tryCatch({
          print(i)
          allModels[[i]] <- nlme::lme(fixed   = model, 
                                random  = ranef, 
                                data    = subset, 
                                method  = "REML",
                                control = lmeControl(opt='optim'))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # fill in the matrices
    }
  } else if (tolower(fitWith) == 'lme4') {
    print("fitting with lme4")
      # for each combination subset, fit, save model
      for (i in seq(nrow(combinations))) {
        # save the formula objects to be used
        form <- paste(combinations[i,1], combinations[i,2], sep = "+")
        model <- as.formula(paste0(outcome,"~0+", form, "+(0+", form, "|cluster/id)"))
        #ranef <- as.formula(paste("~ -1 + ", form, "|cluster/id"))
        # fit the model
        # try to speed up with dplyr?
        # need to use a try catch for models that do not converge.
        dat <- dplyr::filter(longDF, item %in% combinations[i,])
        # subset just the items
        # Try catch incase models do not converge
        tryCatch({
            print(i)
            allModels[[i]] <- lme4::lmer(model, 
                                         data      = dat, 
                                         REML      = TRUE)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        # fill in the matrices
      }
  }
  return(allModels)
}

indicators = c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")

startTime <- proc.time()
temp <- goldstein(longDF = long,
                  outcome = "value",
                  fitWith = 'nlme', 
                  allIndicators = indicators)
endTime <- proc.time()
endTime-startTime

# 86400 seconds in a day
86400/100 #number of reps we could do in a day
25000/(864)

# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
form <- paste("y1", "y2", sep = "+")
model <- as.formula(paste0("value","~0+", form, "+(0+", form, "|cluster/id)"))
dat <- dplyr::filter(long, item %in% c("y1", "y2"))
fit <- nlme::lme(value ~ -1 + y1 + y2, random = ~ -1+y1+y2|cluster/id, data=dat)


# the following is the 'nlme' package function for pulling out the variance covariance matrices
# I am trying to pull it apart and make something similar for more than two levels
# this function uses some matrix which I can't make out what it is yet
# figure out what this matrix is. See if I can reproduce the matrix with the variances (or sd's) + correlations
# if they are the same make my own function for (getVCE) which takes two objects (lme model, and level)
# build this into the overall model
# finally fit one of these models in SAS and make sure the VCE's are the same.

summary(fit)
random.effects(fit)
getVarCov(fit)
intervals(fit)
?getVarCov
fit <- lme4::lmer(model, 
                  data      = dat, 
                  REML      = TRUE)


    sigma <- fit$sigma
    D <- as.matrix(fit$modelStruct$reStruct[[1]]) * sigma^2
    fit$modelStruct$reStruct$id
    
fit$modelStruct$reStruct
fit$dims
fit$coefficients
fit$coefficients$random$id
fit$sigma
fit$apVar
str(fit$apVar)
fit$logLik
fit$numIter
fit$groups
fit$terms
getVarCov
getVarCov.lme
methods("getVarCov")
?methods()
methods('getVarCov', 'lme')
edit(getAnywhere("getVarCov"))
fit$sigma

  
  longDF %>% filter()

getVarCov.lme

fit1 <- lme (value~-1+dummy_y1+dummy_y2, random =~ -1+dummy_y1+dummy_y2|cluster/id, data=long, method = "ML")




temp <- "value~-1+dummy_y1+dummy_y2"
temp_form <- as.formula(temp)
ranef <- as.formula("~-1+dummy_y1+dummy_y2|cluster/id")
fit1 <- lme (temp_form, random = ranef, data=long_sub1, method = "ML")
?lme()

namesOfIndicators <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9")
combinations <- t(combn(namesOfIndicators, m=2))

lme(fixed = )

# choosing to subset of not the data
indicators_subset1 <- c("y1", "y2")
indicators_subset2 <- c("y1", "y3")
indicators_subset3 <- c("y2", "y3")
indicators_subset4 <- c("y1", "y2", "y3")
# filter the data
long_sub1 <- filter(long, item %in% indicators_subset1)
long_sub2 <- filter(long, item %in% indicators_subset2)
long_sub3 <- filter(long, item %in% indicators_subset3)
long_sub4 <- filter(long, item %in% indicators_subset4)
# fit with all data
fit1 <- lme (value~-1+dummy_y1+dummy_y2, random =~ -1+dummy_y1+dummy_y2|cluster/id, data=long, method = "ML")
# fit with subset data
fit2 <- lme (value~-1+dummy_y1+dummy_y2, random =~ -1+dummy_y1+dummy_y2|cluster/id, data=long_sub, method = "ML" )
# check results
summary(fit1)
summary(fit2)


#checking consistency
ctrl <- lmeControl(opt='optim')
fit1 <- lme (value~-1+dummy_y1+dummy_y2, 
             random =~ -1+dummy_y1+dummy_y2|cluster/id, 
             data=long_sub1, 
             method = "ML",
             control = ctrl)
fit2 <- lme (value~-1+dummy_y1+dummy_y3, 
             random =~ -1+dummy_y1+dummy_y3|cluster/id, 
             data=long_sub2, 
             method = "ML",
             control = ctrl)
fit3 <- lme (value~-1+dummy_y2+dummy_y3, 
             random =~ -1+dummy_y2+dummy_y3|cluster/id, 
             data=long_sub3, 
             method = "ML",
             control = ctrl)
fit4 <- lme (value~-1+dummy_y1+dummy_y2+dummy_y3, 
             random =~ -1+dummy_y1+dummy_y2+dummy_y3|cluster/id, 
             data=long_sub4, 
             method = "ML",
             control = ctrl)

summary(fit1)
VarCorr(fit1)
summary(fit2)
VarCorr(fit2)
summary(fit3)
VarCorr(fit3)

VarCorr(fit4)



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



