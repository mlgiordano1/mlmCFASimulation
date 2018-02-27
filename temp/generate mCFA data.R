setwd("c:/users/mgiordan/git/mlmcfasimulation")
source('simulationfunctions.r')


bModel <- '
l1 =~ y1+y2+y3+y4
'
wModel <- '
l1 =~ y1+y2+y3+y4
'

results <- matrix(nrow = 500, ncol = 6)
colnames(results) <- c("y2w", "y3w", "y4w", "y2b", "y3b", "y4b")

for (i in 1:500) {
  print(i)
# generate all random parts
df <- cbind(cluster = 1:100,
            id = 1:10000,
            etaW = rnorm(0,sqrt(2), n = 10000), 
            etaB = rnorm(0,sqrt(.5), n=100), 
            y1EpW = rnorm(mean = 0, sd = sqrt(.8), n = 10000), 
            y1EpB = rnorm(mean = 0, sd = sqrt(.2), n = 100),
            y2EpW = rnorm(mean = 0, sd = sqrt(.8), n = 10000), 
            y2EpB = rnorm(mean = 0, sd = sqrt(.2), n = 100),
            y3EpW = rnorm(mean = 0, sd = sqrt(.8), n = 10000), 
            y3EpB = rnorm(mean = 0, sd = sqrt(.2), n = 100),
            y4EpW = rnorm(mean = 0, sd = sqrt(.8), n = 10000), 
            y4EpB = rnorm(mean = 0, sd = sqrt(.2), n = 100)
            )
df <- as.data.frame(df)

# create the indicators
df[,"y1"] <- ( 1*df[,"etaW"] + df[,"y1EpW"]) + ( 1*df[,"etaB"] + df[, "y1EpB"])
df[,"y2"] <- (.9*df[,"etaW"] + df[,"y2EpW"]) + (.8*df[,"etaB"] + df[, "y2EpB"])
df[,'y3'] <- (.7*df[,"etaW"] + df[,"y3EpW"]) + (.7*df[,"etaB"] + df[, "y3EpB"])
df$y4 <- (.8*df[,"etaW"] + df[,"y4EpW"]) + (.9*df[,"etaB"] + df[, "y4EpB"])
#drop random parts
df <- df[,c("id", "cluster", "y1", "y2", "y3", "y4")]
# fit the model
fit <- mlcfaMIIV(withinModel = wModel, 
          betweenModel = bModel, 
          estimator = "Muthen", 
          allIndicators = c("y1", "y2", "y3", "y4"), 
          l1Var = "id", 
          l2Var = "cluster", 
          df = df)
# save results to the results matrix
results[i, "y2w"] <- fit$within$coefficients["y2~l1"]
results[i, "y3w"] <- fit$within$coefficients["y3~l1"]
results[i, "y4w"] <- fit$within$coefficients["y4~l1"]

results[i, "y2b"] <- fit$between$coefficients["y2~l1"]
results[i, "y3b"] <- fit$between$coefficients["y3~l1"]
results[i, "y4b"] <- fit$between$coefficients["y4~l1"]
}

summarise

mean(results[, "y2w"])
mean(results[, "y3w"])
mean(results[, "y4w"])

mean(results[, "y2b"])
mean(results[, "y3b"])
mean(results[, "y4b"])










