library("lavaan")
source('simulationfunctions.r')

install.packages('plot3D')
library("plot3D")
bModel <- '
l1 =~ y1+y2+y3+y4
'
wModel <- '
l1 =~ y1+y2+y3+y4
'

# between simulation model
bSimModel <- '
l1 =~ 1*y1+.8*y2+.7*y3+.9*y4
l1~~.5*l1
y1~~.2*y1
y2~~.2*y2
y3~~.2*y3
y4~~.2*y4
'

wSimModel <- '
l1 =~ 1*y1+.9*y2+.7*y3+.8*y4
l1~~2*l1
y1~~.8*y1
y2~~.8*y2
y3~~.8*y3
y4~~.8*y4
'

results <- matrix(nrow = 500, ncol = 6)
colnames(results) <- c("y2w", "y3w", "y4w", "y2b", "y3b", "y4b")

for (i in 1:500) {
  print(i)
  # create between data
dfB <- simulateData(model = bSimModel, model.type = 'cfa', sample.nobs = 100)
names(dfB) <- paste0(names(dfB), "b")
# check the data by fitting
# fit<- cfa(model = bModel, simulateData(model = bSimModel))
# summary(fit)

# create within Data
dfW <- simulateData(model = wSimModel, model.type = 'cfa', sample.nobs = 10000, skewness = 5)
names(dfW) <- paste0(names(dfW), "w")
df <- cbind(dfW, dfB,
            cluster = 1:100,
            id = 1:10000)
# make df a dataframe
df <- as.data.frame(df)
# create new vars
df$y1 <- df$y1w+df$y1b
df$y2 <- df$y2w+df$y2b
df$y3 <- df$y3w+df$y3b
df$y4 <- df$y4w+df$y4b
# subset just the vars we want
df <- df[, c("id", "cluster", paste0("y", 1:4))]


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


mean(results[, "y2w"])
sd(results[, "y2w"])
mean(results[, "y3w"])
mean(results[, "y4w"])

mean(results[, "y2b"])
sd(results[, "y2b"])
mean(results[, "y3b"])
mean(results[, "y4b"])

