source('simulationfunctions.r')

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

for (i in 1:200) {
  print(i)
  s <- sample(x = 1:10000000, size = 1, replace = TRUE)
  #s <- 4465250
  # create data
  df <- simData(indicatorNames = c("y1", "y2", "y3", "y4"), 
                withinModel    = wSimModel, 
                betweenModel   = bSimModel, 
                clusterNo      = 30, 
                clusterSize    = 30, 
                skew           = 3, 
                kurtosis       = 0, 
                clusterBal     = TRUE,
                seed = s)
  # fit the model
  #fit <- tryCatch({
  fit<-   mlcfaMIIV(withinModel = wModel, 
                      betweenModel = bModel, 
                      estimator = "Muthen", 
                      allIndicators = c("y1", "y2", "y3", "y4"), 
                      l1Var = "id", 
                      l2Var = "cluster", 
                      df = df)
  # }, warning = function(e) {
  #   message(e)
  #   print(s)
  #   return("model did not fit properly")
  # }, error = function(e) {
  #   message(e)
  #   print(s)
  #   return("model did not fit properly")
  # })

# save results to the results matrix
try(results[i, "y2w"] <- fit$within$coefficients["y2~l1"])
try(results[i, "y3w"] <- fit$within$coefficients["y3~l1"])
try(results[i, "y4w"] <- fit$within$coefficients["y4~l1"])

try(results[i, "y2b"] <- fit$between$coefficients["y2~l1"])
try(results[i, "y3b"] <- fit$between$coefficients["y3~l1"])
try(results[i, "y4b"] <- fit$between$coefficients["y4~l1"])
  
}

library(MplusAutomation)
example <- mplusObject(TITLE = "here is a title;", 
            ANALYSIS = "type = twolevel;", 
            MODEL = " %within%
            l1 by y1-y4;
            %between%
            l1 by y1-y4;",
            rdata = df)
res <- mplusModeler(example, modelout = "ex.inp", run = 1L)


?mplusModeler


?mplusObject

psych::describe(results)


mean(results[, "y2w"])
sd(results[, "y2w"])
mean(results[, "y3w"])
mean(results[, "y4w"])

mean(results[, "y2b"])
sd(results[, "y2b"])
mean(results[, "y3b"])
mean(results[, "y4b"])