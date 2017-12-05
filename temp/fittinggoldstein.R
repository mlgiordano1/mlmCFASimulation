
# REAd in the data
# Make the data wide and dummy code the indicators
# fit the models



myData <- read.table(file = "C:/users/mgiordan/git/mlmcfasimulation/temp/tempdata_1.dat")
names(myData) <- c(paste0("y", 1:9), "cluster")
myData$id <- 1:nrow(myData)

library('nlme')
library("reshape2")

long <- melt(myData, id.vars = c("id", "cluster"), variable.name = "item")
longsub <- long[long$item=="y1"|
                long$item=="y2"|
                long$item=="y3"|
                long$item=="y4", ]
fit <- lme(value~-1, random =~ 1|cluster/item, data=longsub)
summary(fit)
