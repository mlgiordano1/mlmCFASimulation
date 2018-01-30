library(MIIVsem)
library(tidyverse)
library(rlang)
library(stringr)


setwd("c:/users/mgiordan/git/mlmcfasimulation/")
setwd("./savedModels")
allFiles <- list.files()

#make a df
wResults <- matrix(nrow = length(allFiles), ncol = 15)
colnames(wResults) <- c('name',
                       'sampleSize',
                       'balanced',
                       'model',
                       'dist',
                       'est',
                       'y2~l1','y3~l1','y5~l1',
                       'y5~l2','y6~l2','y8~l2',
                       'y8~l3','y9~l3','y6~l3')
wResults<- as.data.frame(wResults)
# aggregating all the models into one object
for (i in seq(allFiles)) {
  # read in the object
  y <- readRDS(allFiles[i])
  # save the name
  wResults[i, 'name'] <- allFiles[i]
  # save the conditions
  b <- str_locate_all(allFiles[i], "_")
  wResults[i, "sampleSize"] <- substr(allFiles[i], 1,              b[[1]][1,2]-1)
  wResults[i, "balanced"]   <- substr(allFiles[i], b[[1]][1,1]+1,  b[[1]][2,1]-1)
  wResults[i, "model"]      <- substr(allFiles[i], b[[1]][2,1]+1,  b[[1]][3,1]-1)
  wResults[i, "dist"]       <- substr(allFiles[i], b[[1]][3,1]+1,  b[[1]][4,1]-1)
  wResults[i, "est"]        <- substr(allFiles[i], b[[1]][4,1]+1,  b[[1]][4,1]+4)
  # save model parameters 
  wResults[i, 'y2~l1'] <- as.numeric(y$within$coefficients['y2~l1'])
  wResults[i, 'y3~l1'] <- y$within$coefficients['y3~l1']
  wResults[i, 'y5~l1'] <- y$within$coefficients['y5~l1'] #misspec path
  wResults[i, 'y5~l2'] <- y$within$coefficients['y5~l2']
  wResults[i, 'y6~l2'] <- y$within$coefficients['y6~l2']
  wResults[i, 'y8~l2'] <- y$within$coefficients['y8~l2'] #misspec path
  wResults[i, 'y8~l3'] <- y$within$coefficients['y8~l3']
  wResults[i, 'y9~l3'] <- y$within$coefficients['y9~l3']
  wResults[i, 'y6~l3'] <- y$within$coefficients['y6~l3'] #misspec path
  
  #wResults <- as.data.frame(wResults, stringsAsFactors=FALSE)
}
aggwResults <- expand.grid(c("bal", "unbal"),
                          "normal", 
                          c("misSpec", "trueModel"),
                          c("3000", "6000"),
                          c("Gold", "Muth"))
names(aggwResults) <- c("balanced", "dist", "model", "sampleSize", "est")
# aggregating wResults across the different conditions
for (i in seq(nrow(aggwResults))) {
  s <- as.data.frame(wResults) %>%
        dplyr::filter(sampleSize==aggwResults[i,"sampleSize"],
                balanced==aggwResults[i,"balanced"],
                #dist == aggwResults[i,"norm"],
                model == aggwResults[i,"model"],
                est == aggwResults[i,"est"]) %>%
        dplyr::summarise(n = nrow(.),
                         y2l1mn = mean(!!sym('y2~l1')), 
                         y2l1se = sd(  !!sym('y2~l1')), 
                         y3l1mn = mean(!!sym('y3~l1')), 
                         y3l1se = sd(  !!sym('y3~l1')), 
                         y5l2mn = mean(!!sym('y5~l2')), 
                         y5l2se = sd(  !!sym('y5~l2')), 
                         y6l2mn = mean(!!sym('y6~l2')), 
                         y6l2se = sd(  !!sym('y6~l2')), 
                         y8l3mn = mean(!!sym('y8~l3')), 
                         y8l3se = sd(  !!sym('y8~l3')), 
                         y9l3mn = mean(!!sym('y9~l3')), 
                         y9l3se = sd(  !!sym('y9~l3')), 
                         #misspec paths
                         y5l1mn = mean(!!sym('y5~l1')), 
                         y5l1se = sd(  !!sym('y5~l1')), 
                         y8l2mn = mean(!!sym('y8~l2')), 
                         y8l2se = sd(  !!sym('y8~l2')), 
                         y6l3mn = mean(!!sym('y6~l3')), 
                         y6l3se = sd(  !!sym('y6~l3'))
        )
  aggwResults[i,'y2l1mn'] <- s$y2l1mn
  aggwResults[i,'y2l1se'] <- s$y2l1se
  aggwResults[i,'y3l1mn'] <- s$y3l1mn
  aggwResults[i,'y3l1se'] <- s$y3l1se 
  aggwResults[i,'y5l2mn'] <- s$y5l2mn
  aggwResults[i,'y5l2se'] <- s$y5l2se
  aggwResults[i,'y6l2mn'] <- s$y6l2mn
  aggwResults[i,'y6l2se'] <- s$y6l2se     
  aggwResults[i,'y8l3mn'] <- s$y8l3mn
  aggwResults[i,'y8l3se'] <- s$y8l3se
  aggwResults[i,'y9l3mn'] <- s$y9l3mn
  aggwResults[i,'y9l3se'] <- s$y9l3se
  aggwResults[i,'y5l1mn'] <- s$y5l1mn
  aggwResults[i,'y5l1se'] <- s$y5l1se
  aggwResults[i,'y8l2mn'] <- s$y8l2mn
  aggwResults[i,'y8l2se'] <- s$y8l2se
  aggwResults[i,'y6l3mn'] <- s$y6l3mn
  aggwResults[i,'y6l3se'] <- s$y6l3se
  aggwResults[i,'n'] <- s$n
}

# subset 6000 balanced
sub <- aggwResults[aggwResults$sampleSize=='6000'&
                  aggwResults$balanced=='bal'  ,]
setwd("../")
setwd("./finalResults")
write.csv(sub, "tableOfwResults.csv")
getwd()

#make a df
bResults <- matrix(nrow = length(allFiles), ncol = 15)

bResults<- as.data.frame(bResults)
# aggregating all the models into one object
for (i in seq(allFiles)) {
  # read in the object
  y <- readRDS(allFiles[i])
  # save the name
  bResults[i, 'name'] <- allFiles[i]
  # save the conditions
  b <- str_locate_all(allFiles[i], "_")
  bResults[i, "sampleSize"] <- substr(allFiles[i], 1,              b[[1]][1,2]-1)
  bResults[i, "balanced"]   <- substr(allFiles[i], b[[1]][1,1]+1,  b[[1]][2,1]-1)
  bResults[i, "model"]      <- substr(allFiles[i], b[[1]][2,1]+1,  b[[1]][3,1]-1)
  bResults[i, "dist"]       <- substr(allFiles[i], b[[1]][3,1]+1,  b[[1]][4,1]-1)
  bResults[i, "est"]        <- substr(allFiles[i], b[[1]][4,1]+1,  b[[1]][4,1]+4)
  # save model parameters 
  bResults[i, 'y2~l1'] <- y$between$coefficients['y2~l1']
  bResults[i, 'y3~l1'] <- y$between$coefficients['y3~l1']
  bResults[i, 'y4~l1'] <- y$between$coefficients['y4~l1'] 
  bResults[i, 'y5~l1'] <- y$between$coefficients['y5~l1']
  bResults[i, 'y6~l1'] <- y$between$coefficients['y6~l1']
  bResults[i, 'y7~l1'] <- y$between$coefficients['y7~l1'] 
  bResults[i, 'y8~l1'] <- y$between$coefficients['y8~l1']
  bResults[i, 'y9~l1'] <- y$between$coefficients['y9~l1']
}

aggbResults <- expand.grid(c("bal", "unbal"),
                          "normal", 
                          c("misSpec", "trueModel"),
                          c("3000", "6000"),
                          c("Gold", "Muth"))
names(aggbResults) <- c("balanced", "dist", "model", "sampleSize", "est")
# aggregating bResults across the different conditions
for (i in seq(nrow(aggbResults))) {
  s <- as.data.frame(bResults) %>%
        dplyr::filter(sampleSize==aggbResults[i,"sampleSize"],
                balanced==aggbResults[i,"balanced"],
                #dist == aggbResults[i,"norm"],
                model == aggbResults[i,"model"],
                est == aggbResults[i,"est"]) %>%
        dplyr::summarise(n = nrow(.),
                         y2l1mn = mean(!!sym('y2~l1')), 
                         y2l1se = sd(  !!sym('y2~l1')), 
                         y3l1mn = mean(!!sym('y3~l1')), 
                         y3l1se = sd(  !!sym('y3~l1')), 
                         y4l1mn = mean(!!sym('y4~l1')), 
                         y4l1se = sd(  !!sym('y4~l1')), 
                         y5l1mn = mean(!!sym('y5~l1')), 
                         y5l1se = sd(  !!sym('y5~l1')), 
                         y6l1mn = mean(!!sym('y6~l1')), 
                         y6l1se = sd(  !!sym('y6~l1')), 
                         y7l1mn = mean(!!sym('y7~l1')), 
                         y7l1se = sd(  !!sym('y7~l1')), 
                         y8l1mn = mean(!!sym('y8~l1')), 
                         y8l1se = sd(  !!sym('y8~l1')), 
                         y9l1mn = mean(!!sym('y9~l1')), 
                         y9l1se = sd(  !!sym('y9~l1'))
        )
  aggbResults[i,'y2l1mn'] <- s$y2l1mn
  aggbResults[i,'y2l1se'] <- s$y2l1se
  aggbResults[i,'y3l1mn'] <- s$y3l1mn
  aggbResults[i,'y3l1se'] <- s$y3l1se 
  aggbResults[i,'y4l1mn'] <- s$y4l1mn
  aggbResults[i,'y4l1se'] <- s$y4l1se
  aggbResults[i,'y5l1mn'] <- s$y5l1mn
  aggbResults[i,'y5l1se'] <- s$y5l1se     
  aggbResults[i,'y6l1mn'] <- s$y6l1mn
  aggbResults[i,'y6l1se'] <- s$y6l1se
  aggbResults[i,'y7l1mn'] <- s$y7l1mn
  aggbResults[i,'y7l1se'] <- s$y7l1se
  aggbResults[i,'y8l1mn'] <- s$y8l1mn
  aggbResults[i,'y8l1se'] <- s$y8l1se
  aggbResults[i,'y9l1mn'] <- s$y9l1mn
  aggbResults[i,'y9l1se'] <- s$y9l1se
  aggbResults[i,'n'] <- s$n
}

# subset 6000 balanced
sub <- aggbResults[aggbResults$sampleSize=='6000'&
                  aggbResults$balanced=='bal'  ,]
setwd("../")
setwd("./finalResults")
write.csv(sub, "tableOfbResults.csv")
getwd()





