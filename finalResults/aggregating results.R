library(MIIVsem)
library(tidyverse)
library(rlang)
library(stringr)


setwd("c:/users/mgiordan/git/mlmcfasimulation/")
setwd("./savedModels")
allFiles <- list.files()

#make a df
results <- matrix(nrow = length(allFiles), ncol = 15)
colnames(results) <- c('name',
                       'sampleSize',
                       'balanced',
                       'model',
                       'dist',
                       'est',
                       'y2~l1','y3~l1','y5~l1',
                       'y5~l2','y6~l2','y8~l2',
                       'y8~l3','y9~l3','y6~l3')
results<- as.data.frame(results)
# aggregating all the models into one object
for (i in seq(allFiles)) {
  # read in the object
  y <- readRDS(allFiles[i])
  # save the name
  results[i, 'name'] <- allFiles[i]
  # save the conditions
  b <- str_locate_all(allFiles[i], "_")
  results[i, "sampleSize"] <- substr(allFiles[i], 1,              b[[1]][1,2]-1)
  results[i, "balanced"]   <- substr(allFiles[i], b[[1]][1,1]+1,  b[[1]][2,1]-1)
  results[i, "model"]      <- substr(allFiles[i], b[[1]][2,1]+1,  b[[1]][3,1]-1)
  results[i, "dist"]       <- substr(allFiles[i], b[[1]][3,1]+1,  b[[1]][4,1]-1)
  results[i, "est"]        <- substr(allFiles[i], b[[1]][4,1]+1,  b[[1]][4,1]+4)
  # save model parameters 
  results[i, 'y2~l1'] <- as.numeric(y$within$coefficients['y2~l1'])
  results[i, 'y3~l1'] <- y$within$coefficients['y3~l1']
  results[i, 'y5~l1'] <- y$within$coefficients['y5~l1'] #misspec path
  results[i, 'y5~l2'] <- y$within$coefficients['y5~l2']
  results[i, 'y6~l2'] <- y$within$coefficients['y6~l2']
  results[i, 'y8~l2'] <- y$within$coefficients['y8~l2'] #misspec path
  results[i, 'y8~l3'] <- y$within$coefficients['y8~l3']
  results[i, 'y9~l3'] <- y$within$coefficients['y9~l3']
  results[i, 'y6~l3'] <- y$within$coefficients['y6~l3'] #misspec path
  
  #results <- as.data.frame(results, stringsAsFactors=FALSE)
}



aggResults <- expand.grid(c("bal", "unbal"),
                          "normal", 
                          c("misSpec", "trueModel"),
                          c("3000", "6000"),
                          c("Gold", "Muth"))
names(aggResults) <- c("balanced", "dist", "model", "sampleSize", "est")
# aggregating results across the different conditions
for (i in seq(nrow(aggResults))) {
  s <- as.data.frame(results) %>%
        dplyr::filter(sampleSize==aggResults[i,"sampleSize"],
                balanced==aggResults[i,"balanced"],
                #dist == aggResults[i,"norm"],
                model == aggResults[i,"model"],
                est == aggResults[i,"est"]) %>%
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
  aggResults[i,'y2l1mn'] <- s$y2l1mn
  aggResults[i,'y2l1se'] <- s$y2l1se
  aggResults[i,'y3l1mn'] <- s$y3l1mn
  aggResults[i,'y3l1se'] <- s$y3l1se 
  aggResults[i,'y5l2mn'] <- s$y5l2mn
  aggResults[i,'y5l2se'] <- s$y5l2se
  aggResults[i,'y6l2mn'] <- s$y6l2mn
  aggResults[i,'y6l2se'] <- s$y6l2se     
  aggResults[i,'y8l3mn'] <- s$y8l3mn
  aggResults[i,'y8l3se'] <- s$y8l3se
  aggResults[i,'y9l3mn'] <- s$y9l3mn
  aggResults[i,'y9l3se'] <- s$y9l3se
  aggResults[i,'y5l1mn'] <- s$y5l1mn
  aggResults[i,'y5l1se'] <- s$y5l1se
  aggResults[i,'y8l2mn'] <- s$y8l2mn
  aggResults[i,'y8l2se'] <- s$y8l2se
  aggResults[i,'y6l3mn'] <- s$y6l3mn
  aggResults[i,'y6l3se'] <- s$y6l3se
  aggResults[i,'n'] <- s$n
}
  


