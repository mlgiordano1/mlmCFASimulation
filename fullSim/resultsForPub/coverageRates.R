library("tidyverse")

baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)

# read in allresults
dm <- readRDS("finalResults/allResultsDf.rds")
# remove current unbal bc they are wrong
dm <- subset(dm, dm$clusterBal=='bal')

#read in the corrected unbal
unbal <- readRDS("./unbalanceFix\\finalResults\\unbalancedResultsDf.rds")
# add together
dm <- rbind(dm, unbal)
# convert all to character
dm[, ] <- lapply(dm[, ], as.character)


# subset what we want
l_sub <- subset(dm, dm$skewKurt=="0000")
l_sub <- subset(l_sub, l_sub$clusterBal=="bal")
l_sub <- subset(l_sub, l_sub$modelSpec=="trueModel")

# l_sub <- subset(l_sub, l_sub$clusterSize==" 30")
# l_sub <- subset(l_sub, l_sub$clusterN==" 30")



df <- l_sub

  # make it long
  df <- gather(df, parameter, est, l1.by.y1:lb.by.y6.sarg.p, factor_key = FALSE)
  df$est <- as.numeric(df$est)
  
  # drop sargans
  df <- df[!grepl("sarg", df$parameter),]
  unique(df$parameter)
  
  # estimate or SE
  df$paramEst_OR_SE <- "paramEst"
  df[grepl("se", df$parameter), "paramEst_OR_SE"] <- "se"
  
  df$parameter <- gsub(".se", "", df$parameter)
  unique(df$parameter)
  
  df <- spread(data = df, key = paramEst_OR_SE, value = est)
  
  # put in true values
  df[df$parameter=="l1.by.y1", "true"] <- 1
  df[df$parameter=="l1.by.y2", "true"] <- .8
  df[df$parameter=="l1.by.y3", "true"] <- .7
  df[df$parameter=="l2.by.y4", "true"] <- 1
  df[df$parameter=="l2.by.y5", "true"] <- .8
  df[df$parameter=="l2.by.y6", "true"] <- .7
  df[df$parameter=="l1.by.y5", "true"] <- .3
  df[df$parameter=="l2.by.y2", "true"] <- .3
  # tru values for between parameters
  df[df$parameter=="lb.by.y2", "true"] <- .7
  df[df$parameter=="lb.by.y3", "true"] <- .6
  df[df$parameter=="lb.by.y4", "true"] <- .8
  df[df$parameter=="lb.by.y5", "true"] <- .7
  df[df$parameter=="lb.by.y6", "true"] <- .8
  # Remove the scaling indicators
  df <- df[which(df$parameter!="l1.by.y1"),]
  df <- df[which(df$parameter!="l2.by.y4"),]
  df <- df[which(df$parameter!="lb.by.y1"),]
  # doing some parameter renaming
  df$parameter <- gsub(".", " ", df$parameter, fixed = TRUE)
  df$parameter <- gsub("l", "L", df$parameter, fixed = TRUE)
  df$parameter <- gsub("y", "Y", df$parameter, fixed = TRUE)
  df$parameter <- gsub("b", "B", df$parameter, fixed = TRUE)
  df$parameter <- gsub("BY", "by", df$parameter, fixed = TRUE)
  
  # making the confidence intervals
  df$l.ci.95 <- df$paramEst - (1.96*df$se)
  df$u.ci.95 <- df$paramEst + (1.96*df$se)
  
  df$covered <- ifelse((df$true>=df$l.ci.95)&(df$true<=df$u.ci.95), 1, 0)
  
  # coverage rates are bad!

  # make a cluster var
  df$clusterN <- as.numeric(df$clusterN)
  df$clusterSize <- as.numeric(df$clusterSize)
  # df$cluster <- as.factor(paste0(df$clusterN,"groups_of_N", df$clusterSize))
  df$cluster <- as.factor(paste0("CN = ",df$clusterN,"; CS = ", df$clusterSize))
  # getting the order right
  try(df$cluster <- relevel(df$cluster, "CN = 30; CS = 30"))
  try(df$cluster <- relevel(df$cluster, "CN = 30; CS = 100"))
  try(df$cluster <- relevel(df$cluster, "CN = 100; CS = 30"))
  try(df$cluster <- relevel(df$cluster, "CN = 100; CS = 100"))

  # df$cluster <- paste(as.numeric(df$cluster), ". ", df$cluster)
  
  coverages <- df %>% group_by(estimators, cluster) %>%
    summarise(Coverage.Mn = mean(covered, na.rm = TRUE))
  
  # Plot it!
  ggplot(coverages, aes(x = reorder(cluster, X = coverages$cluster, descending = FALSE), 
                        y = Coverage.Mn, 
                        color = estimators, 
                        group = estimators,
                        lty = estimators, 
                        shape = estimators)) +
    geom_point() +
    geom_line() +
    scale_color_grey() +
    scale_y_continuous(breaks = seq(.3, 1, .1), limits = c(.3, 1))+
    # geom_hline(yintercept = .95, color = "red") +
    theme_bw() +
    xlab("Number of Clusters (CN); Size of Clusters (CS)") +
    ylab("Coverage Rates") +
    ggtitle(label = "Coverage Rates for True Model Specifications") +
    theme(axis.title.x = element_text("label", vjust = -1), 
          plot.title = element_text(hjust = .5), 
          legend.position = c(0.93, 0.2), 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", size = 1, linetype = "solid"))
  ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/CoverageRates.jpg")
