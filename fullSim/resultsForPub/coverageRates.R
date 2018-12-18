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


# ----------------------------------------------
setwd("./resultsforpub")
source("processDF_function.R")
df <- processDf(l_sub)
  
  coverages <- df %>% group_by(estimators, cluster, w_or_b) %>%
    summarise(Coverage.Mn = mean(covered, na.rm = TRUE))
  
  # Plot it!
  ggplot(coverages, aes(x = reorder(cluster, X = coverages$cluster, descending = FALSE), 
                        y = Coverage.Mn, 
                        color = estimators, 
                        lty = estimators,
                        group = estimators)) +
    facet_wrap(~w_or_b) +
    geom_point(size = 2.5) +
    geom_line(size  = 1.2) +
    scale_color_grey(start = 0, end = .7) +
    scale_y_continuous(breaks = seq(.6, 1, .1), limits = c(.6, 1))+
    geom_hline(yintercept = .95, color = "black") +
    theme_bw() +
    xlab("Number of Clusters (CN); Size of Clusters (CS)") +
    ylab("Coverage Rates") +
    ggtitle(label = "Coverage Rates for True Model Specifications") +
    scale_x_discrete(labels = c("CN 100;\nCS 100", "CN 100;\nCS 30", "CN 30;\nCS 100", "CN 30;\nCS 30")) +
    theme(axis.title.x = element_text("label", vjust = -1), 
          plot.title = element_text(hjust = .5), 
          legend.position = c(0.93, 0.2), 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", size = 1, linetype = "solid")) +
  ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/CoverageRates.jpg")
