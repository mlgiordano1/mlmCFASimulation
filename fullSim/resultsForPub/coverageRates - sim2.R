library("tidyverse")
library("jtools")
baseDir <- "c:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)



# ------------------------------------------
# Reading in original (all) results
# ------------------------------------------
dm <- readRDS("finalResults/allResultsDf.rds")
# remove current unbal bc they are wrong
dm <- subset(dm, dm$clusterBal=='bal')
#read in the corrected unbal
unbal <- readRDS("./unbalanceFix\\finalResults\\unbalancedResultsDf.rds")
# add together
dm <- rbind(dm, unbal)
# convert all to character
dm[, ] <- lapply(dm[, ], as.character)




# --------------------------------------------
# Subsetting conditions we want
# -------------------------------------------
l_sub <- subset(dm, dm$modelSpec=="trueModel")
# l_sub <- subset(l_sub, l_sub$clusterBal=="bal")
# l_sub <- subset(l_sub, l_sub$clusterSize==" 30")
# l_sub <- subset(l_sub, l_sub$clusterN==" 30")




# ----------------------------------------------
setwd("./resultsforpub")
source("processDF_function.R")
df <- processDf(l_sub)


# --------------------------------------------
# plot 1 is just over skew/kurtosis
# --------------------------------------------

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
df$w_or_b <- capwords(df$w_or_b)
df$w_or_b <- as.factor(df$w_or_b)
df$w_or_b <- relevel(df$w_or_b, "Within")
levels(df$w_or_b)

coverages <- df %>% group_by(estimators, cluster, skewKurt, w_or_b) %>%
    summarise(Coverage.Mn = mean(covered, na.rm = TRUE))

coverages[coverages$skewKurt=="0000","newSkew"] <- "No\nSkew/Kurtosis"
coverages[coverages$skewKurt=="2800","newSkew"] <- "Within\nSkew/Kurtosis"
coverages[coverages$skewKurt=="0028","newSkew"] <- "Between\nSkew/Kurtosis"
coverages[coverages$skewKurt=="2828","newSkew"] <- "Within and Btw\nSkew/Kurtosis"
coverages$newSkew <- as.factor(coverages$newSkew)
coverages$newSkew <- relevel(coverages$newSkew, "Within and Btw\nSkew/Kurtosis")
coverages$newSkew <- relevel(coverages$newSkew, "Between\nSkew/Kurtosis")
coverages$newSkew <- relevel(coverages$newSkew, "Within\nSkew/Kurtosis")
coverages$newSkew <- relevel(coverages$newSkew, "No\nSkew/Kurtosis")
# Plot it!
ggplot(coverages, aes(x = reorder(cluster, X = coverages$cluster, descending = FALSE), 
                      y = Coverage.Mn, 
                      color = estimators, 
                      lty   = estimators,
                      group = estimators)) +
  facet_grid(w_or_b~newSkew) +
    geom_point(size = 1.5) +
    geom_line(size = 1) +
    geom_hline(yintercept = .95) +
    # scale_color_grey(label = c("FIML", "MIIV-Goldstein", "MIIV-MUML")) +
    scale_color_grey() +
    scale_y_continuous(breaks = seq(.6, 1, .1), limits = c(.6, 1))+
    # geom_hline(yintercept = .95, color = "red") +
    theme_apa() +
    xlab("Number of Clusters (CN); Size of Clusters (CS)") +
    scale_x_discrete(labels = c("CN 100\nCS100", "CN 100\nCS30", "CN 30\nCN100", "CN 30\nCS30")) +
    ylab("Coverage Rates") +
    ggtitle(label = "Coverage Rates for True Model Specifications") +
    theme(axis.title.x = element_text("label", vjust = -1),
      axis.text.x = element_text(angle = 0),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_blank(),
          legend.position = c(0.01, 0.75),
          legend.background = element_rect(color = "black",
                                           fill = "grey90", size = 1, linetype = "solid"), 
      strip.background = element_blank())+

  ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/CoverageRates_skew.jpg", width = 13, height = 6)

# --------------------------------------------
# plot 2 is over both skew/kurtosis and cluster balance
# --------------------------------------------
coverages <- df %>% group_by(estimators, cluster, skewKurt, clusterBal) %>%
    summarise(Coverage.Mn = mean(covered, na.rm = TRUE))
  
# Plot it!
ggplot(coverages, aes(x = reorder(cluster, X = coverages$cluster, descending = FALSE), 
                        y = Coverage.Mn, 
                        color = estimators, 
                        group = estimators,
                        lty = estimators, 
                        shape = estimators)) +
  facet_grid(clusterBal~skewKurt) +
    geom_point() +
    geom_line() +
  geom_hline(yintercept = .95) +
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
ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/CoverageRates_skew&bal.jpg", width = 15, heigh = 7)


