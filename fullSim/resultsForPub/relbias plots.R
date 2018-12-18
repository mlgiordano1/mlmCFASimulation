library("jtools")
library("grid")
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
l_sub <- subset(l_sub, l_sub$clusterN==" 30"&l_sub$clusterSize==" 30")

# --------------------------------------------------
# Need to make many all the parameter columns long
# -------------------------------------------------
setwd("./resultsforpub")
source("processDF_function.R")
df <- processDf(l_sub)

# -------------------------
# plot it
# -------------------------


wl <-c("L1 by Y1",
       "L1 by Y2",
       "L1 by Y3",
       "L1 by Y5",
       "L2 by Y4",
       "L2 by Y5",
       "L2 by Y6",
       "L2 by Y2")
bl <- c("LB by Y1",
        "LB by Y2",
        "LB by Y3",
        "LB by Y4",
        "LB by Y5",
        "LB by Y6")

spec_names <- c(
  'trueModel'   = "True Model Specification",
  'misSpecW1' = "Misspecification #1",
  'misSpecW2' = "Misspecification #2",
  'misSpecW3' = "Misspecification #3"
)

# plot within
toplot <- df[df$parameter %in% c(wl, bl), ]
toplot <- toplot[grepl("within", toplot$w_or_b),]
pWithin <- ggplot(toplot, aes(x=parameter, y = p_relBias, fill = estimators)) +
  geom_boxplot(outlier.size = 0.3, fatten = NULL) +
  facet_grid(.~modelSpec_fac, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE, 
             labeller = as_labeller(spec_names)) +  
  stat_summary(fun.y = mean, geom="point",colour="black", size=1, 
               position = position_dodge(width = .75)) +
  theme_apa() +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  coord_cartesian(ylim=c(-100, 100)) +
  scale_y_continuous(breaks = c( 0, seq(-90, 90, 20))) +
  # guide_legend(keywidth = .5, keyheight = .5) +       
  theme(axis.text.x=element_text(angle=0,
                                 hjust=.3,
                                 vjust=1, 
                                 color = "black", size = 12),
        legend.position = c(0.06,.93), 
        legend.key.height = unit(x = .17, units = "in"), 
        legend.background = element_rect(color = "black", 
                                         fill = "grey90", size = 1, linetype = "solid")) +
  ylab("Percent Relative Bias") +
  xlab("Within Factor Loadings") +
  scale_x_discrete(labels = c("L1 by Y2" = expression(lambda[21]), 
                              "L1 by Y3" = expression(lambda[31]),
                              "L1 by Y5" = expression(lambda[51]),
                              "L2 by Y2" = expression(lambda[22]),
                              "L2 by Y5" = expression(lambda[52]),
                              "L2 by Y6" = expression(lambda[62])))+
  # scale_fill_discrete(name="Estimator",
  #                     labels=c("FIML", "Goldstein-MIIV", "MUML-MIIV")) +
  scale_fill_grey() 
# pWithin
# ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/relbias.jpg", width = 14, height = 9)

toplot <- df[df$parameter %in% c(wl, bl), ]
toplot <- toplot[grepl("between", toplot$w_or_b),]
pBetween <- ggplot(toplot, aes(x=parameter, y = p_relBias, fill = estimators)) +
  geom_boxplot(outlier.size = 0.3, fatten = NULL) +
  facet_grid(.~modelSpec_fac, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE, 
             labeller = as_labeller(spec_names)) +  
  stat_summary(fun.y = mean, geom="point",colour="black", size=1, 
               position = position_dodge(width = .75)) +
  theme_apa() +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  coord_cartesian(ylim=c(-100, 100)) +
  scale_y_continuous(breaks = c( 0, seq(-90, 90, 20))) +
  # guide_legend(keywidth = .5, keyheight = .5) +       
  theme(axis.text.x=element_text(angle=0,
                                 hjust=.3,
                                 vjust=1, 
                                 color = "black", size = 12),
        legend.position = "none") +
  ylab("Percent Relative Bias") +
  xlab("Between Factor Loadings") +
  scale_x_discrete(labels = c("LB by Y2" = expression(lambda[2*"B"]), 
                              "LB by Y3" = expression(lambda[3*"B"]),
                              "LB by Y4" = expression(lambda[4*"B"]),
                              "LB by Y5" = expression(lambda[5*"B"]),
                              "LB by Y6" = expression(lambda[6*"B"])))+
  # scale_fill_discrete(name="Estimator",
  #                     labels=c("FIML", "Goldstein-MIIV", "MUML-MIIV")) +
  scale_fill_grey() 
# pBetween


# grid.newpage()
# pAll <- grid.draw(rbind(ggplotGrob(pWithin), ggplotGrob(pBetween), size = "last"))
jpeg("relbias.jpg", height = 14, width = 10, units = "in", res = 72)
grid.draw(rbind(ggplotGrob(pWithin), ggplotGrob(pBetween), size = "last"))
dev.off()

# plot 2 changing things
ggplot(df[df$parameter %in% c(wl, bl), ], aes(x=modelSpec, y = p_relBias, fill = estimators)) +
  geom_boxplot(outlier.size = 0.3, fatten = NULL) +
  facet_grid(~parameter, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE, 
             labeller = as_labeller(spec_names)) +  
  stat_summary(fun.y = mean, geom="point",colour="black", size=1, 
               position = position_dodge(width = .75)) +
  theme_apa() +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  coord_cartesian(ylim=c(-100, 100)) +
  scale_y_continuous(breaks = c( 0, seq(-90, 90, 20))) +
  # guide_legend(keywidth = .5, keyheight = .5) +       
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.9),
        legend.position = c(0.06,.93), 
        legend.key.height = unit(x = .17, units = "in"), 
        legend.background = element_rect(color = "black", 
                                         fill = "grey90", size = 1, linetype = "solid")) +
  ylab("Percent Relative Bias") +
  xlab("Factor Loadings") +
  # scale_fill_discrete(name="Estimator",
  #                     labels=c("FIML", "Goldstein-MIIV", "MUML-MIIV")) +
  scale_fill_grey() 


bias <- df %>% group_by(estimators, modelSpec, w_or_b) %>%
  summarise(mean.bias = mean(p_relBias, na.rm = TRUE))
