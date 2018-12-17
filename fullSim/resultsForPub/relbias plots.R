library("jtools")
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
# make it long
df <- gather(l_sub, parameter, est, l1.by.y1:lb.by.y6.sarg.p, factor_key = FALSE)
df$est <- as.numeric(df$est)

# estimate or SE
df$paramEst_OR_SE <- "paramEst"
df[grepl("se",    df$parameter), "paramEst_OR_SE"] <- "se"
df[grepl(".sarg", df$parameter), "paramEst_OR_SE"] <- "sarg"
df[grepl(".p",    df$parameter), "paramEst_OR_SE"] <- "sarg.p"
df$parameter <- gsub(".se", "", df$parameter)
df$parameter <- gsub(".sarg.p", "", df$parameter)
df$parameter <- gsub(".sarg", "", df$parameter)
unique(df$parameter)
# make it wide by est/se/sarg/sarg.p
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

df$modelSpec_fac <- as.factor(df$modelSpec)
try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW3"))
try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW2"))
try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW1"))
try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "trueModel"))

# compute relative bias 
df$p_relBias <- ((df$paramEst - df$true) / df$true)*100

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


ggplot(df[df$parameter %in% c(wl, bl), ], aes(x=parameter, y = p_relBias, fill = estimators)) +
  geom_boxplot(outlier.size = 0.3, fatten = NULL) +
  facet_grid(~modelSpec_fac, scales="free_x", space="free_x", shrink=TRUE, drop=TRUE, 
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
  scale_fill_grey() +
ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/relbias.jpg", width = 14, height = 9)
