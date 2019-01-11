library("tidyverse")
library("jtools")

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

# subsetting the largest and smallest cluster sizes
l_sub1 <- subset(l_sub, l_sub$clusterSize==" 30"&l_sub$clusterN==" 30")
l_sub2 <- subset(l_sub, l_sub$clusterSize=="100"&l_sub$clusterN=="100")
l_sub <- rbind(l_sub1, l_sub2)

# ----------------------------------------------
setwd("./resultsforpub")
source("processDF_function.R")
df <- processDf(l_sub)

# -------------------------
# additional cleaning
# ------------------------
# drop FIML (FIML doesn't apply to sargans)
df <- df[!df$estimators=="FIML",] 

# specify if equation is misspecified or not
df$misOrNot <- "Not"
df[which(df$modelSpec=="misSpecW1"&df$parameter=="L2 by Y5"),"misOrNot"] <- "misspec"
df[which(df$modelSpec=="misSpecW2"&df$parameter=="L1 by Y2"),"misOrNot"] <- "misspec"
df[which(df$modelSpec=="misSpecW3"&df$parameter=="L1 by Y2"),"misOrNot"] <- "misspec"
df[which(df$modelSpec=="misSpecW3"&df$parameter=="L2 by Y2"),"misOrNot"] <- "misspec"
df[which(df$modelSpec=="misSpecW3"&df$parameter=="L1 by Y3"),"misOrNot"] <- "misspec"

# compute whether sargan rejects or not
df$sarg.reject <- ifelse(df$sarg.p<=.05, 1, 0)




# -------------------------
# 
# ------------------------
r <- df %>%
  group_by(estimators, misOrNot, w_or_b, cluster) %>%
  summarize(count = n(),
            mean = mean(sarg.reject, na.rm = TRUE))

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

  r$w_or_b <- capwords(r$w_or_b)
  r$w_or_b <- as.factor(r$w_or_b)
  r$w_or_b <- relevel(r$w_or_b, "Within")
  levels(r$w_or_b)

r$newVar <- as.factor(paste0(r$misOrNot, r$w_or_b))
r$newVar <- relevel(r$newVar, "misspecWithin")
r$newVar <- relevel(r$newVar, "NotBetween")
r$newVar <- relevel(r$newVar, "NotWithin")

ggplot(r, aes(x = newVar, y = mean, fill = estimators)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  # geom_jitter(width = .3, size = 2) +
  scale_fill_grey(labels = c("MIIV-Goldstein", "MIIV-MUML"))+
  facet_grid(~cluster)+
  coord_cartesian(ylim=c(0, 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  theme_bw()+
  guides(labels = c("one", "two")) +
  geom_hline(yintercept = .05, color = "black", linetype = "dashed")+
  scale_x_discrete(labels = c("Correctly\nSpecified\nEquations,\nWithin",
    "Correctly\nSpecified\nEquations,\nBetween",
    "Incorrectly\nSpecified\nEquations,\nWithin")) +
  ylab("Sargan's Alpha/Proportion reject null")+
    theme(strip.background = element_blank(), 
      axis.title.x = element_blank(), 
      legend.position = c(0.1,.9), 
      legend.title = element_blank())

  ggsave("c:/users/mgiordan/git/mlmcfasimulation/fullSim/resultsForPub/Sim1_sargan.jpg")
  