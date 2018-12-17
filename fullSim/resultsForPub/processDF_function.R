processDf <- function(data) {
  
  # -------------------------------------------------
  # Restructuring
  # -------------------------------------------------
  # make it long
  df <- gather(data, parameter, est, l1.by.y1:lb.by.y6.sarg.p, factor_key = FALSE)
  df$est <- as.numeric(df$est)

  # making it wider by est/se/sarg/sarg.p
  df$paramEst_OR_SE <- "paramEst"
  df[grepl("se",    df$parameter), "paramEst_OR_SE"] <- "se"
  df[grepl(".sarg", df$parameter), "paramEst_OR_SE"] <- "sarg"
  df[grepl(".p",    df$parameter), "paramEst_OR_SE"] <- "sarg.p"
  df$parameter <- gsub(".se", "", df$parameter)
  df$parameter <- gsub(".sarg.p", "", df$parameter)
  df$parameter <- gsub(".sarg", "", df$parameter)
  # make it wide by est/se/sarg/sarg.p
  df <- spread(data = df, key = paramEst_OR_SE, value = est)
  
  
  
  # -------------------------------------------------
  # adding true values
  # -------------------------------------------------
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
  
  
  
  # -------------------------------------------------
  # Cleaning - dropping scaling indicators - renaming loadings
  # -------------------------------------------------
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
  
  
  
  
  # ----------------------------------------------
  # Confidence intervals, coverage, relative bias
  # ----------------------------------------------
  df$l.ci.95 <- df$paramEst - (1.96*df$se)
  df$u.ci.95 <- df$paramEst + (1.96*df$se)
  df$covered <- ifelse((df$true>=df$l.ci.95)&(df$true<=df$u.ci.95), 1, 0)
  # compute relative bias 
  df$p_relBias <- ((df$paramEst - df$true) / df$true)*100
  
  
  
  # ----------------------------------------------
  # Making a single Cluster Variable
  # ----------------------------------------------
  df$clusterN <- as.numeric(df$clusterN)
  df$clusterSize <- as.numeric(df$clusterSize)
  # df$cluster <- as.factor(paste0(df$clusterN,"groups_of_N", df$clusterSize))
  df$cluster <- as.factor(paste0("CN = ",df$clusterN,"; CS = ", df$clusterSize))
  # getting the order right
  try(df$cluster <- relevel(df$cluster, "CN = 30; CS = 30"))
  try(df$cluster <- relevel(df$cluster, "CN = 30; CS = 100"))
  try(df$cluster <- relevel(df$cluster, "CN = 100; CS = 30"))
  try(df$cluster <- relevel(df$cluster, "CN = 100; CS = 100"))
  
  
  
  
  # ----------------------------------------------
  # Making a factor version of model specification
  # ----------------------------------------------
  df$modelSpec_fac <- as.factor(df$modelSpec)
  try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW3"))
  try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW2"))
  try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "misSpecW1"))
  try(df$modelSpec_fac <- relevel(df$modelSpec_fac, "trueModel"))

  return(df)
}


