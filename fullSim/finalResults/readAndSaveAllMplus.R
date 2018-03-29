rm(list=ls())
baseDir <- "C:/users/mgiordan/git/mlmcfasimulation/fullSim"
setwd(baseDir)
t       <- list.files("./savedModels", pattern = ".out") # list all files
l       <- vector("list", length = length(t))            # preallocate a list
setwd("./savedModels")                                   # .out files are here
start   <- Sys.time()                                    # save time
for (i in 1:length(t)) {                 # loop through and add all
  print(i)                               # print i to keep track of loop
  l[[i]]      <- readr::read_lines(t[i]) # read in the output
  names(l)[i] <- t[i]                    # name the list object from file
}
end <- Sys.time()                      # save end time
end-start                              # print the time it took to read all 
setwd(baseDir)                         # change dir back
# save the .rds file of all out
saveRDS(object = l, file = "finalResults/allMplus.Rds")





