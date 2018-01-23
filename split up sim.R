
setwd("c:/users/mgiordan/git/mlmcfasimulation")
# simulation file - this is the file with you general simulation code
# we are basically only going to replace the parameters of the for loop
simFile <- "sim.R"
totalIter <- 80      # how many iterations does your simulation have
divideBy <- 80       # how many files would you like to split it into
findBy <- "startingPoint" # what is the character sequence


#----------------------------------------------------------------------

nPer <- ceiling(totalIter/divideBy) # the number of iterations per
lines <- readLines(simFile)         # read the primary simulation file. 
lineToReplace <- grep(findBy, lines)

# for slurm files
a1 <- "#!/bin/bash"
a2 <- "#SBATCH --job-name=example"
a3 <- "#SBATCH --ntasks=1"
a4 <- "#SBATCH --cpus-per-task=1"
a5 <- "#SBATCH --ntasks-per-node=1"
a6 <- "#SBATCH --time=20:00"
a7 <- "#SBATCH --mem-per-cpu=1024"
a8 <- "srun R CMD BATCH --no-save "


start1 <- 1
stop1 <- nPer
i <- 1
while (start1 <= totalIter) {
  # replace the line with the one we want
  lines[lineToReplace] <- paste0(
    "for (i in ", start1, ":", stop1, ") {") 
  # print to .R file
  writeLines(lines, paste0("simRun_", i, ".R"))
  # print the sl file
  output.file <- file(paste0("simRun_", i, ".sl"), "wb")
  #con <- file(paste0("simRun_", i, ".sl"), "wb")
  writeLines(c(a1,a2,a3,a4,a5,a6,a7,
             paste0(a8, "simRun_", i, ".R ", "simRun_", i, ".Rout")), 
      sep = "\n",
      con = output.file)
  close( con )
  #printing the commands
  cat(paste0("sbatch simRun_", i, ".sl\n"), file = "commands.txt", append = TRUE )
  print(paste0("Printing R file sequencing from-", start1, ":", stop1))
  # advance numbers
  start1 <- stop1 + 1
  stop1 <- start1+nPer-1
  i = i+1
}



x <- 5
y  = 3











