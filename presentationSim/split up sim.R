setwd("c:/users/mgiordan/git/mlmcfasimulation/presentationsim")
# simulation file - this is the file with you general simulation code
# we are basically only going to replace the parameters of the for loop
simFile <- "FitModels.R"
totalIter <- 4000      # how many iterations does your simulation have
divideBy <- 100       # how many files would you like to split it into
findBy <-  "startingPoint"# what is the character sequence


#----------------------------------------------------------------------
closeAllConnections()
try({file.remove("commands.txt")})
nPer <- ceiling(totalIter/divideBy) # the number of iterations per
lines <- readLines(simFile)         # read the primary simulation file. 
lineToReplace <- grep(findBy, lines)
# decide which estimator
if (any(grepl("Muthen", lines))) {
  est <- "muthen"

}
if (any(grepl("Goldstein", lines))) {
  est <- "goldstein"
}

nam <- paste0("ZsimRun_", est)
# for slurm files
a1 <- "#!/bin/bash"
a2 <- paste0("#SBATCH --job-name=", est)
a3 <- "#SBATCH --ntasks=1"
a4 <- "#SBATCH --cpus-per-task=1"
a5 <- "#SBATCH --ntasks-per-node=1"
a6 <- "#SBATCH --time=05:00:00"     #HH:MM:SS
a7 <- "#SBATCH --mem-per-cpu=1024"
a8 <- "srun R CMD BATCH --no-save "

cat("module load -r\n", file = "commands.txt", append = TRUE )
cat("module list\n", file = "commands.txt", append = TRUE )
start1 <- 1
stop1 <- nPer
i <- 1
while (start1 <= totalIter) {
  # replace the line with the one we want
  lines[lineToReplace] <- paste0(
    "for (i in ", start1, ":", stop1, ") {") 
  # print to .R file
  writeLines(lines, paste0(nam, i, ".R"))
  # print the sl file
  output.file <- file(paste0(nam, i, ".sl"), "wb")
  writeLines(c(a1,a2,a3,a4,a5,a6,a7,
             paste0(a8, nam, i, ".R ", nam, i, ".Rout")), 
      sep = "\n",
      con = output.file)
  close.connection(output.file)
  #printing the commands
  cat(paste0("sbatch ", nam, i, ".sl\n"), file = "commands.txt", append = TRUE )
  print(paste0("Printing R file sequencing from-", start1, ":", stop1))
  # advance numbers
  start1 <- stop1 + 1
  stop1 <- start1+nPer-1
  i = i+1
}
