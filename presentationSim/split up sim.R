setwd("c:/users/mgiordan/git/mlmcfasimulation/presentationsim")
simFile  <- "FitModels.R"
divideBy <- 100       # how many files would you like to split it into
findBy   <-  "startingPoint"# what is the character sequence
time     <- "05:00:00" #HH:MM:SS  # note 200 muthen models can be run in minutes


# simulation file - this is the file with you general simulation code
# we are basically only going to replace the parameters of the for loop
#----------------------------------------------------------------------
# remove this file before moving forward
try({file.remove("commands.txt")})

# read in the designMatrix to determine number of iterations per estimator
dm <- readRDS("simparams.rds")
totalIter <- nrow(dm$designMatrix)/3 # how many iterations does your simulation have
nPer <- ceiling(totalIter/divideBy)  # the number of iterations per
lines <- readLines(simFile)          # read the primary simulation file. 
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
a6 <- paste0("#SBATCH --time=", time) 
a7 <- "#SBATCH --mem-per-cpu=1024"
a8 <- "srun R CMD BATCH --no-save "

cat("module add r\n", file = "commands.txt", append = TRUE )
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
