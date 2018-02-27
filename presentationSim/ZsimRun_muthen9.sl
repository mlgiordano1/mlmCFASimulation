#!/bin/bash
#SBATCH --job-name=muthen
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:10:00
#SBATCH --mem-per-cpu=1024
srun R CMD BATCH --no-save ZsimRun_muthen9.R ZsimRun_muthen9.Rout
