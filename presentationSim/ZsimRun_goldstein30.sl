#!/bin/bash
#SBATCH --job-name=goldstein
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=1024
srun R CMD BATCH --no-save ZsimRun_goldstein30.R ZsimRun_goldstein30.Rout
