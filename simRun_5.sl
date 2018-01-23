#!/bin/bash
#SBATCH --job-name=example
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=20:00
#SBATCH --mem-per-cpu=1024
srun R CMD BATCH --no-save simRun_5.R simRun_5.Rout
