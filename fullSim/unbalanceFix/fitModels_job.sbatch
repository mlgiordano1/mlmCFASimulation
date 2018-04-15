#!/bin/bash
#SBATCH --nodes=1-1
#SBATCH --ntasks=1
#SBATCH --mem=16g
#SBATCH --time=01:00:00
#SBATCH --output="slurm_%x_jobid_%j.out"

# load application modules 

module load r/3.3.1
export OMP_NUM_THREADS=1 #line from Karl Eklund to constrain the number of threads

DATE=$(date +%Y%m%d%H%M%S)

R CMD BATCH --no-save '--args '${1} fitNonRunModels.R Gold_Rout_${1}_${DATE}.Rout
