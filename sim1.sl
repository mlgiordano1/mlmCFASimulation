#!/bin/bash
#SBATCH --mem 1024
#SBATCH -n 1
#SBATCH -t 20:00
srun R CMD BATCH --no-save /nas/longleaf/home/mgiordan/practice/sim1.R  /nas/longleaf/home/mgiordan/practice/sim1.Rout
