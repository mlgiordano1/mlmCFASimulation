#!/bin/bash
#SBATCH --mem 1024
#SBATCH -n 1
#SBATCH -t 15:00
srun R CMD BATCH --no-save /nas/longleaf/home/mgiordan/practice/sim.R  /nas/longleaf/home/mgiordan/practice/sim.Rout
