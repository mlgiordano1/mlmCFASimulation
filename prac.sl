#!/bin/bash
#SBATCH --mem 1024
#SBATCH -n 1
#SBATCH -t 1:00
srun R CMD BATCH --no-save /nas/longleaf/home/mgiordan/practice/prac.R  /nas/longleaf/home/mgiordan/practice/prac.Rout

