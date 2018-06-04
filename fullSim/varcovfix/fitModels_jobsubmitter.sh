#!/bin/bash

for condition in  1 121 241 361 481 601 721 841 961 1081 1201 1321 1441 1561 1681 1801 1921 2041 2161 2281
    do
        sbatch --job-name=muml_${condition} fitModels_job.sbatch $condition
done



