#!/bin/bash

for condition in 1 122 243 364 485 606 727 848 969 1090 1211 1332 1453 1574 1695 1816 1937 2058 2179 2300 2421 2542 2663 2784 2905 3026 3147 3268 3389 3510 3631 3752 3873 3994 4115 4236 4357 4478 4599 4720 4841 4962 5083 5204 5325 5446 5567 5688 5809 5930 6051 6172 6293 6414 6535 6656 6777 6898 7019 7140 7261 7382 7503 7624 7745 7866 7987 8108 8229 8350 8471 8592 8713 8834 8955 9076 9197 9318 9439 9560 9681 9802 9923 10044 10165 10286 10407 10528 10649 10770 10891 11012 11133 11254 11375 11496 11617 11738 11859 11980
    do
        sbatch --job-name=Gold_${condition} fitModels_job.sbatch $condition
done



