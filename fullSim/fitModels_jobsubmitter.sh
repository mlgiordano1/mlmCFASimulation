#!/bin/bash

for condition in   1 769 1537 2305 3073 3841 4609 5377 6145 6913 7681 8449 9217 9985 10753 11521 12289 13057 13825 14593 15361 16129 16897 17665 18433 19201 19969 20737 21505 22273 23041 23809 24577 25345 26113 26881 27649 28417 29185 29953 30721 31489 32257 33025 33793 34561 35329 36097 36865 37633 38401 39169 39937 40705 41473 42241 43009 43777 44545 45313 46081 46849 47617 48385 49153 49921 50689 51457 52225 52993 53761 54529 55297 56065 56833 57601 58369 59137 59905 60673 61441 62209 62977 63745 64513 65281 66049 66817 67585 68353 69121 69889 70657 71425 72193 72961 73729 74497 75265 76033
    do
        sbatch --job-name=Gold_${condition} fitModels_job.sbatch $condition
done


#conditions: Resting_vs_Continuous_FaceMotor Resting_vs_Continuous_Motor Continuous_Motor_vs_Continuous_FaceMotor \
#all_continuous_3conds block_visonly_vs_block_facemotor resting_vs_block_motoronly block_motoronly_vs_block_facemotor \
#block_visonly_vs_block_motoronly resting_vs_block_facemotor resting_vs_block_visonly Cont_FM_Chopped_vs_Cont_MO_chopped

#VisualOnly_chopped MotorOnly_chopped FaceMotor_chopped Resting_truncated 7_Continuous_MotorOnly 8_Continuous_FaceMotor
