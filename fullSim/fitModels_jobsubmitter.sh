#!/bin/bash

for condition in  1 961 1921 2881 3841 4801 5761 6721 7681 8641 9601 10561 11521 12481 13441 14401 15361 16321 17281 18241 19201 20161 21121 22081 23041 24001 24961 25921 26881 27841 28801 29761 30721 31681 32641 33601 34561 35521 36481 37441 38401 39361 40321 41281 42241 43201 44161 45121 46081 47041 48001 48961 49921 50881 51841 52801 53761 54721 55681 56641 57601 58561 59521 60481 61441 62401 63361 64321 65281 66241 67201 68161 69121 70081 71041 72001 72961 73921 74881 75841 76801 77761 78721 79681 80641 81601 82561 83521 84481 85441 86401 87361 88321 89281 90241 91201 92161 93121 94081 95041
    do
        sbatch --job-name=Muthen_${condition} fitModels_job.sbatch $condition
done


#conditions: Resting_vs_Continuous_FaceMotor Resting_vs_Continuous_Motor Continuous_Motor_vs_Continuous_FaceMotor \
#all_continuous_3conds block_visonly_vs_block_facemotor resting_vs_block_motoronly block_motoronly_vs_block_facemotor \
#block_visonly_vs_block_motoronly resting_vs_block_facemotor resting_vs_block_visonly Cont_FM_Chopped_vs_Cont_MO_chopped

#VisualOnly_chopped MotorOnly_chopped FaceMotor_chopped Resting_truncated 7_Continuous_MotorOnly 8_Continuous_FaceMotor
