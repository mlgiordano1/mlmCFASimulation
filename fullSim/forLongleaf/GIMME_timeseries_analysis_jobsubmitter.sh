#!/bin/bash

for condition in VisualOnly_chopped MotorOnly_chopped FaceMotor_chopped Resting_truncated 7_Continuous_MotorOnly 8_Continuous_FaceMotor
    do
        sbatch --job-name=GIMME_GIMME_${condition} GIMME_timeseries_analysis_job.sbatch $condition
done


#conditions: Resting_vs_Continuous_FaceMotor Resting_vs_Continuous_Motor Continuous_Motor_vs_Continuous_FaceMotor \
#all_continuous_3conds block_visonly_vs_block_facemotor resting_vs_block_motoronly block_motoronly_vs_block_facemotor \
#block_visonly_vs_block_motoronly resting_vs_block_facemotor resting_vs_block_visonly Cont_FM_Chopped_vs_Cont_MO_chopped

#VisualOnly_chopped MotorOnly_chopped FaceMotor_chopped Resting_truncated 7_Continuous_MotorOnly 8_Continuous_FaceMotor
