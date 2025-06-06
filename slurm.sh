#!/bin/bash
#SBATCH --job-name=uni_mod
#SBATCH --output=uni%j.out
#SBATCH --error=uni%j.err
#SBATCH --ntasks=16
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32

# start up conda environment
source /home/bsc/bsc141218/start_conda.sh

# activate inla conda environment
conda activate r-inla

# test script
Rscript  lsl_interaction_brazil/mn5/03a_model_fitting_uni.R

# set up script with number of threads requested
#Rscript /home/bsc/bsc032302/dengue_forecasting/e4warning/scripts_sea/03_run_model_univariable.R --country sri_lanka --admin 1 --tstep m --hpc TRUE --threads 8

