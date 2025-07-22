#!/bin/bash
#SBATCH --job-name=CV4
#SBATCH --output=CV4_%j.out
#SBATCH --error=CV4_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=100
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --mail-type=all
#SBATCH --mail-user=carles.milagarcia@bsc.es

# load modules
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3 
module load R-bundle-CRAN/2023.12-foss-2023b 

# Run script
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/11_CV/16.R
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/11_CV/18.R
