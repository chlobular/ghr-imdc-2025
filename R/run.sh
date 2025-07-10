#!/bin/bash
#SBATCH --job-name=biFEslopes
#SBATCH --output=biFEslopes%j.out
#SBATCH --error=biFEslopes%j.err
#SBATCH --ntasks=8
#SBATCH --cpus-per-task=8
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --mail-type=all
#SBATCH --mail-user=carles.milagarcia@bsc.es

# load modules
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3 
module load R-bundle-CRAN/2023.12-foss-2023b 

# Run script
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/06_FE_bi_slopes.R
