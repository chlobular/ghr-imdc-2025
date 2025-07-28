#!/bin/bash
#SBATCH --job-name=mod28
#SBATCH --output=mod28_%j.out
#SBATCH --error=mod28_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --mail-type=all
#SBATCH --mail-user=carles.milagarcia@bsc.es

# load modules
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3 
module load R-bundle-CRAN/2023.12-foss-2023b 

# Run script
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/13_CV3/mod28.R
