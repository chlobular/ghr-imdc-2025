#!/bin/bash
#SBATCH --job-name=biFE
#SBATCH --output=biFE%j.out
#SBATCH --error=biFE%j.err
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --mail-type=all
#SBATCH --mail-user=carles.milagarcia@bsc.es

# load modules
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3 
module load R-bundle-CRAN/2023.12-foss-2023b 

# Run script
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/05_FE_bi.R
