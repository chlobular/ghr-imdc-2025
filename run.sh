#!/bin/bash
#SBATCH --job-name=REuni
#SBATCH --output=uni%j.out
#SBATCH --error=uni%j.err
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
Rscript /gpfs/scratch/bsc32/bsc498895/sprint2025/R/02_RE_univariate.R
