#!/bin/bash

# User Details
#SBATCH --mail-user=maximilian.d.rohde@vanderbilt.edu
#SBATCH --mail-type=ALL

# Compute details
#SBATCH --partition=production
#SBATCH --nodes=1
#SBATCH --ntasks=1 
#SBATCH --cpus-per-task=1
#SBATCH --time=5:00:00
#SBATCH --mem=2G

# Name of output file
#SBATCH --output=status_reports/out-%A-%a.out

# Number of jobs in SLURM job array
#SBATCH --array=1-124

# Start time
start=$(date +%s)

# Load software
module load GCC/11.3.0 OpenMPI/4.1.4 R/4.2.1

# Run R script
Rscript --no-save power.R $SLURM_ARRAY_TASK_ID

# End time
end=$(date +%s)

# Print elasped time
duration=$((end - start))
hours=$(echo "$duration / 3600" | bc -l)
echo "Script executed in $hours hours."

